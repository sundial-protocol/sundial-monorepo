import { randomUUID } from "node:crypto";
import { Data, Effect, Option } from "effect";
import { SqlClient } from "@effect/sql";
import { CML, getAddressDetails, walletFromSeed } from "@lucid-evolution/lucid";
import { Database } from "@/services/database.js";
import { NodeConfig } from "@/services/config.js";
import {
  FaucetClaimsDB,
  MempoolDB,
  MempoolLedgerDB,
} from "@/database/index.js";
import * as Ledger from "@/database/utils/ledger.js";
import { breakDownTx } from "@/utils.js";
import type { TxIngressMessage } from "@/services/tx-ingress-queue.js";
import {
  cmlToMidgard,
  encodeTransaction,
} from "../../../midgard-ts/src/index.js";

// Minimum-fee parameters; these mirror `defaultPhaseAConfig` in
// `database/mempool.ts` so the fee we attach clears Phase A's R11 check.
const MIN_FEE_A = 44n;
const MIN_FEE_B = 155_381n;
// Small cushion above the computed minimum so rounding/length jitter between
// the build and the validator can never push us below the floor.
const FEE_MARGIN = 5_000n;
const MAX_FEE_ITERATIONS = 4;

// Serializes faucet payouts across concurrent requests (and future multi-node
// deployments) so two claims never select the same faucet UTxO. Distinct from
// the mempool tx-acceptance lock in `database/mempool.ts`.
const FAUCET_CLAIM_ADVISORY_LOCK_KEY = 1_348_021_589;

// IP rate-limit window: successful claims per hashed IP per rolling 24 hours.
const IP_RATE_LIMIT_WINDOW_MS = 24 * 60 * 60 * 1000;

export type FaucetClaimCode =
  | "DISABLED"
  | "ADDRESS_INVALID"
  | "ADDRESS_NETWORK_MISMATCH"
  | "ADDRESS_NO_PAYMENT_CREDENTIAL"
  | "ADDRESS_SCRIPT"
  | "COOLDOWN"
  | "IP_LIMIT"
  | "DEPLETED"
  | "VALIDATION_FAILED"
  | "INTERNAL";

export class FaucetClaimError extends Data.TaggedError("FaucetClaimError")<{
  readonly code: FaucetClaimCode;
  readonly message: string;
  readonly nextEligibleAt?: Date;
  readonly cause?: unknown;
}> {}

export type FaucetClaimInput = {
  readonly address: string;
  readonly idempotencyKey: string;
  readonly ipHash: string;
};

export type FaucetClaimResult = {
  readonly claimId: string;
  readonly txHash: string;
  readonly amount: bigint;
  readonly nextEligibleAt: Date;
  readonly idempotentReplay: boolean;
};

const internalError = (message: string, cause?: unknown): FaucetClaimError =>
  new FaucetClaimError({ code: "INTERNAL", message, cause });

// ---------------------------------------------------------------------------
// Address validation
// ---------------------------------------------------------------------------

type ValidatedAddress = {
  readonly bech32: string;
  readonly cmlAddress: CML.Address;
};

/**
 * Normalizes and validates a recipient payment address: it must be a testnet
 * address (matching the node's network), carry a payment credential, and not
 * be script-controlled.
 */
const validateRecipientAddress = (
  address: string,
  expectedNetworkId: number,
): Effect.Effect<ValidatedAddress, FaucetClaimError> =>
  Effect.gen(function* () {
    const details = yield* Effect.try({
      try: () => getAddressDetails(address.trim()),
      catch: (e) =>
        new FaucetClaimError({
          code: "ADDRESS_INVALID",
          message: "Address is not a valid bech32 Cardano address",
          cause: e,
        }),
    });

    if (details.paymentCredential === undefined) {
      return yield* Effect.fail(
        new FaucetClaimError({
          code: "ADDRESS_NO_PAYMENT_CREDENTIAL",
          message:
            "Address has no payment credential (reward-only addresses cannot receive funds)",
        }),
      );
    }
    if (details.networkId !== expectedNetworkId) {
      return yield* Effect.fail(
        new FaucetClaimError({
          code: "ADDRESS_NETWORK_MISMATCH",
          message: `Address is for network id ${details.networkId}; expected ${expectedNetworkId} (testnet)`,
        }),
      );
    }
    if (details.paymentCredential.type === "Script") {
      return yield* Effect.fail(
        new FaucetClaimError({
          code: "ADDRESS_SCRIPT",
          message: "Script payment addresses are not supported by the faucet",
        }),
      );
    }

    const cmlAddress = yield* Effect.try({
      try: () => CML.Address.from_bech32(details.address.bech32),
      catch: (e) =>
        new FaucetClaimError({
          code: "ADDRESS_INVALID",
          message: "Failed to decode recipient address",
          cause: e,
        }),
    });

    return { bech32: details.address.bech32, cmlAddress };
  });

// ---------------------------------------------------------------------------
// Faucet UTxO selection
// ---------------------------------------------------------------------------

type FaucetUtxo = {
  readonly inputCbor: Buffer;
  readonly coin: bigint;
};

type FaucetUtxoSelection = {
  readonly selected: FaucetUtxo;
  readonly totalBalance: bigint;
};

// Decodes pure-lovelace faucet UTxOs, returns the largest plus the total
// balance. UTxOs that carry native assets are ignored: the faucet only ever
// produces lovelace-only change, so this should never happen, but skipping
// them keeps value preservation trivially correct.
const selectFaucetUtxo = (
  entries: readonly Ledger.Entry[],
): Effect.Effect<FaucetUtxoSelection, FaucetClaimError> =>
  Effect.try({
    try: (): FaucetUtxoSelection => {
      let totalBalance = 0n;
      let selected: FaucetUtxo | undefined;
      for (const entry of entries) {
        const output = CML.TransactionOutput.from_cbor_bytes(
          entry[Ledger.Columns.OUTPUT],
        );
        const amount = output.amount();
        if (amount.has_multiassets()) {
          continue;
        }
        const coin = amount.coin();
        totalBalance += coin;
        if (selected === undefined || coin > selected.coin) {
          selected = {
            inputCbor: Buffer.from(entry[Ledger.Columns.OUTREF]),
            coin,
          };
        }
      }
      if (selected === undefined) {
        throw new Error("no spendable lovelace-only faucet UTxO");
      }
      return { selected, totalBalance };
    },
    catch: (e) =>
      new FaucetClaimError({
        code: "DEPLETED",
        message: "Faucet has no spendable UTxO",
        cause: e,
      }),
  });

// ---------------------------------------------------------------------------
// Transaction building & signing
// ---------------------------------------------------------------------------

type SignedFaucetTx = {
  readonly txBytes: Uint8Array;
  readonly fee: bigint;
  readonly change: bigint;
};

const conwayLovelaceOutput = (
  address: CML.Address,
  lovelace: bigint,
): CML.TransactionOutput =>
  CML.TransactionOutput.new_conway_format_tx_out(
    CML.ConwayFormatTxOut.new(address, CML.Value.from_coin(lovelace)),
  );

/**
 * Builds a signed L2 transfer spending `selected` and paying `amount` to
 * `recipient`, with the remainder returned to the faucet address. The fee is
 * solved iteratively so it clears the Phase A minimum fee, and change is
 * derived from it so value is exactly preserved (Phase B R12).
 */
const buildAndSignFaucetTx = (
  selected: FaucetUtxo,
  recipient: CML.Address,
  faucetAddress: CML.Address,
  amount: bigint,
  signingKey: CML.PrivateKey,
  publicKey: CML.PublicKey,
  networkId: number,
): Effect.Effect<SignedFaucetTx, FaucetClaimError> =>
  Effect.try({
    try: (): SignedFaucetTx => {
      const input = CML.TransactionInput.from_cbor_bytes(selected.inputCbor);

      let fee = MIN_FEE_B + FEE_MARGIN;
      for (let iteration = 0; iteration < MAX_FEE_ITERATIONS; iteration++) {
        const change = selected.coin - amount - fee;
        if (change < 0n) {
          throw new FaucetClaimError({
            code: "DEPLETED",
            message:
              "Faucet UTxO cannot cover the grant amount plus transaction fee",
          });
        }

        const inputs = CML.TransactionInputList.new();
        inputs.add(input);

        const outputs = CML.TransactionOutputList.new();
        outputs.add(conwayLovelaceOutput(recipient, amount));
        if (change > 0n) {
          outputs.add(conwayLovelaceOutput(faucetAddress, change));
        }

        const body = CML.TransactionBody.new(inputs, outputs, fee);
        body.set_network_id(CML.NetworkId.new(BigInt(networkId)));

        const bodyHash = CML.hash_transaction(body);
        const witnessSet = CML.TransactionWitnessSet.new();
        const vkeyWitnesses = CML.VkeywitnessList.new();
        vkeyWitnesses.add(
          CML.Vkeywitness.new(
            publicKey,
            signingKey.sign(bodyHash.to_raw_bytes()),
          ),
        );
        witnessSet.set_vkeywitnesses(vkeyWitnesses);

        const tx = CML.Transaction.new(body, witnessSet, true);

        const midgardTx = cmlToMidgard(
          tx as unknown as Parameters<typeof cmlToMidgard>[0],
        );
        const encodedLength = BigInt(encodeTransaction(midgardTx).length);
        const requiredFee = MIN_FEE_A * encodedLength + MIN_FEE_B;

        if (fee >= requiredFee) {
          return { txBytes: tx.to_cbor_bytes(), fee, change };
        }
        fee = requiredFee + FEE_MARGIN;
      }
      throw new Error("fee did not converge");
    },
    catch: (e) =>
      e instanceof FaucetClaimError
        ? e
        : new FaucetClaimError({
            code: "INTERNAL",
            message: "Failed to build and sign faucet transaction",
            cause: e,
          }),
  });

// ---------------------------------------------------------------------------
// Claim processing
// ---------------------------------------------------------------------------

const recordToResult = (
  record: FaucetClaimsDB.ClaimRecord,
  idempotentReplay: boolean,
): FaucetClaimResult => ({
  claimId: record.claimId,
  txHash: record.txHashHex,
  amount: record.amountLovelace,
  nextEligibleAt: record.nextEligibleAt,
  idempotentReplay,
});

/**
 * Processes a single faucet claim: enforces idempotency, per-address cooldown
 * and per-IP limits, builds/signs/validates a genesis-funded L2 transfer, and
 * atomically inserts the transaction and the claim record under a serializing
 * advisory lock.
 */
export const processClaim = (
  input: FaucetClaimInput,
): Effect.Effect<FaucetClaimResult, FaucetClaimError, Database | NodeConfig> =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;

    if (
      !config.FAUCET_ENABLED ||
      config.FAUCET_ADDRESS === "" ||
      config.FAUCET_SEED_PHRASE.trim() === ""
    ) {
      return yield* Effect.fail(
        new FaucetClaimError({
          code: "DISABLED",
          message: "Faucet is not enabled on this node",
        }),
      );
    }

    const networkId = config.NETWORK === "Mainnet" ? 1 : 0;
    const recipient = yield* validateRecipientAddress(input.address, networkId);

    const faucetWallet = yield* Effect.try({
      try: () =>
        walletFromSeed(config.FAUCET_SEED_PHRASE, {
          network: config.NETWORK,
        }),
      catch: (e) => internalError("Failed to derive faucet wallet", e),
    });
    const signingKey = yield* Effect.try({
      try: () => CML.PrivateKey.from_bech32(faucetWallet.paymentKey),
      catch: (e) => internalError("Failed to decode faucet signing key", e),
    });
    const publicKey = signingKey.to_public();
    const faucetCmlAddress = yield* Effect.try({
      try: () => CML.Address.from_bech32(config.FAUCET_ADDRESS),
      catch: (e) => internalError("Failed to decode faucet address", e),
    });

    const sql = yield* SqlClient.SqlClient;

    return yield* sql.withTransaction(
      Effect.gen(function* () {
        // Serialize all faucet payouts so cooldown checks and UTxO selection
        // are race-free across concurrent requests.
        yield* sql`SELECT pg_advisory_xact_lock(${FAUCET_CLAIM_ADVISORY_LOCK_KEY})`;

        // Idempotency: a retried request returns the original claim without
        // paying out again.
        const existing = yield* FaucetClaimsDB.findByIdempotencyKey(
          input.idempotencyKey,
        );
        if (Option.isSome(existing)) {
          return recordToResult(existing.value, true);
        }

        const now = new Date();

        const activeCooldown =
          yield* FaucetClaimsDB.findActiveCooldownByAddress(
            recipient.bech32,
            now,
          );
        if (Option.isSome(activeCooldown)) {
          return yield* Effect.fail(
            new FaucetClaimError({
              code: "COOLDOWN",
              message: "Address is in faucet cooldown",
              nextEligibleAt: activeCooldown.value.nextEligibleAt,
            }),
          );
        }

        const ipWindowStart = new Date(now.getTime() - IP_RATE_LIMIT_WINDOW_MS);
        const ipCount = yield* FaucetClaimsDB.countByIpSince(
          input.ipHash,
          ipWindowStart,
        );
        if (ipCount >= config.FAUCET_DAILY_IP_LIMIT) {
          return yield* Effect.fail(
            new FaucetClaimError({
              code: "IP_LIMIT",
              message: "Daily faucet limit reached for this IP",
            }),
          );
        }

        const faucetEntries = yield* MempoolLedgerDB.retrieveByAddress(
          config.FAUCET_ADDRESS,
        );
        const { selected, totalBalance } =
          yield* selectFaucetUtxo(faucetEntries);

        if (totalBalance < config.FAUCET_MIN_BALANCE_LOVELACE) {
          return yield* Effect.fail(
            new FaucetClaimError({
              code: "DEPLETED",
              message: "Faucet balance is below the configured minimum",
            }),
          );
        }

        const amount = config.FAUCET_AMOUNT_LOVELACE;
        const signed = yield* buildAndSignFaucetTx(
          selected,
          recipient.cmlAddress,
          faucetCmlAddress,
          amount,
          signingKey,
          publicKey,
          networkId,
        );

        const processedTx = yield* breakDownTx(signed.txBytes);
        const claimId = randomUUID();
        const message: TxIngressMessage = {
          id: claimId,
          txCbor: Buffer.from(signed.txBytes).toString("hex"),
          deliveryCount: 0,
        };

        const acceptance = yield* MempoolDB.validateAndInsertMultiple([
          { arrivalSeq: 0n, message, processedTx },
        ]);

        if (acceptance.insertedCount !== 1) {
          const reason =
            acceptance.rejected.length > 0
              ? acceptance.rejected[0].reason
              : "transaction was not accepted into the mempool";
          return yield* Effect.fail(
            new FaucetClaimError({
              code: "VALIDATION_FAILED",
              message: `Faucet transaction rejected: ${reason}`,
            }),
          );
        }

        const nextEligibleAt = new Date(
          now.getTime() + config.FAUCET_COOLDOWN_SECONDS * 1000,
        );
        yield* FaucetClaimsDB.insertClaim({
          [FaucetClaimsDB.Columns.CLAIM_ID]: claimId,
          [FaucetClaimsDB.Columns.IDEMPOTENCY_KEY]: input.idempotencyKey,
          [FaucetClaimsDB.Columns.ADDRESS]: recipient.bech32,
          [FaucetClaimsDB.Columns.IP_HASH]: input.ipHash,
          [FaucetClaimsDB.Columns.AMOUNT_LOVELACE]: amount,
          [FaucetClaimsDB.Columns.TX_ID]: processedTx.txId,
          [FaucetClaimsDB.Columns.NEXT_ELIGIBLE_AT]: nextEligibleAt,
        });

        return {
          claimId,
          txHash: processedTx.txId.toString("hex"),
          amount,
          nextEligibleAt,
          idempotentReplay: false,
        };
      }),
    );
  }).pipe(
    Effect.catchAll((e) =>
      e instanceof FaucetClaimError
        ? Effect.fail(e)
        : Effect.fail(
            internalError("Unexpected error while processing faucet claim", e),
          ),
    ),
  );
