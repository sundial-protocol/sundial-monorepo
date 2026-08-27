/**
 * Build, sign, and submit a plain payment on Sundial's Midgard L2.
 *
 * There is no published Lucid `Provider` for the Midgard node's HTTP API, and
 * no "transfer" helper in this SDK (the `user-events/*` modules only cover
 * deposit/withdrawal/tx-order — see internal-docs/smart-contracts.md). A
 * plain payment is just an ordinary Cardano transaction spending L2 UTxOs, so
 * this example wires Lucid Evolution's `Provider` interface directly to the
 * node's `GET /utxos` and `POST /submit` endpoints (see internal-docs/api.md)
 * instead of a standard L1 provider like Blockfrost/Kupmios.
 *
 * Run with:
 *   pnpm --dir demo/midgard-sdk exec ts-node examples/send-payment.ts
 *
 * Required environment variables:
 *   NODE_URL              Midgard node base URL (default http://localhost:3000)
 *   SENDER_ADDRESS         addr_test1... address holding the funds to send
 *   SENDER_PRIVATE_KEY     bech32 ed25519 private key (ed25519_sk...) for SENDER_ADDRESS
 *   RECIPIENT_ADDRESS      addr_test1... address to pay
 *   AMOUNT_LOVELACE        amount to send, in lovelace (default 5000000n)
 *
 * To sign with a browser wallet (CIP-30) instead of a raw private key, swap
 * the `lucid.selectWallet.fromPrivateKey(senderPrivateKey)` call below for
 * `lucid.selectWallet.fromAPI(cip30Api)`, where `cip30Api` is what
 * `window.cardano.<wallet>.enable()` resolves to (the `WalletApi` type
 * already declared in sundial-web/global.d.ts). Everything else — sourcing
 * UTxOs and submitting the signed tx — stays the same: it still has to go
 * through this provider, not the wallet's own `getUtxos`/`submitTx`, because
 * a wallet's built-in backend only knows about Cardano L1, not Midgard's L2
 * ledger.
 */
import {
  Address,
  Assets,
  Credential,
  CML,
  coreToUtxo,
  Lucid,
  Network,
  PROTOCOL_PARAMETERS_DEFAULT,
  Provider,
  ProtocolParameters,
  Transaction,
  TxHash,
  UTxO,
} from "@lucid-evolution/lucid";

const NODE_URL = process.env.NODE_URL ?? "http://localhost:3000";
const NETWORK: Network = "Preprod";

// Cardano min-fee defaults the node's mempool validation actually checks
// against (demo/midgard-node/src/database/mempool.ts, defaultPhaseAConfig) —
// a transaction built with any other minFeeA/minFeeB risks being rejected as
// underpaying its fee.
const NODE_PROTOCOL_PARAMETERS: ProtocolParameters = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  minFeeA: 44,
  minFeeB: 155381,
};

/**
 * A minimal Lucid Evolution `Provider` backed by the Midgard node's HTTP
 * API. Only the methods a plain payment actually needs are implemented;
 * the rest intentionally throw so a future caller doesn't silently get
 * wrong behavior (e.g. delegation/datum lookups the node doesn't serve).
 */
class MidgardNodeProvider implements Provider {
  constructor(private readonly baseUrl: string) {}

  async getProtocolParameters(): Promise<ProtocolParameters> {
    return NODE_PROTOCOL_PARAMETERS;
  }

  async getUtxos(addressOrCredential: Address | Credential): Promise<UTxO[]> {
    if (typeof addressOrCredential !== "string") {
      throw new Error(
        "MidgardNodeProvider.getUtxos only supports querying by address, not by credential",
      );
    }
    const res = await fetch(
      `${this.baseUrl}/utxos?address=${encodeURIComponent(addressOrCredential)}`,
    );
    if (!res.ok) {
      throw new Error(`GET /utxos failed: ${res.status} ${await res.text()}`);
    }
    const { utxos } = (await res.json()) as {
      utxos: { outref: string; value: string }[];
    };
    return utxos.map(({ outref, value }) =>
      coreToUtxo(
        CML.TransactionUnspentOutput.new(
          CML.TransactionInput.from_cbor_hex(outref),
          CML.TransactionOutput.from_cbor_hex(value),
        ),
      ),
    );
  }

  async submitTx(tx: Transaction): Promise<TxHash> {
    const res = await fetch(`${this.baseUrl}/submit`, {
      method: "POST",
      headers: { "Content-Type": "text/plain" },
      body: tx,
    });
    if (!res.ok) {
      throw new Error(`POST /submit failed: ${res.status} ${await res.text()}`);
    }
    // The node's success response only carries a Redis stream entry id, not
    // the tx hash (see internal-docs/api.md#3-submit-an-l2-transaction), so
    // compute the hash from the CBOR we just submitted.
    const body = CML.Transaction.from_cbor_hex(tx).body();
    return CML.hash_transaction(body).to_hex();
  }

  getUtxosWithUnit(): Promise<UTxO[]> {
    throw new Error("MidgardNodeProvider: getUtxosWithUnit is not implemented");
  }
  getUtxoByUnit(): Promise<UTxO> {
    throw new Error("MidgardNodeProvider: getUtxoByUnit is not implemented");
  }
  getUtxosByOutRef(): Promise<UTxO[]> {
    throw new Error("MidgardNodeProvider: getUtxosByOutRef is not implemented");
  }
  getDelegation(): Promise<never> {
    throw new Error("MidgardNodeProvider: getDelegation is not implemented");
  }
  getDatum(): Promise<never> {
    throw new Error("MidgardNodeProvider: getDatum is not implemented");
  }
  awaitTx(): Promise<boolean> {
    throw new Error(
      "MidgardNodeProvider: awaitTx is not implemented — poll GET /tx?tx_hash=... instead",
    );
  }
  evaluateTx(): Promise<never> {
    throw new Error("MidgardNodeProvider: evaluateTx is not implemented");
  }
}

async function main() {
  const senderAddress = requireEnv("SENDER_ADDRESS");
  const senderPrivateKey = requireEnv("SENDER_PRIVATE_KEY");
  const recipientAddress = requireEnv("RECIPIENT_ADDRESS");
  const amountLovelace = BigInt(process.env.AMOUNT_LOVELACE ?? "5000000");

  const provider = new MidgardNodeProvider(NODE_URL);
  const lucid = await Lucid(provider, NETWORK);

  // Deriving the address from the private key here would need a few more
  // CML calls (public key -> credential -> address); passing the address
  // explicitly keeps this example focused on the Midgard-specific parts.
  lucid.selectWallet.fromPrivateKey(senderPrivateKey);

  const assets: Assets = { lovelace: amountLovelace };

  const tx = await lucid
    .newTx()
    .pay.ToAddress(recipientAddress, assets)
    .complete();
  const signed = await tx.sign.withPrivateKey(senderPrivateKey).complete();
  const txHash = await signed.submit();

  console.log(
    `Sent ${amountLovelace} lovelace from ${senderAddress} to ${recipientAddress}`,
  );
  console.log(`Transaction hash: ${txHash}`);
}

function requireEnv(name: string): string {
  const value = process.env[name];
  if (!value) {
    throw new Error(`Missing required environment variable: ${name}`);
  }
  return value;
}

main().catch((error) => {
  console.error(error);
  process.exit(1);
});
