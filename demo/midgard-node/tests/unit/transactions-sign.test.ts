import { describe, expect, vi } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";

const retrieveTxHashesByHeaderHashMock = vi.hoisted(() => vi.fn());
const retrieveTxCborsByHashesMock = vi.hoisted(() => vi.fn());
const headerHashFromStateQueueUTxOMock = vi.hoisted(() => vi.fn());

vi.mock("@/database/index.js", () => ({
  BlocksTxsDB: {
    retrieveTxHashesByHeaderHash: retrieveTxHashesByHeaderHashMock,
  },
  ImmutableDB: {
    retrieveTxCborsByHashes: retrieveTxCborsByHashesMock,
  },
}));

vi.mock("@al-ft/midgard-sdk", () => ({
  headerHashFromStateQueueUTxO: headerHashFromStateQueueUTxOMock,
  GENESIS_HEADER_HASH:
    "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
  DataCoercionError: class DataCoercionError extends Error {
    _tag = "DataCoercionError";
    constructor(fields: { message: string; cause?: unknown }) {
      super(fields.message);
    }
  },
  GenericErrorFields: {},
}));

import {
  handleSignSubmitNoConfirmation,
  TxSignError,
  TxSubmitError,
  TxConfirmError,
  GenesisDepositError,
  fetchFirstBlockTxs,
} from "@/transactions/utils.js";
import { createMockSqlHarness } from "./harness/mock-sql-layer.js";

const mockTxHash = "aa".repeat(32);

const mockSignedTx = {
  toCBOR: () => "mock-cbor-hex",
  submitProgram: () => Effect.succeed(undefined),
};

const mockSignBuilder: any = {
  toHash: () => mockTxHash,
  sign: {
    withWallet: () => ({
      completeProgram: () => Effect.succeed(mockSignedTx),
    }),
  },
};

const mockLucid: any = {
  wallet: () => ({
    address: () => Promise.resolve("addr_test1mock"),
  }),
};

const mockDbLayer = createMockSqlHarness().layer;

// ---------------------------------------------------------------------------
// handleSignSubmitNoConfirmation
// ---------------------------------------------------------------------------

describe("handleSignSubmitNoConfirmation returns the tx hash on success", () => {
  it.effect(
    "handleSignSubmitNoConfirmation returns the tx hash on success",
    () =>
      Effect.gen(function* () {
        const result = yield* handleSignSubmitNoConfirmation(
          mockLucid,
          mockSignBuilder,
        );
        expect(result).toBe(mockTxHash);
      }),
  );
});

describe("handleSignSubmitNoConfirmation fails with TxSignError on sign failure", () => {
  it.effect(
    "handleSignSubmitNoConfirmation fails with TxSignError on sign failure",
    () => {
      const failingBuilder: any = {
        toHash: () => mockTxHash,
        sign: {
          withWallet: () => ({
            completeProgram: () =>
              Effect.fail(
                new TxSignError({
                  message: "sign failed",
                  cause: undefined,
                  txHash: mockTxHash,
                }),
              ),
          }),
        },
      };
      return handleSignSubmitNoConfirmation(mockLucid, failingBuilder).pipe(
        Effect.exit,
        Effect.map((exit) => {
          expect(exit._tag).toBe("Failure");
        }),
      );
    },
  );
});

describe("handleSignSubmitNoConfirmation tolerates wallet address failure", () => {
  it.effect(
    "handleSignSubmitNoConfirmation tolerates wallet address failure",
    () => {
      const lucidNoWallet: any = {
        wallet: () => ({
          address: () => Promise.reject(new Error("no wallet")),
        }),
      };
      return Effect.gen(function* () {
        const result = yield* handleSignSubmitNoConfirmation(
          lucidNoWallet,
          mockSignBuilder,
        );
        expect(result).toBe(mockTxHash);
      });
    },
  );
});

// ---------------------------------------------------------------------------
// Error class constructors
// ---------------------------------------------------------------------------

describe("TxSignError has expected tag and fields", () => {
  it("TxSignError has expected tag and fields", () => {
    const err = new TxSignError({
      message: "sign err",
      cause: undefined,
      txHash: "abc",
    });
    expect(err._tag).toBe("TxSignError");
    expect(err.txHash).toBe("abc");
  });
});

describe("TxSubmitError has expected tag and fields", () => {
  it("TxSubmitError has expected tag and fields", () => {
    const err = new TxSubmitError({
      message: "submit err",
      cause: undefined,
      txHash: "def",
    });
    expect(err._tag).toBe("TxSubmitError");
    expect(err.txHash).toBe("def");
  });
});

describe("TxConfirmError has expected tag and fields", () => {
  it("TxConfirmError has expected tag and fields", () => {
    const err = new TxConfirmError({
      message: "confirm err",
      cause: undefined,
      txHash: "ghi",
    });
    expect(err._tag).toBe("TxConfirmError");
    expect(err.txHash).toBe("ghi");
  });
});

describe("GenesisDepositError has expected tag", () => {
  it("GenesisDepositError has expected tag", () => {
    const err = new GenesisDepositError({
      message: "genesis err",
      cause: undefined,
    });
    expect(err._tag).toBe("GenesisDepositError");
  });
});

// ---------------------------------------------------------------------------
// fetchFirstBlockTxs
// ---------------------------------------------------------------------------

describe("fetchFirstBlockTxs returns empty txs for genesis header hash", () => {
  it.effect(
    "fetchFirstBlockTxs returns empty txs for genesis header hash",
    () => {
      const genesisHash =
        "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421";
      headerHashFromStateQueueUTxOMock.mockReturnValue(
        Effect.succeed(genesisHash),
      );
      retrieveTxHashesByHeaderHashMock.mockReturnValue(Effect.succeed([]));

      const fakeUTxO: any = {};
      return fetchFirstBlockTxs(fakeUTxO).pipe(
        Effect.map(({ txs, headerHash }) => {
          expect(txs.length).toBe(0);
          expect(headerHash).toBeInstanceOf(Buffer);
        }),
        Effect.provide(mockDbLayer),
      );
    },
  );
});

describe("fetchFirstBlockTxs fetches tx CBORs for non-genesis block", () => {
  it.effect("fetchFirstBlockTxs fetches tx CBORs for non-genesis block", () => {
    const nonGenesisHash = "bb".repeat(32);
    const txHash1 = Buffer.alloc(32, 0x11);
    const txCbor1 = Buffer.alloc(64, 0x22);

    headerHashFromStateQueueUTxOMock.mockReturnValue(
      Effect.succeed(nonGenesisHash),
    );
    retrieveTxHashesByHeaderHashMock.mockReturnValue(Effect.succeed([txHash1]));
    retrieveTxCborsByHashesMock.mockReturnValue(Effect.succeed([txCbor1]));

    const fakeUTxO: any = {};
    return fetchFirstBlockTxs(fakeUTxO).pipe(
      Effect.map(({ txs, headerHash }) => {
        expect(txs.length).toBe(1);
        expect(txs[0]).toEqual(txCbor1);
        expect(headerHash).toBeInstanceOf(Buffer);
      }),
      Effect.provide(mockDbLayer),
    );
  });
});
