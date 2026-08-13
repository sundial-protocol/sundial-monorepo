import { describe, expect, it, vi, beforeEach } from "vitest";
import { Effect } from "effect";
import { SqlClient } from "@effect/sql";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";

// The handler calls `AddressHistoryDB.retrieve` directly; mock that single
// collaborator so we can assert what pagination options the HTTP layer
// derives from `limit`/`offset` query params, without needing a real DB.
let retrieveImpl: (
  address: string,
  options?: { limit?: number; offset?: number },
) => Effect.Effect<readonly Buffer[], unknown>;

vi.mock("@/database/addressHistory.js", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("@/database/addressHistory.js")>();
  return {
    ...actual,
    retrieve: (
      address: string,
      options?: { limit?: number; offset?: number },
    ) => retrieveImpl(address, options),
  };
});

// The `@al-ft/midgard-sdk` stub doesn't implement `bufferToHex`, which the
// handler uses to serialize returned cbors; extend it here.
vi.mock("@al-ft/midgard-sdk", async (importOriginal) => {
  const actual = await importOriginal<typeof import("@al-ft/midgard-sdk")>();
  return {
    ...actual,
    bufferToHex: (buf: Buffer) => buf.toString("hex"),
  };
});

// `@lucid-evolution/lucid` is aliased to a stub that doesn't implement
// `getAddressDetails`; extend it here so the handler's address-validation
// branch can be exercised.
vi.mock("@lucid-evolution/lucid", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("@lucid-evolution/lucid")>();
  return {
    ...actual,
    getAddressDetails: (addr: string) => {
      if (!addr.startsWith("addr_test1")) {
        throw new Error(`invalid address: ${addr}`);
      }
      return {
        paymentCredential: { type: "Key", hash: "deadbeef" },
        address: { bech32: addr },
      };
    },
  };
});

import { getTxsOfAddressHandlerForTesting } from "@/commands/listen.js";
import * as AddressHistoryDB from "@/database/addressHistory.js";

const TEST_ADDRESS =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";

const runHandler = (url: string) => {
  const request = HttpServerRequest.fromWeb(new Request(url));
  return getTxsOfAddressHandlerForTesting.pipe(
    Effect.provideService(HttpServerRequest.HttpServerRequest, request),
    Effect.provideService(
      HttpServerRequest.ParsedSearchParams,
      HttpServerRequest.searchParamsFromURL(new URL(url)),
    ),
    // `AddressHistoryDB.retrieve` is mocked, so the SqlClient is never used
    // at runtime; it's only present in the handler's static requirements.
    Effect.provideService(
      SqlClient.SqlClient,
      {} as unknown as SqlClient.SqlClient,
    ),
  );
};

const responseJson = async (
  response: HttpServerResponse.HttpServerResponse,
) => {
  const web = HttpServerResponse.toWeb(response);
  return { status: web.status, body: await web.json() };
};

describe("getTxsOfAddressHandler pagination", () => {
  beforeEach(() => {
    retrieveImpl = () =>
      Effect.die("AddressHistoryDB.retrieve should not be called here");
  });

  it("defaults to DEFAULT_ADDRESS_HISTORY_LIMIT and offset 0 when unspecified", async () => {
    let capturedOptions: { limit?: number; offset?: number } | undefined;
    retrieveImpl = (_address, options) => {
      capturedOptions = options;
      return Effect.succeed([]);
    };

    const response = await Effect.runPromise(
      runHandler(`http://localhost/txs?address=${TEST_ADDRESS}`),
    );
    const { status, body } = await responseJson(response);

    expect(status).toBe(200);
    expect(capturedOptions).toEqual({
      limit: AddressHistoryDB.DEFAULT_ADDRESS_HISTORY_LIMIT,
      offset: 0,
    });
    expect(body).toMatchObject({
      txs: [],
      limit: AddressHistoryDB.DEFAULT_ADDRESS_HISTORY_LIMIT,
      offset: 0,
      hasMore: false,
    });
  });

  it("passes explicit limit and offset through to the DB layer", async () => {
    let capturedOptions: { limit?: number; offset?: number } | undefined;
    retrieveImpl = (_address, options) => {
      capturedOptions = options;
      return Effect.succeed([Buffer.from("aa", "hex")]);
    };

    const response = await Effect.runPromise(
      runHandler(
        `http://localhost/txs?address=${TEST_ADDRESS}&limit=2&offset=10`,
      ),
    );
    const { status, body } = await responseJson(response);

    expect(status).toBe(200);
    expect(capturedOptions).toEqual({ limit: 2, offset: 10 });
    // fewer rows than the limit came back, so this is the last page.
    expect(body).toMatchObject({ limit: 2, offset: 10, hasMore: false });
  });

  it("reports hasMore when a full page is returned", async () => {
    retrieveImpl = () =>
      Effect.succeed([Buffer.from("aa", "hex"), Buffer.from("bb", "hex")]);

    const response = await Effect.runPromise(
      runHandler(`http://localhost/txs?address=${TEST_ADDRESS}&limit=2`),
    );
    const { body } = await responseJson(response);

    expect(body).toMatchObject({ limit: 2, hasMore: true });
  });

  it("clamps a limit above MAX_ADDRESS_HISTORY_LIMIT instead of passing it through unbounded", async () => {
    let capturedOptions: { limit?: number; offset?: number } | undefined;
    retrieveImpl = (_address, options) => {
      capturedOptions = options;
      return Effect.succeed([]);
    };

    const response = await Effect.runPromise(
      runHandler(`http://localhost/txs?address=${TEST_ADDRESS}&limit=1000000`),
    );
    const { status } = await responseJson(response);

    expect(status).toBe(200);
    expect(capturedOptions).toEqual({
      limit: AddressHistoryDB.MAX_ADDRESS_HISTORY_LIMIT,
      offset: 0,
    });
  });

  it("rejects a non-numeric limit with 400 and never queries the DB", async () => {
    const response = await Effect.runPromise(
      runHandler(`http://localhost/txs?address=${TEST_ADDRESS}&limit=abc`),
    );
    const { status, body } = await responseJson(response);

    expect(status).toBe(400);
    expect(body).toMatchObject({
      error: expect.stringContaining("Invalid limit"),
    });
  });

  it("rejects a zero limit with 400", async () => {
    const response = await Effect.runPromise(
      runHandler(`http://localhost/txs?address=${TEST_ADDRESS}&limit=0`),
    );
    const { status } = await responseJson(response);

    expect(status).toBe(400);
  });

  it("rejects a negative offset with 400 and never queries the DB", async () => {
    const response = await Effect.runPromise(
      runHandler(`http://localhost/txs?address=${TEST_ADDRESS}&offset=-1`),
    );
    const { status, body } = await responseJson(response);

    expect(status).toBe(400);
    expect(body).toMatchObject({
      error: expect.stringContaining("Invalid offset"),
    });
  });

  it("rejects a non-numeric offset with 400", async () => {
    const response = await Effect.runPromise(
      runHandler(`http://localhost/txs?address=${TEST_ADDRESS}&offset=xyz`),
    );
    const { status } = await responseJson(response);

    expect(status).toBe(400);
  });
});
