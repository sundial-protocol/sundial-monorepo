import { describe, expect, it } from "vitest";
import * as sdk from "@sdk/index.ts";
import { Effect } from "effect";
import { makeFakeLucid } from "./harness/fake-lucid.ts";
import {
  FIXTURE_ADDRESS_SCRIPT_A,
  FIXTURE_NONCE_UTXO,
  NETWORK,
} from "./harness/fixtures.ts";
import { withFakeCardanoHttp } from "./harness/fake-cardano-http.ts";
import { makeTempMptPaths } from "./harness/mpt-temp.ts";

describe("SDK package and harness integration", () => {
  it("exports core SDK functions needed by consumers", () => {
    expect(typeof sdk.makeReturn).toBe("function");
    expect(typeof sdk.hashHexWithBlake2b256).toBe("function");
    expect(typeof sdk.fetchDepositUTxOsProgram).toBe("function");
    expect(typeof sdk.fetchLatestCommittedBlockProgram).toBe("function");
    expect(typeof sdk.incompleteInitializationTxProgram).toBe("function");
  });

  it("protocol parameters resolve for Preview network", () => {
    const params = sdk.getProtocolParameters(NETWORK);
    expect(params.event_wait_duration).toBeGreaterThan(0);
    expect(params.maturity_duration).toBeGreaterThanOrEqual(1n);
    expect(params.slashing_penalty).toBeGreaterThan(0n);
  });

  it("fake lucid exposes deterministic boundary behavior", async () => {
    const { lucid, submitRecorder } = makeFakeLucid({
      walletUtxos: [FIXTURE_NONCE_UTXO],
      utxosAt: { [FIXTURE_ADDRESS_SCRIPT_A]: [FIXTURE_NONCE_UTXO] },
    });

    const walletUtxos = await (lucid as any).wallet().getUtxos();
    const scriptUtxos = await (lucid as any).utxosAt(FIXTURE_ADDRESS_SCRIPT_A);
    const txid = await (lucid as any).wallet().submitTx("abcd");

    expect(walletUtxos).toHaveLength(1);
    expect(scriptUtxos).toHaveLength(1);
    expect(txid).toBe(submitRecorder.lastTxId);
    expect(submitRecorder.submitted).toEqual(["abcd"]);
  });

  it("fake HTTP provider records request and returns configured response", async () => {
    await withFakeCardanoHttp(
      {
        allowListenFailureFallback: true,
        routes: {
          "/submit": { status: 200, body: { ok: true } },
        },
      },
      async ({ port, requests }) => {
        if (port === 0) {
          expect(requests).toEqual([]);
          return;
        }
        const response = await fetch(`http://127.0.0.1:${port}/submit`, {
          method: "POST",
          headers: { "content-type": "application/json" },
          body: JSON.stringify({ tx: "deadbeef" }),
        });
        const body = await response.json();

        expect(response.status).toBe(200);
        expect(body.ok).toBe(true);
        expect(requests).toHaveLength(1);
        expect(requests[0].path).toBe("/submit");
        expect(requests[0].method).toBe("POST");
        expect(requests[0].body).toContain("deadbeef");
      },
    );
  });

  it("effect programs can run through SDK helpers in test mode", async () => {
    const value = await Effect.runPromise(
      sdk.hashHexWithBlake2b256("deadbeef"),
    );
    expect(typeof value).toBe("string");
    expect(value).toHaveLength(64);
  });

  it("temporary MPT paths are unique and cleanable", async () => {
    const a = makeTempMptPaths("sdk-int-a");
    const b = makeTempMptPaths("sdk-int-b");

    expect(a.ledgerPath).not.toBe(b.ledgerPath);
    expect(a.mempoolPath).not.toBe(b.mempoolPath);

    await a.cleanup();
    await b.cleanup();
  });
});
