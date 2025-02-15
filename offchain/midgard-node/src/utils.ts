import {
  Blockfrost,
  CML,
  Koios,
  Kupmios,
  Lucid,
  LucidEvolution,
  Maestro,
  Network,
  OutRef,
  Provider,
  Script,
  ScriptType,
  UTxO,
  valueToAssets,
} from "@lucid-evolution/lucid";
import * as SDK from "@al-ft/midgard-sdk";
import * as chalk_ from "chalk";

export const chalk = new chalk_.Chalk();

export type Result<T> =
  | { type: "ok"; data: T }
  | { type: "error"; error: Error };

export function ok<T>(x: T): Result<T> {
  return {
    type: "ok",
    data: x,
  };
}

export function fail<T>(e: string): Result<T> {
  return {
    type: "error",
    error: new Error(e),
  };
}

export type ProviderName = "Blockfrost" | "Koios" | "Kupmios" | "Maestro";

export const errorToString = (error: any): string => {
  return error.message ?? JSON.stringify(error);
};

export const showTime = (d: Date): string => {
  return d
    .toLocaleString("en-US", {
      month: "2-digit",
      day: "2-digit",
      year: "numeric",
      hour: "2-digit",
      minute: "2-digit",
      second: "2-digit",
      hour12: false,
    })
    .replace(/\//g, ".");
};

const logWithTime = (
  color: chalk_.ChalkInstance,
  label: string,
  msg: string,
) => {
  const now = new Date();
  const timeStr = showTime(now);
  console.log(
    `${color(chalk.bold(`${timeStr}\u0009${label}`))}${
      label === "" ? "" : " "
    }${color(msg)}`,
  );
};

export const logSuccess = (msg: string) => {
  logWithTime(chalk.green, "SUCCESS!", msg);
};

export const logWarning = (msg: string, quiet?: true) => {
  if (!quiet) {
    logWithTime(
      chalk.yellow,
      "WARNING",
      `
${msg}`,
    );
  }
};

export const logAbort = (msg: string) => {
  logWithTime(
    chalk.red,
    "ABORT",
    `
${msg}`,
  );
};

export const logDim = (msg: string) => {
  logWithTime(chalk.dim, "", msg);
};

export const logInfo = (msg: string) => {
  logWithTime(
    chalk.blue,
    "INFO",
    `
${msg}`,
  );
};

export const isHexString = (str: string): boolean => {
  const hexRegex = /^[0-9A-Fa-f]+$/;
  return hexRegex.test(str);
};

export const setupLucid = async (
  network: Network,
  providerName: ProviderName,
): Promise<LucidEvolution> => {
  const seedPhrase = process.env.SEED_PHRASE;
  if (!seedPhrase) {
    logAbort("No wallet seed phrase found (SEED_PHRASE)");
    process.exit(1);
  }
  const networkStr = `${network}`.toLowerCase();
  let provider: Provider;
  if (providerName === "Blockfrost" || providerName === "Maestro") {
    const apiKey = process.env.API_KEY;
    if (!apiKey) {
      logAbort("No API key was found (API_KEY)");
      process.exit(1);
    }
    if (providerName === "Blockfrost") {
      provider = new Blockfrost(
        `https://cardano-${networkStr}.blockfrost.io/api/v0`,
        apiKey,
      );
    } else {
      provider = new Maestro({
        network: network === "Custom" ? "Mainnet" : network,
        apiKey,
      });
    }
  } else if (providerName === "Koios") {
    provider = new Koios(
      `https://${network === "Mainnet" ? "api" : networkStr}.koios.rest/api/v1`,
    );
  } else {
    const kupoURL = process.env.KUPO_URL;
    const ogmiosURL = process.env.OGMIOS_URL;
    if (!kupoURL || !ogmiosURL) {
      logAbort(
        "Make sure to set both KUPO_URL and OGMIOS_URL environment variables",
      );
      process.exit(1);
    }
    provider = new Kupmios(kupoURL, ogmiosURL);
  }
  try {
    const lucid = await Lucid(provider, network);
    lucid.selectWallet.fromSeed(seedPhrase);
    return lucid;
  } catch (e) {
    logAbort(errorToString(e));
    process.exit(1);
  }
};

export const findSpentAndProducedUTxOs = (
  txCBOR: string,
): { spent: OutRef[]; produced: UTxO[] } => {
  try {
    const tx = CML.Transaction.from_cbor_hex(txCBOR);
    const txBody = tx.body();
    const inputs = txBody.inputs();
    const inputsCount = inputs.len();
    const spent: OutRef[] = [];
    for (let i = 0; i < inputsCount; i++) {
      try {
        const input = inputs.get(i);
        spent.push(SDK.Utils.cmlInputToOutRef(input));
      } catch (e) {
        console.log(e);
      }
    }
    const txHash = CML.hash_transaction(txBody).to_hex();
    const outputs = txBody.outputs();
    const outputsCount = outputs.len();
    const produced: UTxO[] = [];
    for (let i = 0; i < outputsCount; i++) {
      try {
        const output = outputs.get(i);
        const scriptRefObj = output.script_ref();
        let scriptRef: Script | null | undefined = undefined;
        if (scriptRefObj) {
          const scriptRefKind = scriptRefObj.kind();
          const [scriptKind, scriptHex]: [ScriptType, string | undefined] =
            scriptRefKind === CML.ScriptKind.Native
              ? ["Native", scriptRefObj.as_native()?.to_cbor_hex()]
              : scriptRefKind === CML.ScriptKind.PlutusV1
                ? ["PlutusV1", scriptRefObj.as_plutus_v1()?.to_cbor_hex()]
                : scriptRefKind === CML.ScriptKind.PlutusV2
                  ? ["PlutusV2", scriptRefObj.as_plutus_v2()?.to_cbor_hex()]
                  : ["PlutusV3", scriptRefObj.as_plutus_v3()?.to_cbor_hex()];
          if (scriptHex) {
            scriptRef = { type: scriptKind, script: scriptHex };
          }
        }
        const theUTxO: UTxO = {
          address: output.address().to_bech32(),
          assets: valueToAssets(output.amount()),
          datumHash: output.datum_hash()?.to_hex(),
          datum: output.datum()?.to_cbor_hex(),
          scriptRef,
          txHash,
          outputIndex: i,
        };
        produced.push(theUTxO);
      } catch (e) {
        console.log(e);
      }
    }
    return { spent, produced };
  } catch (_e) {
    throw Error("Something went wrong decoding the transaction");
  }
};

export const ENV_VARS_GUIDE = `
Make sure you first have set the environment variable for your seed phrase:

\u0009${chalk.bold("SEED_PHRASE")}\u0009 Your wallet's seed phrase

Depending on which provider you'll be using, other environment variables may also be needed:

Blockfrost or Maestro:
\u0009${chalk.bold("API_KEY")}    \u0009 Your provider's API key

Kupmios:
\u0009${chalk.bold("KUPO_URL")}   \u0009 URL of your Kupo instance
\u0009${chalk.bold("OGMIOS_URL")} \u0009 URL of your Ogmios instance
`;
