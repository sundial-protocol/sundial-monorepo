import { createServer } from "node:http";

const host = process.env.OGMIOS_MOCK_HOST ?? "127.0.0.1";
const port = Number.parseInt(process.env.OGMIOS_MOCK_PORT ?? "1337", 10);

if (!Number.isInteger(port) || port < 1 || port > 65535) {
  process.stderr.write(`Invalid OGMIOS_MOCK_PORT: ${process.env.OGMIOS_MOCK_PORT ?? ""}\n`);
  process.exit(1);
}

const responseBody = JSON.stringify({
  jsonrpc: "2.0",
  method: "queryLedgerState/protocolParameters",
  id: null,
  result: {
    minFeeCoefficient: 44,
    minFeeReferenceScripts: { base: 15, range: 0.2, multiplier: 1 },
    maxReferenceScriptsSize: { bytes: 200000 },
    stakePoolVotingThresholds: {
      noConfidence: "1/2",
      constitutionalCommittee: {
        default: "1/2",
        stateOfNoConfidence: "1/2",
      },
      hardForkInitiation: "1/2",
      protocolParametersUpdate: { security: "1/2" },
    },
    delegateRepresentativeVotingThresholds: {
      noConfidence: "1/2",
      constitutionalCommittee: {
        default: "1/2",
        stateOfNoConfidence: "1/2",
      },
      constitution: "1/2",
      hardForkInitiation: "1/2",
      protocolParametersUpdate: {
        network: "1/2",
        economic: "1/2",
        technical: "1/2",
        governance: "1/2",
      },
      treasuryWithdrawals: "1/2",
    },
    constitutionalCommitteeMinSize: 1,
    constitutionalCommitteeMaxTermLength: 100,
    governanceActionLifetime: 100,
    governanceActionDeposit: { ada: { lovelace: 100000000 } },
    delegateRepresentativeDeposit: { ada: { lovelace: 2000000 } },
    delegateRepresentativeMaxIdleTime: 100,
    minFeeConstant: { ada: { lovelace: 155381 } },
    maxBlockBodySize: { bytes: 90112 },
    maxBlockHeaderSize: { bytes: 1100 },
    maxTransactionSize: { bytes: 16384 },
    stakeCredentialDeposit: { ada: { lovelace: 2000000 } },
    stakePoolDeposit: { ada: { lovelace: 500000000 } },
    stakePoolRetirementEpochBound: 18,
    desiredNumberOfStakePools: 500,
    stakePoolPledgeInfluence: "3/10",
    monetaryExpansion: "3/1000",
    treasuryExpansion: "1/5",
    minStakePoolCost: { ada: { lovelace: 170000000 } },
    minUtxoDepositConstant: { ada: { lovelace: 1000000 } },
    minUtxoDepositCoefficient: 4310,
    plutusCostModels: {
      "plutus:v1": [],
      "plutus:v2": [],
      "plutus:v3": [],
    },
    scriptExecutionPrices: { memory: "577/10000", cpu: "721/10000000" },
    maxExecutionUnitsPerTransaction: { memory: 14000000, cpu: 10000000000 },
    maxExecutionUnitsPerBlock: { memory: 62000000, cpu: 20000000000 },
    maxValueSize: { bytes: 5000 },
    collateralPercentage: 150,
    maxCollateralInputs: 3,
    version: { major: 10, minor: 0 },
  },
});

const server = createServer((req, res) => {
  if (req.method !== "POST") {
    res.statusCode = 405;
    res.setHeader("content-type", "application/json");
    res.end(JSON.stringify({ error: "method_not_allowed" }));
    return;
  }

  req.on("data", () => {});
  req.on("end", () => {
    res.statusCode = 200;
    res.setHeader("content-type", "application/json");
    res.end(responseBody);
  });
});

const shutdown = () => {
  server.close(() => {
    process.exit(0);
  });
};

process.on("SIGTERM", shutdown);
process.on("SIGINT", shutdown);

server.listen(port, host, () => {
  process.stdout.write("ogmios-mock-ready\n");
});
