#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "${SCRIPT_DIR}/../../../" && pwd)"
cd "${REPO_ROOT}"

PG_CONT=midgard-legacy-pg
NODE_ENV_FILE="demo/midgard-node/.env"
BACKUP_ENV_FILE="demo/midgard-node/.env.bak.legacy"
OGMIOS_MOCK_PORT=1337
OGMIOS_MOCK_HOST=127.0.0.1
OGMIOS_MOCK_PID=""

cleanup() {
  if [ -n "$OGMIOS_MOCK_PID" ]; then
    kill "$OGMIOS_MOCK_PID" >/dev/null 2>&1 || true
  fi
  docker rm -f "$PG_CONT" >/dev/null 2>&1 || true
  if [ -f "$BACKUP_ENV_FILE" ]; then
    mv "$BACKUP_ENV_FILE" "$NODE_ENV_FILE"
  else
    rm -f "$NODE_ENV_FILE"
  fi
}
trap cleanup EXIT

node >/dev/null 2>&1 <<'EOF' &
const http = require("node:http");

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

const server = http.createServer((req, res) => {
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

server.listen(1337, "127.0.0.1", () => {
  process.stdout.write("ogmios-mock-ready\n");
});
EOF
OGMIOS_MOCK_PID="$!"

for _ in $(seq 1 30); do
  if nc -z "$OGMIOS_MOCK_HOST" "$OGMIOS_MOCK_PORT" >/dev/null 2>&1; then
    break
  fi
  sleep 1
done

if ! nc -z "$OGMIOS_MOCK_HOST" "$OGMIOS_MOCK_PORT" >/dev/null 2>&1; then
  echo "Failed to start Ogmios mock server on ${OGMIOS_MOCK_HOST}:${OGMIOS_MOCK_PORT}" >&2
  exit 1
fi

docker rm -f "$PG_CONT" >/dev/null 2>&1 || true
docker run -d --name "$PG_CONT" \
  -e POSTGRES_USER=postgres \
  -e POSTGRES_PASSWORD=postgres \
  -e POSTGRES_DB=midgard \
  -p 5432:5432 \
  postgres:15-alpine >/dev/null

until docker exec "$PG_CONT" pg_isready -U postgres -d midgard >/dev/null 2>&1; do
  sleep 1
done

rm -f "$BACKUP_ENV_FILE"
if [ -f "$NODE_ENV_FILE" ]; then
  cp "$NODE_ENV_FILE" "$BACKUP_ENV_FILE"
fi
cp demo/midgard-node/.env.example "$NODE_ENV_FILE"
sed -i 's/^POSTGRES_HOST=.*/POSTGRES_HOST=localhost/' "$NODE_ENV_FILE"
sed -i 's|^L1_OGMIOS_KEY=.*|L1_OGMIOS_KEY=http://127.0.0.1:1337|' "$NODE_ENV_FILE"
sed -i 's|^L1_KUPO_KEY=.*|L1_KUPO_KEY=http://127.0.0.1:1442|' "$NODE_ENV_FILE"

set +e
npm --prefix demo run test:legacy:all
TEST_EXIT_CODE=$?
set -e

exit "$TEST_EXIT_CODE"
