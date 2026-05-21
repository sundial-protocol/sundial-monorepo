#!/usr/bin/env bash
set -euo pipefail

AWS_SESSION_ENV_FILE="${AWS_SESSION_ENV_FILE:-${XDG_CACHE_HOME:-${HOME}/.cache}/sundial-node/aws-session.env}"

ENVIRONMENT="testnet"
REGION=""
SERVICE="all"
SINCE="30m"
FOLLOW=0
EVENTS_ONLY=0
EVENT_COUNT=20

usage() {
  cat <<'USAGE' >&2
Usage: ./scripts/infra/aws-ecs-logs.sh [options]

Options:
  --environment=<testnet|mainnet>
                             Deployment environment (default: testnet)
  --region=<aws-region>     AWS region (default: us-west-2 for testnet, us-west-1 for mainnet)
  --service=<name|all>      sundial-node,prometheus,loki,alloy,grafana,postgres-exporter,all
  --since=<duration>        CloudWatch tail window (default: 30m)
  --follow                  Follow log output (single service only)
  --events-only             Print ECS service events only
  --event-count=<n>         Number of ECS events to show (default: 20)
USAGE
}

fail() {
  printf '[aws-ecs-logs] error: %s\n' "$*" >&2
  exit 1
}

load_cached_aws_session_env() {
  if [[ -n "${AWS_ACCESS_KEY_ID:-}" && -n "${AWS_SECRET_ACCESS_KEY:-}" ]]; then
    return 0
  fi
  if [[ -n "${AWS_PROFILE:-}" || -n "${AWS_DEFAULT_PROFILE:-}" ]]; then
    return 0
  fi
  if [[ ! -f "${AWS_SESSION_ENV_FILE}" ]]; then
    return 0
  fi
  # shellcheck disable=SC1090
  source "${AWS_SESSION_ENV_FILE}"
  export AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY AWS_SESSION_TOKEN AWS_SDK_LOAD_CONFIG
}

for arg in "$@"; do
  case "${arg}" in
    --environment=*)
      ENVIRONMENT="${arg#*=}"
      ;;
    --region=*)
      REGION="${arg#*=}"
      ;;
    --service=*)
      SERVICE="${arg#*=}"
      ;;
    --since=*)
      SINCE="${arg#*=}"
      ;;
    --event-count=*)
      EVENT_COUNT="${arg#*=}"
      ;;
    --follow)
      FOLLOW=1
      ;;
    --events-only)
      EVENTS_ONLY=1
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      fail "unknown argument: ${arg}"
      ;;
  esac
done

case "${ENVIRONMENT}" in
  testnet) REGION="${REGION:-us-west-2}" ;;
  mainnet) REGION="${REGION:-us-west-1}" ;;
  *) fail "--environment must be one of: testnet mainnet" ;;
esac
CLUSTER_NAME="sundial-node-${ENVIRONMENT}"
LOG_GROUP="/ecs/${CLUSTER_NAME}"
VALID_SERVICES=(sundial-node prometheus loki alloy grafana postgres-exporter all)

is_valid_service=0
for candidate in "${VALID_SERVICES[@]}"; do
  [[ "${candidate}" == "${SERVICE}" ]] && is_valid_service=1
done
[[ "${is_valid_service}" -eq 1 ]] || fail "--service must be one of: ${VALID_SERVICES[*]}"
[[ "${FOLLOW}" -eq 0 || "${SERVICE}" != "all" ]] || fail "--follow requires a single service"

command -v aws >/dev/null 2>&1 || fail "required command not found: aws"
load_cached_aws_session_env
export AWS_SDK_LOAD_CONFIG="${AWS_SDK_LOAD_CONFIG:-1}"

services=()
if [[ "${SERVICE}" == "all" ]]; then
  services=(sundial-node prometheus loki alloy grafana postgres-exporter)
else
  services=("${SERVICE}")
fi

for service_name in "${services[@]}"; do
  printf '\n===== %s =====\n' "${service_name}"
  aws ecs describe-services \
    --cluster "${CLUSTER_NAME}" \
    --services "${service_name}" \
    --region "${REGION}" \
    --query "services[0].events[0:${EVENT_COUNT}].[createdAt,message]" \
    --output table

  if [[ "${EVENTS_ONLY}" -eq 0 ]]; then
    if [[ "${FOLLOW}" -eq 1 ]]; then
      aws logs tail "${LOG_GROUP}" --region "${REGION}" --log-stream-name-prefix "${service_name}" --since "${SINCE}" --follow
    else
      aws logs tail "${LOG_GROUP}" --region "${REGION}" --log-stream-name-prefix "${service_name}" --since "${SINCE}"
    fi
  fi
done
