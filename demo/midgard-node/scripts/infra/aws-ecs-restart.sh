#!/usr/bin/env bash
set -euo pipefail

AWS_SESSION_ENV_FILE="${AWS_SESSION_ENV_FILE:-${XDG_CACHE_HOME:-${HOME}/.cache}/sundial-node/aws-session.env}"
ENVIRONMENT="testnet"
REGION=""
SERVICE="all"

fail() {
  printf '[aws-ecs-restart] error: %s\n' "$*" >&2
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
    --environment=*) ENVIRONMENT="${arg#*=}" ;;
    --region=*) REGION="${arg#*=}" ;;
    --service=*) SERVICE="${arg#*=}" ;;
    --help|-h)
      echo "Usage: ./scripts/infra/aws-ecs-restart.sh --environment=testnet [--service=name|all]" >&2
      exit 0
      ;;
    *) fail "unknown argument: ${arg}" ;;
  esac
done

case "${ENVIRONMENT}" in
  testnet) REGION="${REGION:-us-west-2}" ;;
  *) fail "--environment must be testnet" ;;
esac
CLUSTER_NAME="sundial-node-${ENVIRONMENT}"
SERVICES=(sundial-node prometheus loki alloy grafana postgres-exporter)

command -v aws >/dev/null 2>&1 || fail "required command not found: aws"
load_cached_aws_session_env
export AWS_SDK_LOAD_CONFIG="${AWS_SDK_LOAD_CONFIG:-1}"

if [[ "${SERVICE}" != "all" ]]; then
  SERVICES=("${SERVICE}")
fi

for service_name in "${SERVICES[@]}"; do
  aws ecs update-service \
    --cluster "${CLUSTER_NAME}" \
    --service "${service_name}" \
    --region "${REGION}" \
    --force-new-deployment \
    >/dev/null
  printf '[aws-ecs-restart] restarted service=%s\n' "${service_name}" >&2
done
