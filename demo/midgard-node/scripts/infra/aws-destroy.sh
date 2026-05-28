#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "${ROOT_DIR}"

ENVIRONMENT="testnet"
SERVICE=""
DRY_RUN=false

fail() {
  printf '[aws-destroy] error: %s\n' "$*" >&2
  exit 1
}

run_cmd() {
  if [[ "${DRY_RUN}" == "true" ]]; then
    printf '[aws-destroy] [dry-run] ' >&2
    printf '%q ' "$@" >&2
    printf '\n' >&2
    return 0
  fi
  "$@"
}

usage() {
  cat <<'USAGE' >&2
Usage: ./scripts/infra/aws-destroy.sh --service=<name> [options]

Options:
  --environment=testnet          Deployment environment (default: testnet)
  --service=<sundial-node|prometheus|loki|alloy|grafana|postgres-exporter>
                         Service to destroy (required)
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --environment=*)
      ENVIRONMENT="${1#*=}"
      ;;
    --service=*)
      SERVICE="${1#*=}"
      ;;
    --service)
      shift
      SERVICE="${1:-}"
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      fail "unknown argument: $1"
      ;;
  esac
  shift
done

[[ -n "${SERVICE}" ]] || fail "destroy requires explicit --service"
case "${ENVIRONMENT}" in
  testnet) ;;
  *)
    fail "--environment must be testnet"
    ;;
esac
[[ "${IS_NOT_DRY_RUN:-}" == "true" ]] || DRY_RUN=true

target=""
case "${SERVICE}" in
  sundial-node)
    target="aws_ecs_service.sundial_node"
    ;;
  prometheus|loki|alloy|grafana)
    target="aws_ecs_service.${SERVICE}"
    ;;
  postgres-exporter)
    target="aws_ecs_service.postgres_exporter"
    ;;
  *)
    fail "--service must be one of: sundial-node prometheus loki alloy grafana postgres-exporter"
    ;;
esac

run_cmd bash ./scripts/infra/aws.sh destroy \
  --module=platform \
  --backend-config=backends/${ENVIRONMENT}.hcl \
  "-var-file=envs/${ENVIRONMENT}.tfvars" \
  "-target=${target}"
