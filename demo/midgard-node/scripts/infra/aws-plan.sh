#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "${ROOT_DIR}"

usage() {
  cat <<'USAGE' >&2
Usage: ./scripts/infra/aws-plan.sh [options]

Options:
  --environment=testnet          Deployment environment (default: testnet)
  --service=<sundial-node|prometheus|loki|alloy|grafana|postgres-exporter|obs|shared|rds>
                       Optional targeted service selector
  --help               Show this message
USAGE
}

fail() {
  printf '[aws-plan] error: %s\n' "$*" >&2
  exit 1
}

log() {
  printf '[aws-plan] %s\n' "$*" >&2
}

ENVIRONMENT="testnet"
SERVICE=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --environment=*)
      ENVIRONMENT="${1#*=}"
      ;;
    --environment)
      shift
      ENVIRONMENT="${1:-}"
      [[ -n "${ENVIRONMENT}" ]] || fail "--environment requires a value"
      ;;
    --service=*)
      SERVICE="${1#*=}"
      ;;
    --service)
      shift
      SERVICE="${1:-}"
      [[ -n "${SERVICE}" ]] || fail "--service requires a value"
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

case "${ENVIRONMENT}" in
  testnet) ;;
  *)
    fail "--environment must be testnet"
    ;;
esac

terraform_targets_for_service() {
  case "$1" in
    sundial-node)
      printf '%s\n' \
        "aws_service_discovery_service.sundial_node" \
        "aws_lb_target_group.sundial_node" \
        "aws_ecs_task_definition.sundial_node" \
        "aws_ecs_service.sundial_node"
      ;;
    grafana)
      printf '%s\n' \
        "aws_lb_target_group.grafana" \
        "aws_lb_listener_rule.http_bootstrap_grafana" \
        "aws_lb_listener_rule.https_grafana" \
        "aws_service_discovery_service.grafana" \
        "aws_ecs_task_definition.grafana" \
        "aws_ecs_service.grafana"
      ;;
    prometheus|loki|alloy)
      printf '%s\n' \
        "aws_service_discovery_service.$1" \
        "aws_ecs_task_definition.$1" \
        "aws_ecs_service.$1"
      ;;
    postgres-exporter)
      printf '%s\n' \
        "aws_service_discovery_service.postgres_exporter" \
        "aws_ecs_task_definition.postgres_exporter" \
        "aws_ecs_service.postgres_exporter"
      ;;
    obs)
      printf '%s\n' \
        "aws_ecs_service.prometheus" \
        "aws_ecs_service.loki" \
        "aws_ecs_service.alloy" \
        "aws_ecs_service.grafana" \
        "aws_ecs_service.postgres_exporter"
      ;;
    rds)
      printf '%s\n' \
        "aws_db_subnet_group.main" \
        "aws_db_parameter_group.main" \
        "aws_db_instance.main"
      ;;
    shared)
      return 0
      ;;
    *)
      fail "--service must be one of: sundial-node prometheus loki alloy grafana postgres-exporter obs shared rds"
      ;;
  esac
}

args=("-var-file=envs/${ENVIRONMENT}.tfvars")

if [[ -n "${SERVICE}" ]]; then
  while IFS= read -r target; do
    args+=("-target=${target}")
  done < <(terraform_targets_for_service "${SERVICE}")
  log "environment=${ENVIRONMENT} service=${SERVICE} scope=targeted"
else
  log "environment=${ENVIRONMENT} scope=platform"
fi

bash ./scripts/infra/aws.sh plan --module=platform --backend-config=backends/${ENVIRONMENT}.hcl "${args[@]}"
