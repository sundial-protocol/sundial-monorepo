#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "${ROOT_DIR}"

usage() {
  cat <<'USAGE' >&2
Usage: ./scripts/infra/aws-apply.sh <plan|apply|output|validate> [options]

Options:
  --environment=testnet          Deployment environment (default: testnet)
  --service=<shared|sundial-node|prometheus|loki|alloy|grafana|postgres-exporter|obs|rds>
                                 Service selector for targeted apply
  --image-tag <tag>             Optional image tag for sundial-node
  --image-latest-tag <tag>      Also push mutable alias tag, or "none"
  --help                        Show this message

Apply mode is a dry run unless IS_NOT_DRY_RUN=true is set.
USAGE
}

log() {
  printf '[aws-apply] %s\n' "$*" >&2
}

fail() {
  printf '[aws-apply] error: %s\n' "$*" >&2
  exit 1
}

run_cmd() {
  if [[ "${DRY_RUN}" == "true" ]]; then
    printf '[aws-apply] [dry-run] ' >&2
    printf '%q ' "$@" >&2
    printf '\n' >&2
    return 0
  fi
  "$@"
}

ACTION="${1:-}"
[[ -n "${ACTION}" ]] || {
  usage
  exit 1
}
shift || true

ENVIRONMENT="testnet"
SERVICE=""
DRY_RUN=false
IMAGE_TAG_OVERRIDE=""
ECR_ARGS=()
APP_IMAGE=""

case "${ACTION}" in
  plan|apply|output|validate) ;;
  *)
    usage
    exit 1
    ;;
esac

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
    --image-tag=*)
      IMAGE_TAG_OVERRIDE="${1#*=}"
      ;;
    --image-tag)
      shift
      IMAGE_TAG_OVERRIDE="${1:-}"
      [[ -n "${IMAGE_TAG_OVERRIDE}" ]] || fail "--image-tag requires a value"
      ;;
    --image-latest-tag=*)
      ECR_ARGS+=("--image-latest-tag" "${1#*=}")
      ;;
    --image-latest-tag)
      shift
      ECR_ARGS+=("--image-latest-tag" "${1:-}")
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

if [[ -n "${SERVICE}" ]]; then
  case "${SERVICE}" in
    shared|sundial-node|prometheus|loki|alloy|grafana|postgres-exporter|obs|rds) ;;
    *)
      fail "--service must be one of: shared sundial-node prometheus loki alloy grafana postgres-exporter obs rds"
      ;;
  esac
fi

if [[ "${ACTION}" == "apply" && "${IS_NOT_DRY_RUN:-}" != "true" ]]; then
  DRY_RUN=true
  ECR_ARGS+=("--dry-run")
fi

resolve_platform_region() {
  if [[ -n "${AWS_PLATFORM_REGION:-}" ]]; then
    printf '%s' "${AWS_PLATFORM_REGION}"
    return 0
  fi

  case "${ENVIRONMENT}" in
    testnet) printf '%s' "us-west-2" ;;
    *) fail "unsupported environment for region mapping: ${ENVIRONMENT}" ;;
  esac
}

resolve_account_id() {
  if [[ -n "${AWS_ACCOUNT_ID:-}" ]]; then
    printf '%s' "${AWS_ACCOUNT_ID}"
  elif [[ "${DRY_RUN}" == "true" ]]; then
    printf '%s' "000000000000"
  else
    aws sts get-caller-identity --query Account --output text
  fi
}

resolve_repository_name() {
  printf '%s' "${AWS_ECR_SUNDIAL_NODE_REPOSITORY:-sundial/sundial-node}"
}

resolve_image_tag() {
  if [[ -n "${IMAGE_TAG_OVERRIDE}" ]]; then
    printf '%s' "${IMAGE_TAG_OVERRIDE}"
    return 0
  fi
  printf '%s' "${ENVIRONMENT}-latest"
}

resolve_image_ref() {
  local region=""
  local account_id=""
  local repository=""
  local image_tag=""
  local digest=""

  region="$(resolve_platform_region)"
  account_id="$(resolve_account_id)"
  repository="$(resolve_repository_name)"
  image_tag="$(resolve_image_tag)"

  if [[ "${DRY_RUN}" == "true" ]]; then
    printf '%s.dkr.ecr.%s.amazonaws.com/%s:%s' "${account_id}" "${region}" "${repository}" "${image_tag}"
    return 0
  fi

  digest="$(aws ecr describe-images \
    --repository-name "${repository}" \
    --image-ids "imageTag=${image_tag}" \
    --region "${region}" \
    --query 'imageDetails[0].imageDigest' \
    --output text)"

  printf '%s.dkr.ecr.%s.amazonaws.com/%s@%s' "${account_id}" "${region}" "${repository}" "${digest}"
}

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
      fail "unsupported service target: $1"
      ;;
  esac
}

case "${ACTION}" in
  output)
    exec bash ./scripts/infra/aws.sh output --module=platform --backend-config=backends/${ENVIRONMENT}.hcl
    ;;
  validate)
    exec bash ./scripts/infra/aws.sh validate --module=platform
    ;;
  plan)
    plan_args=("--environment=${ENVIRONMENT}")
    if [[ -n "${SERVICE}" ]]; then
      plan_args+=("--service=${SERVICE}")
    fi
    exec bash ./scripts/infra/aws-plan.sh "${plan_args[@]}"
    ;;
esac

if [[ "${SERVICE}" == "sundial-node" ]]; then
  if [[ -n "${IMAGE_TAG_OVERRIDE}" ]]; then
    ECR_ARGS+=("--image-tag" "${IMAGE_TAG_OVERRIDE}")
  fi
  run_cmd bash ./scripts/infra/aws-bootstrap.sh ecr --environment="${ENVIRONMENT}" "${ECR_ARGS[@]}"
  APP_IMAGE="$(resolve_image_ref)"
fi

args=("-var-file=envs/${ENVIRONMENT}.tfvars")
if [[ -n "${APP_IMAGE}" ]]; then
  args+=("-var=sundial_node_image=${APP_IMAGE}")
fi

if [[ -n "${SERVICE}" ]]; then
  while IFS= read -r target; do
    args+=("-target=${target}")
  done < <(terraform_targets_for_service "${SERVICE}")
fi

log "action=apply environment=${ENVIRONMENT} service=${SERVICE:-platform} dry_run=${DRY_RUN}"
run_cmd bash ./scripts/infra/aws.sh apply --module=platform --backend-config=backends/${ENVIRONMENT}.hcl "${args[@]}"
log "completed action=apply environment=${ENVIRONMENT} service=${SERVICE:-platform}"
