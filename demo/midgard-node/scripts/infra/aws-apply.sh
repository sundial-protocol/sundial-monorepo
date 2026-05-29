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
  --image-tag <tag>             Explicit sundial-node release tag (default: resolved from HEAD);
                                 must match sundial-node-YYYYMMDDTHHMMSSZ-<shortsha>
  --image-latest-tag <tag>      Also push mutable alias tag, or "none"
  --help                        Show this message

Apply mode is a dry run unless IS_NOT_DRY_RUN=true is set.
sundial-node deploys require the current branch to be the target environment
branch (e.g. testnet) and a release tag on HEAD. Run
infra:testnet:release:tag:node first if needed.
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
APP_IMAGE_TAG=""
PASSTHROUGH_ARGS=()

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
    --)
      shift
      PASSTHROUGH_ARGS+=("$@")
      break
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
    shared|sundial-node|prometheus|loki|alloy|grafana|postgres-exporter|obs|rds|data-tier) ;;
    *)
      fail "--service must be one of: shared sundial-node prometheus loki alloy grafana postgres-exporter obs rds data-tier"
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

ensure_deploy_source_branch() {
  local current_branch="" local_head_sha="" remote_head_sha=""

  current_branch="$(git rev-parse --abbrev-ref HEAD)"
  if [[ "${current_branch}" != "${ENVIRONMENT}" ]]; then
    fail "sundial-node deploys must run from branch=${ENVIRONMENT}; current_branch=${current_branch}"
  fi

  git fetch --quiet origin "${ENVIRONMENT}"
  local_head_sha="$(git rev-parse HEAD)"
  remote_head_sha="$(git rev-parse "origin/${ENVIRONMENT}")"
  if [[ "${local_head_sha}" != "${remote_head_sha}" ]]; then
    fail "local HEAD (${local_head_sha}) must match origin/${ENVIRONMENT} (${remote_head_sha}) before deploy"
  fi

  git fetch --quiet --tags origin
}

is_service_release_tag() {
  [[ "$1" =~ ^sundial-node-[0-9]{8}T[0-9]{6}Z-[0-9a-f]{7,40}$ ]]
}

resolve_release_tag_from_head() {
  local selected_tag="" tag=""

  while IFS= read -r tag; do
    if [[ "${tag}" =~ ^sundial-node-[0-9]{8}T[0-9]{6}Z-[0-9a-f]{7,40}$ ]]; then
      if [[ -z "${selected_tag}" || "${tag}" > "${selected_tag}" ]]; then
        selected_tag="${tag}"
      fi
    fi
  done < <(git tag --points-at HEAD --list "sundial-node-*" || true)

  [[ -n "${selected_tag}" ]] || return 1
  printf '%s' "${selected_tag}"
}

ensure_remote_release_tag_points_at_head() {
  local tag="$1" head_sha="" remote_tag_sha=""

  head_sha="$(git rev-parse HEAD)"
  remote_tag_sha="$(git ls-remote --tags origin "refs/tags/${tag}^{}" | awk 'NR==1 { print $1 }')"
  if [[ -z "${remote_tag_sha}" ]]; then
    remote_tag_sha="$(git ls-remote --tags --refs origin "refs/tags/${tag}" | awk 'NR==1 { print $1 }')"
  fi

  if [[ -z "${remote_tag_sha}" ]]; then
    fail "release tag ${tag} not found on origin; run pnpm run infra:${ENVIRONMENT}:release:tag:node"
  fi
  if [[ "${remote_tag_sha}" != "${head_sha}" ]]; then
    fail "release tag ${tag} points to ${remote_tag_sha}, expected HEAD ${head_sha}"
  fi
}

ensure_app_image_tag() {
  local tag_points_at_head=""

  if [[ -n "${IMAGE_TAG_OVERRIDE}" ]]; then
    if ! is_service_release_tag "${IMAGE_TAG_OVERRIDE}"; then
      fail "--image-tag must match sundial-node-YYYYMMDDTHHMMSSZ-<shortsha>"
    fi
    tag_points_at_head="$(git tag --points-at HEAD --list "${IMAGE_TAG_OVERRIDE}" || true)"
    if [[ "${tag_points_at_head}" != "${IMAGE_TAG_OVERRIDE}" ]]; then
      fail "--image-tag ${IMAGE_TAG_OVERRIDE} is not on HEAD; refusing deploy drift"
    fi
    APP_IMAGE_TAG="${IMAGE_TAG_OVERRIDE}"
  else
    APP_IMAGE_TAG="$(resolve_release_tag_from_head || true)"
    if [[ -z "${APP_IMAGE_TAG}" ]]; then
      fail "no release tag found on HEAD for sundial-node; run pnpm run infra:${ENVIRONMENT}:release:tag:node"
    fi
  fi

  ensure_remote_release_tag_points_at_head "${APP_IMAGE_TAG}"
}

resolve_app_image_digest() {
  local image_tag="$1" region="" repository="" digest=""

  region="$(resolve_platform_region)"
  repository="$(resolve_repository_name)"

  if [[ "${DRY_RUN}" == "true" ]]; then
    printf '%s' "sha256:0000000000000000000000000000000000000000000000000000000000000000"
    return 0
  fi

  digest="$(aws ecr describe-images \
    --repository-name "${repository}" \
    --image-ids "imageTag=${image_tag}" \
    --region "${region}" \
    --query 'imageDetails[0].imageDigest' \
    --output text)"

  if [[ -z "${digest}" || "${digest}" == "None" || "${digest}" == "null" ]]; then
    fail "unable to resolve image digest for tag=${image_tag}"
  fi

  printf '%s' "${digest}"
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
    data-tier)
      printf '%s\n' \
        "aws_db_subnet_group.main" \
        "aws_db_parameter_group.main" \
        "aws_db_instance.main" \
        "aws_elasticache_subnet_group.redis" \
        "aws_elasticache_cluster.redis"
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
    exec bash ./scripts/infra/aws.sh output --module=platform --backend-config=backends/${ENVIRONMENT}.hcl "${PASSTHROUGH_ARGS[@]}"
    ;;
  validate)
    exec bash ./scripts/infra/aws.sh validate --module=platform "${PASSTHROUGH_ARGS[@]}"
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
  ensure_deploy_source_branch
  ensure_app_image_tag
  ECR_ARGS+=("--image-tag" "${APP_IMAGE_TAG}")
  run_cmd bash ./scripts/infra/aws-bootstrap.sh ecr --environment="${ENVIRONMENT}" "${ECR_ARGS[@]}"
  _digest="$(resolve_app_image_digest "${APP_IMAGE_TAG}")"
  APP_IMAGE="$(resolve_account_id).dkr.ecr.$(resolve_platform_region).amazonaws.com/$(resolve_repository_name)@${_digest}"
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

log "action=apply environment=${ENVIRONMENT} service=${SERVICE:-platform} image_tag=${APP_IMAGE_TAG:-n/a} dry_run=${DRY_RUN}"
run_cmd bash ./scripts/infra/aws.sh apply --module=platform --backend-config=backends/${ENVIRONMENT}.hcl -auto-approve "${args[@]}"
log "completed action=apply environment=${ENVIRONMENT} service=${SERVICE:-platform}"
