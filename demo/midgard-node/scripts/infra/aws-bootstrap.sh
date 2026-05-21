#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
DEMO_DIR="$(cd "${ROOT_DIR}/.." && pwd)"
cd "${ROOT_DIR}"

AWS_SESSION_ENV_FILE="${AWS_SESSION_ENV_FILE:-${XDG_CACHE_HOME:-${HOME}/.cache}/sundial-node/aws-session.env}"
AWS_STATE_REGION="${AWS_STATE_REGION:-us-east-1}"
AWS_TF_STATE_BUCKET="${AWS_TF_STATE_BUCKET:-sndl-sundial-node-tf-state}"
AWS_TF_LOCK_TABLE="${AWS_TF_LOCK_TABLE:-sndl-sundial-node-tf-locks}"
AWS_ECR_SUNDIAL_NODE_REPOSITORY="${AWS_ECR_SUNDIAL_NODE_REPOSITORY:-sundial/sundial-node}"
ECR_IMAGE_TAG_MUTABILITY="${ECR_IMAGE_TAG_MUTABILITY:-IMMUTABLE_WITH_EXCLUSION}"
ECR_IMAGE_TAG_MUTABILITY_EXCLUSION_FILTER="${ECR_IMAGE_TAG_MUTABILITY_EXCLUSION_FILTER:-*-latest}"
ENVIRONMENT="testnet"
ECR_SERVICE="sundial-node"
DRY_RUN=false
UPDATE_EXISTING_SECRETS=false
IMAGE_TAG=""
IMAGE_LATEST_TAG=""
ENV_FILES=()

log() {
  printf '[aws-bootstrap] %s\n' "$*" >&2
}

die() {
  printf '[aws-bootstrap] error: %s\n' "$*" >&2
  exit 1
}

usage() {
  cat <<'USAGE' >&2
Usage: ./scripts/infra/aws-bootstrap.sh <state|ecr|secrets> [options]

Commands:
  state      Ensure Terraform S3 backend + DynamoDB lock table exist
  ecr        Ensure ECR repository exists and push the sundial-node image
  secrets    Ensure bootstrap Secrets Manager entries exist

Options:
  --environment=<testnet|mainnet>
                                Deployment environment (default: testnet)
  --dry-run                     Print commands without running them
  --update-existing-secrets     Update values for secrets that already exist
  --image-tag <tag>             Override image tag (default: <environment>-<utc-timestamp>)
  --image-latest-tag <tag>      Also push mutable alias tag, or "none" to disable

Environment overrides:
  DEPLOY_ENV_FILE               Deployment env source (default .env.deploy.<environment>)
  AWS_PLATFORM_REGION           App infra region (default us-west-2 for testnet, us-west-1 for mainnet)
  AWS_STATE_REGION              Terraform backend region (default us-east-1)
  AWS_TF_STATE_BUCKET           State bucket name (default sndl-sundial-node-tf-state)
  AWS_TF_LOCK_TABLE             Lock table name (default sndl-sundial-node-tf-locks)
  AWS_ECR_SUNDIAL_NODE_REPOSITORY
                                  ECR repo (default sundial/sundial-node)
USAGE
}

default_platform_region_for_environment() {
  case "$1" in
    testnet) printf '%s' "us-west-2" ;;
    mainnet) printf '%s' "us-west-1" ;;
    *) die "unsupported environment for region mapping: $1" ;;
  esac
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

require_command() {
  command -v "$1" >/dev/null 2>&1 || die "required command not found: $1"
}

run_cmd() {
  if [[ "${DRY_RUN}" == "true" ]]; then
    printf '[aws-bootstrap] [dry-run] ' >&2
    printf '%q ' "$@" >&2
    printf '\n' >&2
    return 0
  fi
  "$@"
}

read_env_file_value() {
  local file="$1"
  local key="$2"
  local line=""

  [[ -f "${file}" ]] || return 1
  line="$(grep -E "^(export[[:space:]]+)?${key}=" "${file}" | tail -n1 || true)"
  [[ -n "${line}" ]] || return 1
  local value="${line#*=}"
  value="${value%$'\r'}"
  value="${value#\"}"
  value="${value%\"}"
  printf '%s' "${value}"
}

resolve_env_value() {
  local key="$1"
  local value=""

  if [[ -n "${!key:-}" ]]; then
    printf '%s' "${!key}"
    return 0
  fi

  for file in "${ENV_FILES[@]}"; do
    value="$(read_env_file_value "${file}" "${key}" || true)"
    if [[ -n "${value}" ]]; then
      printf '%s' "${value}"
      return 0
    fi
  done
  return 1
}

register_env_file() {
  if [[ -f "$1" ]]; then
    ENV_FILES+=("$1")
  fi
}

prepare_env_sources() {
  DEPLOY_ENV_FILE="${DEPLOY_ENV_FILE:-.env.deploy.${ENVIRONMENT}}"
  register_env_file "${DEPLOY_ENV_FILE}"
  register_env_file ".env"
  if [[ ${#ENV_FILES[@]} -gt 0 ]]; then
    log "loaded env sources: ${ENV_FILES[*]}"
  else
    log "no env source files found; relying only on process env vars"
  fi
}

resolve_account_id() {
  if [[ -n "${AWS_ACCOUNT_ID:-}" ]]; then
    printf '%s' "${AWS_ACCOUNT_ID}"
    return 0
  fi
  if [[ "${DRY_RUN}" == "true" ]]; then
    printf '%s' "000000000000"
    return 0
  fi
  aws sts get-caller-identity --query Account --output text
}

repository_exists() {
  aws ecr describe-repositories \
    --repository-names "$1" \
    --region "${AWS_PLATFORM_REGION}" \
    >/dev/null 2>&1
}

configure_ecr_tag_mutability_policy() {
  run_cmd aws ecr put-image-tag-mutability \
    --repository-name "$1" \
    --image-tag-mutability "${ECR_IMAGE_TAG_MUTABILITY}" \
    --image-tag-mutability-exclusion-filters "filterType=WILDCARD,filter=${ECR_IMAGE_TAG_MUTABILITY_EXCLUSION_FILTER}" \
    --region "${AWS_PLATFORM_REGION}"
}

ensure_ecr_repository() {
  if repository_exists "$1"; then
    log "ecr repository already exists: $1"
    configure_ecr_tag_mutability_policy "$1"
    return 0
  fi

  run_cmd aws ecr create-repository \
    --repository-name "$1" \
    --image-tag-mutability "${ECR_IMAGE_TAG_MUTABILITY}" \
    --image-tag-mutability-exclusion-filters "filterType=WILDCARD,filter=${ECR_IMAGE_TAG_MUTABILITY_EXCLUSION_FILTER}" \
    --image-scanning-configuration scanOnPush=true \
    --region "${AWS_PLATFORM_REGION}"
}

resolve_repository_uri() {
  local account_id="$2"
  if [[ "${DRY_RUN}" == "true" ]]; then
    printf '%s.dkr.ecr.%s.amazonaws.com/%s' "${account_id}" "${AWS_PLATFORM_REGION}" "$1"
    return 0
  fi

  aws ecr describe-repositories \
    --repository-names "$1" \
    --region "${AWS_PLATFORM_REGION}" \
    --query 'repositories[0].repositoryUri' \
    --output text
}

login_to_ecr() {
  local registry="$1.dkr.ecr.${AWS_PLATFORM_REGION}.amazonaws.com"
  if [[ "${DRY_RUN}" == "true" ]]; then
    log "[dry-run] aws ecr get-login-password --region ${AWS_PLATFORM_REGION} | docker login --username AWS --password-stdin ${registry}"
    return 0
  fi
  aws ecr get-login-password --region "${AWS_PLATFORM_REGION}" | docker login --username AWS --password-stdin "${registry}" >/dev/null
}

build_and_push_image() {
  local repository_uri="$1"
  local dockerfile_path="${ROOT_DIR}/Dockerfile"
  local image_ref="${repository_uri}:${IMAGE_TAG}"
  local latest_ref=""
  [[ -n "${IMAGE_LATEST_TAG}" ]] && latest_ref="${repository_uri}:${IMAGE_LATEST_TAG}"

  if [[ "${DRY_RUN}" == "true" ]]; then
    log "[dry-run] docker build --platform linux/amd64 --file ${dockerfile_path} --tag ${image_ref} ${DEMO_DIR}"
    log "[dry-run] docker push ${image_ref}"
    [[ -n "${latest_ref}" ]] && log "[dry-run] docker tag ${image_ref} ${latest_ref} && docker push ${latest_ref}"
    return 0
  fi

  docker build \
    --platform linux/amd64 \
    --file "${dockerfile_path}" \
    --tag "${image_ref}" \
    "${DEMO_DIR}"
  docker push "${image_ref}"
  log "pushed image: ${image_ref}"

  if [[ -n "${latest_ref}" ]]; then
    docker tag "${image_ref}" "${latest_ref}"
    docker push "${latest_ref}"
    log "pushed image alias: ${latest_ref}"
  fi
}

ensure_ecr_and_image() {
  require_command aws
  require_command docker
  local account_id=""
  local repository_uri=""
  account_id="$(resolve_account_id)"
  ensure_ecr_repository "${AWS_ECR_SUNDIAL_NODE_REPOSITORY}"
  repository_uri="$(resolve_repository_uri "${AWS_ECR_SUNDIAL_NODE_REPOSITORY}" "${account_id}")"
  login_to_ecr "${account_id}"
  build_and_push_image "${repository_uri}"
  log "sundial-node image URI: ${repository_uri}:${IMAGE_TAG}"
}

is_placeholder_value() {
  case "$1" in
    ""|*replace_me*|*changeme*|*todo*|*TODO*|*bootstrap.invalid*)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

resolve_secret_value() {
  case "$1" in
    rds-master-password)
      resolve_env_value RDS_MASTER_PASSWORD
      ;;
    l1-provider)
      resolve_env_value L1_PROVIDER
      ;;
    l1-blockfrost-api-url)
      resolve_env_value L1_BLOCKFROST_API_URL
      ;;
    l1-blockfrost-key)
      resolve_env_value L1_BLOCKFROST_KEY
      ;;
    l1-ogmios-key)
      resolve_env_value L1_OGMIOS_KEY
      ;;
    l1-kupo-key)
      resolve_env_value L1_KUPO_KEY
      ;;
    operator-seed-phrase)
      resolve_env_value L1_OPERATOR_SEED_PHRASE
      ;;
    operator-seed-phrase-block-commitment)
      resolve_env_value L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT
      ;;
    operator-seed-phrase-merge)
      resolve_env_value L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX
      ;;
    grafana-admin-password)
      resolve_env_value GRAFANA_ADMIN_PASSWORD
      ;;
    testnet-genesis-wallet-seed-phrase-a)
      resolve_env_value TESTNET_GENESIS_WALLET_SEED_PHRASE_A
      ;;
    testnet-genesis-wallet-seed-phrase-b)
      resolve_env_value TESTNET_GENESIS_WALLET_SEED_PHRASE_B
      ;;
    testnet-genesis-wallet-seed-phrase-c)
      resolve_env_value TESTNET_GENESIS_WALLET_SEED_PHRASE_C
      ;;
    *)
      return 1
      ;;
  esac
}

secret_exists() {
  [[ "${DRY_RUN}" == "true" ]] && return 1
  aws secretsmanager describe-secret \
    --secret-id "$1" \
    --region "${AWS_PLATFORM_REGION}" \
    >/dev/null 2>&1
}

ensure_secret() {
  local secret_name="$1"
  local secret_value="$2"

  if secret_exists "${secret_name}"; then
    if [[ "${UPDATE_EXISTING_SECRETS}" == "true" ]]; then
      run_cmd aws secretsmanager put-secret-value \
        --secret-id "${secret_name}" \
        --secret-string "${secret_value}" \
        --region "${AWS_PLATFORM_REGION}" \
        >/dev/null
      log "updated existing secret: ${secret_name}"
      return 0
    fi
    log "secret already exists (skipped): ${secret_name}"
    return 0
  fi

  run_cmd aws secretsmanager create-secret \
    --name "${secret_name}" \
    --secret-string "${secret_value}" \
    --region "${AWS_PLATFORM_REGION}" \
    >/dev/null
  log "created secret: ${secret_name}"
}

ensure_secrets() {
  require_command aws
  local secret_name=""
  local secret_value=""
  local -a secret_names=(
    rds-master-password
    l1-provider
    l1-blockfrost-api-url
    l1-blockfrost-key
    l1-ogmios-key
    l1-kupo-key
    operator-seed-phrase
    operator-seed-phrase-block-commitment
    operator-seed-phrase-merge
    grafana-admin-password
    testnet-genesis-wallet-seed-phrase-a
    testnet-genesis-wallet-seed-phrase-b
    testnet-genesis-wallet-seed-phrase-c
  )

  for secret_name in "${secret_names[@]}"; do
    secret_value="$(resolve_secret_value "${secret_name}" || true)"
    if is_placeholder_value "${secret_value}"; then
      die "missing or placeholder value for secret ${secret_name}; set it via env var or ${DEPLOY_ENV_FILE}"
    fi
    ensure_secret "sundial-node/${ENVIRONMENT}/${secret_name}" "${secret_value}"
  done
}

ensure_state_bucket() {
  if [[ "${DRY_RUN}" != "true" ]] && aws s3api head-bucket --bucket "${AWS_TF_STATE_BUCKET}" >/dev/null 2>&1; then
    log "state bucket already exists: ${AWS_TF_STATE_BUCKET}"
  elif [[ "${AWS_STATE_REGION}" == "us-east-1" ]]; then
    run_cmd aws s3api create-bucket --bucket "${AWS_TF_STATE_BUCKET}" --region "${AWS_STATE_REGION}" >/dev/null
  else
    run_cmd aws s3api create-bucket \
      --bucket "${AWS_TF_STATE_BUCKET}" \
      --region "${AWS_STATE_REGION}" \
      --create-bucket-configuration "LocationConstraint=${AWS_STATE_REGION}" \
      >/dev/null
  fi

  run_cmd aws s3api put-public-access-block \
    --bucket "${AWS_TF_STATE_BUCKET}" \
    --region "${AWS_STATE_REGION}" \
    --public-access-block-configuration "BlockPublicAcls=true,IgnorePublicAcls=true,BlockPublicPolicy=true,RestrictPublicBuckets=true" \
    >/dev/null
  run_cmd aws s3api put-bucket-versioning \
    --bucket "${AWS_TF_STATE_BUCKET}" \
    --region "${AWS_STATE_REGION}" \
    --versioning-configuration Status=Enabled \
    >/dev/null
  run_cmd aws s3api put-bucket-encryption \
    --bucket "${AWS_TF_STATE_BUCKET}" \
    --region "${AWS_STATE_REGION}" \
    --server-side-encryption-configuration '{"Rules":[{"ApplyServerSideEncryptionByDefault":{"SSEAlgorithm":"AES256"}}]}' \
    >/dev/null
}

ensure_lock_table() {
  if [[ "${DRY_RUN}" != "true" ]] && aws dynamodb describe-table --table-name "${AWS_TF_LOCK_TABLE}" --region "${AWS_STATE_REGION}" >/dev/null 2>&1; then
    log "lock table already exists: ${AWS_TF_LOCK_TABLE}"
    return 0
  fi

  run_cmd aws dynamodb create-table \
    --table-name "${AWS_TF_LOCK_TABLE}" \
    --region "${AWS_STATE_REGION}" \
    --attribute-definitions AttributeName=LockID,AttributeType=S \
    --key-schema AttributeName=LockID,KeyType=HASH \
    --billing-mode PAY_PER_REQUEST \
    >/dev/null
  run_cmd aws dynamodb wait table-exists --table-name "${AWS_TF_LOCK_TABLE}" --region "${AWS_STATE_REGION}"
}

ACTION="${1:-}"
[[ -n "${ACTION}" ]] || {
  usage
  exit 1
}
shift || true

while [[ $# -gt 0 ]]; do
  case "$1" in
    --environment=*)
      ENVIRONMENT="${1#*=}"
      ;;
    --environment)
      shift
      ENVIRONMENT="${1:-}"
      [[ -n "${ENVIRONMENT}" ]] || die "--environment requires a value"
      ;;
    --dry-run)
      DRY_RUN=true
      ;;
    --update-existing-secrets)
      UPDATE_EXISTING_SECRETS=true
      ;;
    --image-tag=*)
      IMAGE_TAG="${1#*=}"
      ;;
    --image-tag)
      shift
      IMAGE_TAG="${1:-}"
      [[ -n "${IMAGE_TAG}" ]] || die "--image-tag requires a value"
      ;;
    --image-latest-tag=*)
      IMAGE_LATEST_TAG="${1#*=}"
      ;;
    --image-latest-tag)
      shift
      IMAGE_LATEST_TAG="${1:-}"
      [[ -n "${IMAGE_LATEST_TAG}" ]] || die "--image-latest-tag requires a value"
      ;;
    --service=*)
      ECR_SERVICE="${1#*=}"
      [[ "${ECR_SERVICE}" == "sundial-node" ]] || die "--service must be sundial-node"
      ;;
    --service)
      shift
      ECR_SERVICE="${1:-}"
      [[ "${ECR_SERVICE}" == "sundial-node" ]] || die "--service must be sundial-node"
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      die "unknown argument: $1"
      ;;
  esac
  shift
done

case "${ACTION}" in
  state|ecr|secrets) ;;
  *)
    usage
    exit 1
    ;;
esac

case "${ENVIRONMENT}" in
  testnet|mainnet) ;;
  *)
    die "--environment must be one of: testnet mainnet"
    ;;
esac

AWS_PLATFORM_REGION="${AWS_PLATFORM_REGION:-$(default_platform_region_for_environment "${ENVIRONMENT}")}"
IMAGE_TAG="${IMAGE_TAG:-${ENVIRONMENT}-$(date -u +%Y%m%d%H%M%S)}"
IMAGE_LATEST_TAG="${IMAGE_LATEST_TAG:-${ENVIRONMENT}-latest}"
[[ "${IMAGE_LATEST_TAG}" == "none" ]] && IMAGE_LATEST_TAG=""

prepare_env_sources
load_cached_aws_session_env
export AWS_SDK_LOAD_CONFIG="${AWS_SDK_LOAD_CONFIG:-1}"

log "action=${ACTION} environment=${ENVIRONMENT} platform_region=${AWS_PLATFORM_REGION} image_tag=${IMAGE_TAG} image_latest_tag=${IMAGE_LATEST_TAG:-disabled}"

case "${ACTION}" in
  state)
    require_command aws
    ensure_state_bucket
    ensure_lock_table
    ;;
  ecr)
    ensure_ecr_and_image
    ;;
  secrets)
    ensure_secrets
    ;;
esac

log "completed action=${ACTION}"
