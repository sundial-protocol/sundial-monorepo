#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
AWS_SESSION_ENV_FILE="${AWS_SESSION_ENV_FILE:-${XDG_CACHE_HOME:-${HOME}/.cache}/sundial-node/aws-session.env}"
# shellcheck disable=SC1091
source "${ROOT_DIR}/scripts/infra/terraform-tool.sh"

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

usage() {
  cat <<'USAGE' >&2
Usage: ./scripts/infra/aws.sh <plan|apply|output|destroy|validate> [options] [extra args...]

Options:
  --module <platform>       Terraform module under infra/aws/terraform (default: platform)
  --backend-config <path>   Backend config file for terraform init
USAGE
}

ensure_init() {
  local infra_bin="$1"
  local infra_dir="$2"
  local backend_config="$3"

  if [[ -d "${infra_dir}/.terraform/providers" && -f "${infra_dir}/.terraform.lock.hcl" ]]; then
    return 0
  fi

  local -a init_args=("-input=false")
  if [[ -n "${backend_config}" ]]; then
    init_args+=("-backend-config=${backend_config}")
  else
    init_args+=("-backend=false")
  fi

  "${infra_bin}" -chdir="${infra_dir}" init "${init_args[@]}" >/dev/null
}

command_name="${1:-}"
if [[ -z "${command_name}" ]]; then
  usage
  exit 1
fi
shift || true

module="${AWS_TERRAFORM_MODULE:-platform}"
backend_config="${AWS_TERRAFORM_BACKEND_CONFIG:-}"
extra_args=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --module)
      module="$2"
      shift 2
      ;;
    --module=*)
      module="${1#*=}"
      shift
      ;;
    --backend-config)
      backend_config="$2"
      shift 2
      ;;
    --backend-config=*)
      backend_config="${1#*=}"
      shift
      ;;
    *)
      extra_args+=("$1")
      shift
      ;;
  esac
done

has_lock_flag=false
for arg in "${extra_args[@]}"; do
  case "${arg}" in
    -lock|-lock=*)
      has_lock_flag=true
      break
      ;;
  esac
done

if [[ "${module}" != "platform" ]]; then
  echo "Unsupported module: ${module}. Use platform." >&2
  exit 1
fi

infra_dir="${ROOT_DIR}/infra/aws/terraform/${module}"
if [[ ! -d "${infra_dir}" ]]; then
  echo "Terraform module directory not found: ${infra_dir}" >&2
  exit 1
fi

infra_bin="$(pick_infra_bin)"
load_cached_aws_session_env
export AWS_SDK_LOAD_CONFIG="${AWS_SDK_LOAD_CONFIG:-1}"

case "${command_name}" in
  plan|apply|output|destroy|validate)
    ensure_init "${infra_bin}" "${infra_dir}" "${backend_config}"
    if [[ "${command_name}" == "plan" && "${has_lock_flag}" != "true" ]]; then
      extra_args+=("-lock=false")
    fi
    exec "${infra_bin}" -chdir="${infra_dir}" "${command_name}" "${extra_args[@]}"
    ;;
  *)
    usage
    exit 1
    ;;
esac
