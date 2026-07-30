#!/usr/bin/env bash
set -euo pipefail

CACHE_DIR_DEFAULT="${XDG_CACHE_HOME:-${HOME}/.cache}/sundial-node"
SESSION_ENV_FILE="${AWS_SESSION_ENV_FILE:-${CACHE_DIR_DEFAULT}/aws-session.env}"

die() {
  echo "[aws-refresh-credentials] error: $*" >&2
  exit 1
}

usage() {
  cat <<'USAGE' >&2
Usage: ./scripts/infra/aws-refresh-credentials.sh [options]

Options:
  --duration-seconds <n>   Request a session token valid for <n> seconds (e.g. 3600
                           for 1 hour). Requires IAM user long-term credentials in the
                           active profile. Does NOT work with SSO or assumed-role sessions
                           — for those, re-authenticate via "aws sso login" first and omit
                           this flag.
  --help                   Show this message

Without --duration-seconds the script exports whatever credentials the active AWS
profile currently holds (the default and recommended path for most auth setups).
USAGE
}

require_command() {
  local command_name="$1"
  if ! command -v "${command_name}" >/dev/null 2>&1; then
    die "required command not found: ${command_name}"
  fi
}

profile="${AWS_PROFILE:-default}"
DURATION_SECONDS="${AWS_CREDENTIALS_DURATION_SECONDS:-}"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --duration-seconds=*)
      DURATION_SECONDS="${1#*=}"
      ;;
    --duration-seconds)
      shift
      DURATION_SECONDS="${1:-}"
      [[ -n "${DURATION_SECONDS}" ]] || die "--duration-seconds requires a value"
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

require_command aws

if [[ -n "${DURATION_SECONDS}" ]]; then
  if ! [[ "${DURATION_SECONDS}" =~ ^[0-9]+$ ]]; then
    die "--duration-seconds must be a positive integer"
  fi
  # aws sts get-session-token only works with long-term IAM user credentials.
  # It fails with "Cannot call GetSessionToken with session credentials" when the
  # active profile uses SSO or an assumed role — re-authenticate via
  # "aws sso login --profile ${profile}" in that case and omit this flag.
  IFS=$'\t' read -r AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY AWS_SESSION_TOKEN < <(
    aws sts get-session-token \
      --duration-seconds "${DURATION_SECONDS}" \
      --query '[Credentials.AccessKeyId, Credentials.SecretAccessKey, Credentials.SessionToken]' \
      --output text
  )
  unset AWS_PROFILE AWS_DEFAULT_PROFILE
else
  credential_env="$(aws configure export-credentials --profile "${profile}" --format env)"

  if [[ -z "${credential_env}" ]]; then
    die "aws configure export-credentials returned empty output"
  fi

  eval "${credential_env}"
  unset AWS_PROFILE AWS_DEFAULT_PROFILE
fi

if [[ -z "${AWS_ACCESS_KEY_ID:-}" ]]; then
  die "refresh failed: AWS_ACCESS_KEY_ID is empty"
fi

if [[ -z "${AWS_SECRET_ACCESS_KEY:-}" ]]; then
  die "refresh failed: AWS_SECRET_ACCESS_KEY is empty"
fi

if [[ -z "${AWS_SESSION_TOKEN:-}" ]]; then
  die "refresh failed: AWS_SESSION_TOKEN is empty"
fi

mkdir -p "$(dirname "${SESSION_ENV_FILE}")"
chmod 700 "$(dirname "${SESSION_ENV_FILE}")"

{
  printf 'export AWS_ACCESS_KEY_ID=%q\n' "${AWS_ACCESS_KEY_ID}"
  printf 'export AWS_SECRET_ACCESS_KEY=%q\n' "${AWS_SECRET_ACCESS_KEY}"
  printf 'export AWS_SESSION_TOKEN=%q\n' "${AWS_SESSION_TOKEN}"
  printf 'export AWS_SDK_LOAD_CONFIG=%q\n' "${AWS_SDK_LOAD_CONFIG:-1}"
} >"${SESSION_ENV_FILE}"
chmod 600 "${SESSION_ENV_FILE}"

identity_json="$(aws sts get-caller-identity --output json)"
printf '%s\n' "${identity_json}"
printf '[aws-refresh-credentials] credentials_refreshed=true profile=%s\n' "${profile}"
printf '[aws-refresh-credentials] session_env_file=%s\n' "${SESSION_ENV_FILE}"
printf '[aws-refresh-credentials] note=infra scripts auto-load this cache when AWS env vars are unset\n'
printf '[aws-refresh-credentials] to load into current shell: source %s\n' "${SESSION_ENV_FILE}"
