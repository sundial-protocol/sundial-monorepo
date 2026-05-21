#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
# shellcheck disable=SC1091
source "${ROOT_DIR}/scripts/infra/terraform-tool.sh"

ACTION="${1:-check}"
INFRA_BIN="$(pick_infra_bin)"
INFRA_DIR="${ROOT_DIR}/infra/aws/terraform/platform"

case "${ACTION}" in
  check)
    exec "${INFRA_BIN}" -chdir="${INFRA_DIR}" fmt -check -recursive
    ;;
  fix)
    exec "${INFRA_BIN}" -chdir="${INFRA_DIR}" fmt -recursive
    ;;
  *)
    echo "Usage: ./scripts/infra/aws-fmt.sh <check|fix>" >&2
    exit 1
    ;;
esac
