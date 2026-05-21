#!/usr/bin/env bash

pick_infra_bin() {
  if [[ -n "${TOFU_BIN:-}" ]]; then
    printf '%s\n' "${TOFU_BIN}"
    return 0
  fi

  if command -v tofu >/dev/null 2>&1; then
    printf '%s\n' "tofu"
    return 0
  fi

  if command -v terraform >/dev/null 2>&1; then
    printf '%s\n' "terraform"
    return 0
  fi

  echo "OpenTofu or Terraform is required. Install 'tofu' (preferred) or 'terraform' to use infra/aws/terraform." >&2
  return 1
}
