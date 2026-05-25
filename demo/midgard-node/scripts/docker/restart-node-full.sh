#!/usr/bin/env bash
set -euo pipefail

# Ensure we run from the midgard-node package root regardless of caller cwd.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MIDGARD_NODE_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"
cd "${MIDGARD_NODE_DIR}"

PROJECT_LABEL="com.docker.compose.project=sundial"
SERVICE_LABEL="com.docker.compose.service=node"

# Drop existing node container so its image becomes prune-eligible.
EXISTING_NODE_IDS="$(docker compose ps -q node || true)"
docker compose rm -sf node || true

# Docker can report "removal ... is already in progress" for a short window.
# Wait until previously known node container IDs are fully removed.
if [[ -n "${EXISTING_NODE_IDS}" ]]; then
  for container_id in ${EXISTING_NODE_IDS}; do
    for _ in $(seq 1 120); do
      if ! docker inspect "${container_id}" >/dev/null 2>&1; then
        break
      fi
      sleep 0.5
    done
  done
fi

# Remove any unused previously built node images from this compose project.
docker image prune -af \
  --filter "label=${PROJECT_LABEL}" \
  --filter "label=${SERVICE_LABEL}"

# Rebuild and recreate node from scratch.
for attempt in 1 2 3; do
  ATTEMPT_LOG_FILE="$(mktemp)"
  if docker compose up -d --no-deps --build --force-recreate node 2>&1 | tee "${ATTEMPT_LOG_FILE}"; then
    rm -f "${ATTEMPT_LOG_FILE}"
    exit 0
  fi

  if grep -q "removal of container .* is already in progress" "${ATTEMPT_LOG_FILE}"; then
    rm -f "${ATTEMPT_LOG_FILE}"
    sleep 2
    continue
  fi

  rm -f "${ATTEMPT_LOG_FILE}"
  exit 1
done

echo "docker compose up failed repeatedly due to container removal still in progress" >&2
exit 1
