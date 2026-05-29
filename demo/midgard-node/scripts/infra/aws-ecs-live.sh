#!/usr/bin/env bash
set -euo pipefail

AWS_SESSION_ENV_FILE="${AWS_SESSION_ENV_FILE:-${XDG_CACHE_HOME:-${HOME}/.cache}/sundial-node/aws-session.env}"
NAMESPACE="sundial-node.local"

usage() {
  cat <<'USAGE'
Usage: ./scripts/infra/aws-ecs-live.sh [options]

Options:
  --environment=<testnet>   Deployment environment (default: testnet)
  --region=<aws-region>     AWS region (default: us-west-2 for testnet)
  --service=<name|all>      Service name or "all"
                            ECS:          sundial-node, prometheus, loki, alloy, grafana, postgres-exporter
                            ASG:          ecs-host
                            EC2:          nat
                            ElastiCache:  redis
                            Default: all
  --help                    Show this message

Examples:
  npm run infra:testnet:live
  bash ./scripts/infra/aws-ecs-live.sh --environment=testnet --service=sundial-node
  bash ./scripts/infra/aws-ecs-live.sh --environment=testnet --service=grafana
USAGE
}

log() {
  printf '[aws-ecs-live] %s\n' "$*" >&2
}

fail() {
  printf '[aws-ecs-live] error: %s\n' "$*" >&2
  exit 1
}

require_cmd() {
  local cmd="$1"
  command -v "${cmd}" >/dev/null 2>&1 || fail "required command not found: ${cmd}"
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

normalize_count() {
  local value="$1"
  if [[ "${value}" =~ ^[0-9]+$ ]]; then
    printf '%s' "${value}"
  else
    printf '0'
  fi
}

ENVIRONMENT="testnet"
REGION=""
SERVICE="all"

for arg in "$@"; do
  case "${arg}" in
    --environment=*)
      ENVIRONMENT="${arg#*=}"
      ;;
    --region=*)
      REGION="${arg#*=}"
      ;;
    --service=*)
      SERVICE="${arg#*=}"
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      fail "unknown argument: ${arg} (use --help)"
      ;;
  esac
done

case "${ENVIRONMENT}" in
  testnet) ;;
  *)
    fail "--environment must be testnet"
    ;;
esac

if [[ -z "${REGION}" ]]; then
  REGION="us-west-2"
fi

VALID_SERVICES=(sundial-node prometheus loki alloy grafana postgres-exporter ecs-host nat redis all)
is_valid_service=0
for candidate in "${VALID_SERVICES[@]}"; do
  if [[ "${candidate}" == "${SERVICE}" ]]; then
    is_valid_service=1
    break
  fi
done
[[ "${is_valid_service}" -eq 1 ]] || fail "--service must be one of: ${VALID_SERVICES[*]}"

ALL_SERVICES=(sundial-node prometheus loki alloy grafana postgres-exporter ecs-host nat redis)

SERVICES_TO_CHECK=()
if [[ "${SERVICE}" == "all" ]]; then
  SERVICES_TO_CHECK=("${ALL_SERVICES[@]}")
else
  SERVICES_TO_CHECK=("${SERVICE}")
fi

require_cmd aws
load_cached_aws_session_env
export AWS_SDK_LOAD_CONFIG="${AWS_SDK_LOAD_CONFIG:-1}"

CLUSTER_NAME="sundial-node-${ENVIRONMENT}"
NAME_PREFIX="sundial-node-${ENVIRONMENT}"
log "environment=${ENVIRONMENT} region=${REGION} services=${SERVICE}"

failing_count=0

# ─── SSM health probe setup ───────────────────────────────────────────────────
#
# We send one batched SSM command to an ECS host that curls all internal
# service health endpoints simultaneously. This avoids one round-trip per
# service and keeps the check fast.

# Cloud Map with routing_policy=MULTIVALUE creates only SRV records (no A records),
# so curl cannot resolve these hostnames. Internal services are validated by ECS
# running count only (http=skipped). sundial-node is validated via ALB target group.
declare -A SERVICE_HEALTH_URL=()

ECS_HOST_INSTANCE_ID=""
SSM_BATCH_CMD_ID=""
# Associative array filled after SSM poll: service_name -> ok|fail|skip
declare -A SSM_HEALTH_RESULT=()

resolve_ecs_host() {
  local raw
  raw="$(
    aws autoscaling describe-auto-scaling-groups \
      --region "${REGION}" \
      --auto-scaling-group-names "${NAME_PREFIX}-ecs-asg" \
      --query 'AutoScalingGroups[0].Instances[?LifecycleState==`InService`] | [0].InstanceId' \
      --output text 2>/dev/null
  )" || true
  [[ "${raw}" == "None" || -z "${raw}" ]] && return 0
  ECS_HOST_INSTANCE_ID="${raw}"
}

start_ssm_health_batch() {
  [[ -z "${ECS_HOST_INSTANCE_ID}" ]] && return 0

  # Build an inline shell that curls each requested ECS service and prints
  # "<name>=ok" or "<name>=fail" lines.
  local probe_lines=""
  for svc in "${SERVICES_TO_CHECK[@]}"; do
    local url="${SERVICE_HEALTH_URL[${svc}]:-}"
    [[ -z "${url}" ]] && continue
    probe_lines+="if curl -sf --max-time 5 '${url}' >/dev/null 2>&1; then echo '${svc}=ok'; else echo '${svc}=fail'; fi; "
  done
  [[ -z "${probe_lines}" ]] && return 0

  SSM_BATCH_CMD_ID="$(
    aws ssm send-command \
      --region "${REGION}" \
      --instance-ids "${ECS_HOST_INSTANCE_ID}" \
      --document-name "AWS-RunShellScript" \
      --parameters "commands=[\"${probe_lines}\"]" \
      --query 'Command.CommandId' \
      --output text 2>/dev/null
  )" || SSM_BATCH_CMD_ID=""
}

poll_ssm_health_batch() {
  [[ -z "${ECS_HOST_INSTANCE_ID}" || -z "${SSM_BATCH_CMD_ID}" ]] && return 0

  local attempt=0
  local raw_output=""
  while [[ "${attempt}" -lt 15 ]]; do
    sleep 2
    local invocation_raw
    invocation_raw="$(
      aws ssm get-command-invocation \
        --region "${REGION}" \
        --command-id "${SSM_BATCH_CMD_ID}" \
        --instance-id "${ECS_HOST_INSTANCE_ID}" \
        --query '[Status,StandardOutputContent]' \
        --output text 2>/dev/null
    )" || break

    local cmd_status cmd_output
    IFS=$'\t' read -r cmd_status cmd_output <<<"${invocation_raw}"

    if [[ "${cmd_status}" == "Success" ]]; then
      raw_output="${cmd_output}"
      break
    elif [[ "${cmd_status}" == "Failed" || "${cmd_status}" == "Cancelled" || "${cmd_status}" == "TimedOut" ]]; then
      break
    fi
    attempt=$((attempt + 1))
  done

  # Parse "<name>=ok|fail" lines into the associative array
  while IFS= read -r line; do
    local svc="${line%%=*}"
    local result="${line#*=}"
    result="${result//[[:space:]]/}"
    [[ -n "${svc}" ]] && SSM_HEALTH_RESULT["${svc}"]="${result}"
  done <<<"${raw_output}"
}

# ─── check functions ──────────────────────────────────────────────────────────

check_ecs_service_counts() {
  local service_name="$1"

  local raw
  raw="$(
    aws ecs describe-services \
      --cluster "${CLUSTER_NAME}" \
      --services "${service_name}" \
      --region "${REGION}" \
      --query '[length(services), services[0].status, services[0].desiredCount, services[0].runningCount, services[0].pendingCount, failures[0].reason]' \
      --output text
  )"

  IFS=$'\t' read -r found_count service_status desired_count running_count pending_count failure_reason <<<"${raw}"

  desired_count="$(normalize_count "${desired_count}")"
  running_count="$(normalize_count "${running_count}")"
  pending_count="$(normalize_count "${pending_count}")"

  local ecs_state="live" reason="ok"

  if [[ "${found_count}" != "1" ]]; then
    ecs_state="failing"
    if [[ -n "${failure_reason}" && "${failure_reason}" != "None" ]]; then
      reason="${failure_reason}"
    else
      reason="service_not_found"
    fi
  elif [[ "${service_status}" != "ACTIVE" ]]; then
    ecs_state="failing"
    reason="service_status_${service_status}"
  elif [[ "${running_count}" -eq 0 ]]; then
    ecs_state="failing"
    reason="running_0"
  elif [[ "${running_count}" -lt "${desired_count}" ]]; then
    ecs_state="failing"
    reason="running_lt_desired"
  fi

  printf '%s %s %s %s %s %s' \
    "${ecs_state}" "${reason}" "${running_count}" "${desired_count}" "${pending_count}" "${failure_reason:-}"
}

check_ecs_service() {
  local service_name="$1"

  local ecs_state reason running_count desired_count pending_count _fr
  read -r ecs_state reason running_count desired_count pending_count _fr \
    <<<"$(check_ecs_service_counts "${service_name}")"

  # SSM health probe result (populated by poll_ssm_health_batch)
  local http_state="skipped"
  local http_result="${SSM_HEALTH_RESULT[${service_name}]:-}"
  if [[ -n "${http_result}" ]]; then
    http_state="${http_result}"
  elif [[ -z "${ECS_HOST_INSTANCE_ID}" ]]; then
    http_state="no_host"
  elif [[ -z "${SSM_BATCH_CMD_ID}" ]]; then
    http_state="ssm_unavailable"
  fi

  local state="live"
  if [[ "${ecs_state}" != "live" || "${http_state}" == "fail" ]]; then
    state="failing"
    [[ "${ecs_state}" != "live" ]] || reason="http_probe_failed"
  fi

  printf '%-22s %-8s running=%-3s desired=%-3s pending=%-3s http=%-15s kind=ecs reason=%s\n' \
    "${service_name}:" "${state}" "${running_count}" "${desired_count}" "${pending_count}" "${http_state}" "${reason}"

  [[ "${state}" == "live" ]]
}

check_ecs_service_with_alb() {
  local service_name="$1"
  local tg_name="$2"

  local ecs_state reason running_count desired_count pending_count _fr
  read -r ecs_state reason running_count desired_count pending_count _fr \
    <<<"$(check_ecs_service_counts "${service_name}")"

  # ALB target group health — resolves TG ARN by name
  local alb_state="unknown"
  local tg_arn
  tg_arn="$(
    aws elbv2 describe-target-groups \
      --region "${REGION}" \
      --names "${tg_name}" \
      --query 'TargetGroups[0].TargetGroupArn' \
      --output text 2>/dev/null
  )" || true

  if [[ -n "${tg_arn}" && "${tg_arn}" != "None" ]]; then
    local health_raw
    health_raw="$(
      aws elbv2 describe-target-health \
        --region "${REGION}" \
        --target-group-arn "${tg_arn}" \
        --query 'TargetHealthDescriptions[*].TargetHealth.State' \
        --output text 2>/dev/null
    )" || true

    if [[ -z "${health_raw}" || "${health_raw}" == "None" ]]; then
      alb_state="no_targets"
    elif echo "${health_raw}" | grep -qw "healthy"; then
      alb_state="healthy"
    else
      alb_state="${health_raw// /_}"
    fi
  else
    alb_state="tg_not_found"
  fi

  local state="live"
  if [[ "${ecs_state}" != "live" ]]; then
    state="failing"
  elif [[ "${alb_state}" != "healthy" ]]; then
    state="failing"
    reason="alb_${alb_state}"
  fi

  printf '%-22s %-8s running=%-3s desired=%-3s pending=%-3s alb=%-15s kind=ecs reason=%s\n' \
    "${service_name}:" "${state}" "${running_count}" "${desired_count}" "${pending_count}" "${alb_state}" "${reason}"

  [[ "${state}" == "live" ]]
}

check_asg() {
  local label="$1"
  local asg_name="$2"

  local raw
  raw="$(
    aws autoscaling describe-auto-scaling-groups \
      --region "${REGION}" \
      --auto-scaling-group-names "${asg_name}" \
      --query 'AutoScalingGroups[0].[DesiredCapacity,length(Instances[?LifecycleState==`InService`])]' \
      --output text 2>/dev/null
  )"

  local desired_count="0" inservice_count="0"
  if [[ -n "${raw}" && "${raw}" != "None" ]]; then
    IFS=$'\t' read -r desired_count inservice_count <<<"${raw}"
  fi

  desired_count="$(normalize_count "${desired_count}")"
  inservice_count="$(normalize_count "${inservice_count}")"

  local state="live" reason="ok"

  if [[ "${desired_count}" == "0" && "${inservice_count}" == "0" ]]; then
    state="failing"
    reason="asg_not_found"
  elif [[ "${inservice_count}" -eq 0 ]]; then
    state="failing"
    reason="inservice_0"
  elif [[ "${inservice_count}" -lt "${desired_count}" ]]; then
    state="failing"
    reason="inservice_lt_desired"
  fi

  printf '%-22s %-8s inservice=%-3s desired=%-3s kind=asg reason=%s\n' \
    "${label}:" "${state}" "${inservice_count}" "${desired_count}" "${reason}"

  [[ "${state}" == "live" ]]
}

check_nat() {
  local label="$1"

  # Try NAT EC2 instance first (nat_type=instance)
  local ec2_raw
  ec2_raw="$(
    aws ec2 describe-instances \
      --region "${REGION}" \
      --filters "Name=tag:Name,Values=${NAME_PREFIX}-nat" "Name=instance-state-name,Values=pending,running,stopping,stopped" \
      --query 'sort_by(Reservations[].Instances[], &LaunchTime)[-1].[InstanceId,State.Name,InstanceType]' \
      --output text 2>/dev/null
  )"

  local nat_id nat_state nat_type
  IFS=$'\t' read -r nat_id nat_state nat_type <<<"${ec2_raw:-}"

  if [[ -n "${nat_id}" && "${nat_id}" != "None" ]]; then
    local state="live" reason="ok"
    if [[ "${nat_state}" != "running" ]]; then
      state="failing"
      reason="instance_state_${nat_state}"
    fi
    printf '%-22s %-8s ec2_state=%-10s type=%-14s kind=ec2_nat reason=%s\n' \
      "${label}:" "${state}" "${nat_state}" "${nat_type}" "${reason}"
    [[ "${state}" == "live" ]]
    return
  fi

  # Fall back to NAT Gateway (nat_type=gateway)
  local gw_raw
  gw_raw="$(
    aws ec2 describe-nat-gateways \
      --region "${REGION}" \
      --filter "Name=tag:Name,Values=${NAME_PREFIX}-nat-gw-*" \
      --query 'NatGateways[*].[NatGatewayId,State]' \
      --output text 2>/dev/null
  )"

  if [[ -z "${gw_raw}" ]]; then
    printf '%-22s %-8s kind=nat reason=not_found\n' "${label}:" "failing"
    return 1
  fi

  local gw_total=0 gw_available=0
  while IFS=$'\t' read -r _gw_id gw_state; do
    gw_total=$((gw_total + 1))
    [[ "${gw_state}" == "available" ]] && gw_available=$((gw_available + 1))
  done <<<"${gw_raw}"

  local state="live" reason="ok"
  if [[ "${gw_available}" -eq 0 ]]; then
    state="failing"
    reason="all_gateways_unavailable"
  elif [[ "${gw_available}" -lt "${gw_total}" ]]; then
    state="failing"
    reason="some_gateways_unavailable"
  fi

  printf '%-22s %-8s available=%-3s total=%-3s kind=nat_gw reason=%s\n' \
    "${label}:" "${state}" "${gw_available}" "${gw_total}" "${reason}"

  [[ "${state}" == "live" ]]
}

check_elasticache() {
  local label="$1"
  local cluster_id="$2"

  local raw
  raw="$(
    aws elasticache describe-cache-clusters \
      --cache-cluster-id "${cluster_id}" \
      --region "${REGION}" \
      --query 'CacheClusters[0].[CacheClusterStatus,NumCacheNodes]' \
      --output text 2>/dev/null
  )"

  local cluster_status="None" num_nodes="0"
  if [[ -n "${raw}" && "${raw}" != "None" ]]; then
    IFS=$'\t' read -r cluster_status num_nodes <<<"${raw}"
  fi

  num_nodes="$(normalize_count "${num_nodes}")"

  local state="live" reason="ok"

  if [[ -z "${cluster_status}" || "${cluster_status}" == "None" ]]; then
    state="failing"
    reason="cluster_not_found"
  elif [[ "${cluster_status}" != "available" ]]; then
    state="failing"
    reason="cluster_status_${cluster_status}"
  elif [[ "${num_nodes}" -eq 0 ]]; then
    state="failing"
    reason="nodes_0"
  fi

  printf '%-22s %-8s status=%-14s nodes=%-3s kind=elasticache reason=%s\n' \
    "${label}:" "${state}" "${cluster_status}" "${num_nodes}" "${reason}"

  [[ "${state}" == "live" ]]
}

# ─── pre-fetch: resolve ECS host and start batched SSM health probe ───────────

needs_ecs_check=0
for svc in "${SERVICES_TO_CHECK[@]}"; do
  [[ -n "${SERVICE_HEALTH_URL[${svc}]:-}" ]] && needs_ecs_check=1 && break
done

if [[ "${needs_ecs_check}" -eq 1 ]]; then
  resolve_ecs_host
  if [[ -n "${ECS_HOST_INSTANCE_ID}" ]]; then
    log "ecs_host=${ECS_HOST_INSTANCE_ID} — sending SSM health probe"
    start_ssm_health_batch
  else
    log "no InService ECS host found — skipping SSM health probes"
  fi
fi

# ─── main dispatch ────────────────────────────────────────────────────────────

ecs_services_checked=()
for service_name in "${SERVICES_TO_CHECK[@]}"; do
  case "${service_name}" in
    sundial-node|prometheus|loki|alloy|grafana|postgres-exporter)
      ecs_services_checked+=("${service_name}")
      ;;
  esac
done

# Poll SSM results before printing ECS service lines
if [[ "${#ecs_services_checked[@]}" -gt 0 && -n "${SSM_BATCH_CMD_ID}" ]]; then
  log "polling SSM health probe results"
  poll_ssm_health_batch
fi

for service_name in "${SERVICES_TO_CHECK[@]}"; do
  ok=1
  case "${service_name}" in
    sundial-node)
      check_ecs_service_with_alb "sundial-node" "${NAME_PREFIX}-node" || ok=0
      ;;
    prometheus|loki|alloy|grafana|postgres-exporter)
      check_ecs_service "${service_name}" || ok=0
      ;;
    ecs-host)
      check_asg "ecs-host" "${NAME_PREFIX}-ecs-asg" || ok=0
      ;;
    nat)
      check_nat "nat" || ok=0
      ;;
    redis)
      check_elasticache "redis" "${NAME_PREFIX}-redis" || ok=0
      ;;
    *)
      log "unknown service: ${service_name}"
      ok=0
      ;;
  esac

  if [[ "${ok}" -eq 0 ]]; then
    failing_count=$((failing_count + 1))
  fi
done

if [[ "${failing_count}" -gt 0 ]]; then
  log "result=failing failing_services=${failing_count}"
  exit 1
fi

log "result=live checked_services=${#SERVICES_TO_CHECK[@]}"
