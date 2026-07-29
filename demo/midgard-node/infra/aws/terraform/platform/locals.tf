locals {
  name_prefix          = "sundial-node-${var.environment}"
  ecs_log_group_name   = "/ecs/${local.name_prefix}"
  vpc_dns_resolver_ip  = cidrhost(var.vpc_cidr, 2)
  public_subnet_count  = length(var.public_subnet_cidrs)
  private_subnet_count = length(var.private_subnet_cidrs)

  secret_prefix = "sundial-node/${var.environment}"

  sundial_node_environment = [
    { name = "NODE_ENV", value = "production" },
    { name = "NETWORK", value = var.network },
    { name = "PORT", value = tostring(var.sundial_node_container_port) },
    { name = "PROM_METRICS_PORT", value = tostring(var.sundial_node_metrics_port) },
    { name = "POSTGRES_HOST", value = aws_db_instance.main.address },
    { name = "POSTGRES_DB", value = var.rds_db_name },
    { name = "POSTGRES_USER", value = var.rds_master_username },
    { name = "LEDGER_MPT_DB_PATH", value = "/var/lib/sundial-node/ledger-mpt" },
    { name = "MEMPOOL_MPT_DB_PATH", value = "/var/lib/sundial-node/mempool-mpt" },
    { name = "WAIT_BETWEEN_BLOCK_COMMITMENTS", value = tostring(var.wait_between_block_commitments_ms) },
    { name = "WAIT_BETWEEN_BLOCK_SUBMISSIONS", value = tostring(var.wait_between_block_submissions_ms) },
    { name = "WAIT_BETWEEN_USER_EVENT_FETCHES", value = tostring(var.wait_between_user_event_fetches_ms) },
    { name = "WAIT_BETWEEN_MERGE_TXS", value = tostring(var.wait_between_merge_txs_ms) },
    { name = "COMMITMENT_WORKER_TIMEOUT_MS", value = tostring(var.commitment_worker_timeout_ms) },
    { name = "OLTP_EXPORTER_URL", value = "http://alloy.${var.private_dns_namespace_name}:4318/v1/traces" },
    { name = "REDIS_URL", value = "redis://${aws_elasticache_cluster.redis.cache_nodes[0].address}:${aws_elasticache_cluster.redis.port}" },
    { name = "PGSSLMODE", value = "disable" },
    { name = "FAUCET_ENABLED", value = tostring(var.faucet_enabled) },
    { name = "FAUCET_AMOUNT_LOVELACE", value = var.faucet_amount_lovelace },
    { name = "FAUCET_COOLDOWN_SECONDS", value = tostring(var.faucet_cooldown_seconds) },
    { name = "FAUCET_DAILY_IP_LIMIT", value = tostring(var.faucet_daily_ip_limit) },
    { name = "FAUCET_MIN_BALANCE_LOVELACE", value = var.faucet_min_balance_lovelace },
    { name = "FAUCET_GENESIS_ALLOCATION_LOVELACE", value = var.faucet_genesis_allocation_lovelace },
  ]
}
