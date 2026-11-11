environment = "testnet"
aws_region  = "us-west-2"

api_domain     = "rpc.testnet.sundialprotocol.com"
grafana_domain = "dashboard.testnet.sundialprotocol.com"

vpc_cidr             = "10.2.0.0/16"
availability_zones   = ["us-west-2a", "us-west-2b"]
public_subnet_cidrs  = ["10.2.0.0/24", "10.2.2.0/24"]
private_subnet_cidrs = ["10.2.1.0/24", "10.2.3.0/24"]

nat_type          = "gateway"
nat_instance_type = "t3.micro"

ecs_instance_type          = "m6i.large"
ecs_host_ebs_size_gb       = 30
ecs_desired_count          = 2
ecs_min_size               = 2
ecs_max_size               = 2
enable_ecs_services        = true
enable_https_listener      = true
sundial_node_image         = "810809345231.dkr.ecr.us-west-2.amazonaws.com/sundial/sundial-node:testnet-latest"
sundial_node_desired_count = 1

redis_node_type = "cache.t3.micro"

rds_instance_class            = "db.t4g.micro"
rds_multi_az                  = false
rds_backup_retention_days     = 1
rds_storage_gb                = 20
rds_db_name                   = "sundial"
rds_master_username           = "sundial"
rds_deletion_protection       = false
rds_skip_final_snapshot       = false
rds_final_snapshot_identifier = "sundial-node-testnet-final"

network = "Preprod"

# Retention sized for backward reliability reporting (closed calendar windows
# plus margin). Data now lives on EFS, so it survives task rescheduling.
prometheus_retention      = "120d"
prometheus_retention_size = "40GB"
loki_retention_days       = 120

# Faucet wallet. Flip faucet_enabled to true once the faucet-seed-phrase and
# faucet-api-key secrets exist in Secrets Manager under sundial-node/testnet/.
faucet_enabled                     = true
faucet_amount_lovelace             = "100000000"
faucet_cooldown_seconds            = 86400
faucet_daily_ip_limit              = 5
faucet_min_balance_lovelace        = "100000000"
faucet_genesis_allocation_lovelace = "10000000000000"

# The node reads L1 through Blockfrost's free tier (50,000 requests a day). At the
# 10-11 s defaults it ran out every evening around 19:00 UTC and was not ready
# until the midnight reset. 30 s should keep it under the limit. Live since
# 2026-10-29 as task definition sundial-node-testnet-node:12.
wait_between_block_submissions_ms  = 30000
wait_between_user_event_fetches_ms = 30000
wait_between_merge_txs_ms          = 30000
