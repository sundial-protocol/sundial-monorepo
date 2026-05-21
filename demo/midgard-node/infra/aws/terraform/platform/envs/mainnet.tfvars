environment = "mainnet"
aws_region  = "us-west-1"

api_domain     = "sundial-node.sundialprotocol.com"
grafana_domain = "grafana.sundialprotocol.com"

vpc_cidr             = "10.3.0.0/16"
availability_zones   = ["us-west-1a", "us-west-1b"]
public_subnet_cidrs  = ["10.3.0.0/24", "10.3.2.0/24"]
private_subnet_cidrs = ["10.3.1.0/24", "10.3.3.0/24"]

nat_type = "gateway"

ecs_instance_type          = "m6i.large"
ecs_host_ebs_size_gb       = 40
ecs_desired_count          = 2
ecs_min_size               = 2
ecs_max_size               = 3
enable_ecs_services        = false
enable_https_listener      = true
sundial_node_image         = "000000000000.dkr.ecr.us-west-1.amazonaws.com/sundial/sundial-node:mainnet-latest"
sundial_node_desired_count = 1

rds_instance_class            = "db.t4g.small"
rds_multi_az                  = true
rds_backup_retention_days     = 7
rds_storage_gb                = 40
rds_db_name                   = "sundial"
rds_master_username           = "sundial"
rds_deletion_protection       = true
rds_skip_final_snapshot       = false
rds_final_snapshot_identifier = "sundial-node-mainnet-final"

network = "Mainnet"

prometheus_retention = "14d"
loki_retention_days  = 14
