environment = "testnet"
aws_region  = "us-west-2"

api_domain     = "sundial-node.testnet.sundialprotocol.com"
grafana_domain = "grafana.testnet.sundialprotocol.com"

vpc_cidr             = "10.2.0.0/16"
availability_zones   = ["us-west-2a", "us-west-2b"]
public_subnet_cidrs  = ["10.2.0.0/24", "10.2.2.0/24"]
private_subnet_cidrs = ["10.2.1.0/24", "10.2.3.0/24"]

nat_type          = "instance"
nat_instance_type = "t3.micro"

ecs_instance_type          = "m6i.large"
ecs_host_ebs_size_gb       = 30
ecs_desired_count          = 2
ecs_min_size               = 2
ecs_max_size               = 2
enable_ecs_services        = false
enable_https_listener      = true
sundial_node_image         = "000000000000.dkr.ecr.us-west-2.amazonaws.com/sundial/sundial-node:testnet-latest"
sundial_node_desired_count = 1

rds_instance_class            = "db.t4g.micro"
rds_multi_az                  = false
rds_backup_retention_days     = 1
rds_storage_gb                = 20
rds_db_name                   = "sundial"
rds_master_username           = "sundial"
rds_deletion_protection       = true
rds_skip_final_snapshot       = false
rds_final_snapshot_identifier = "sundial-node-testnet-final"

network = "Preprod"

prometheus_retention = "3d"
loki_retention_days  = 3
