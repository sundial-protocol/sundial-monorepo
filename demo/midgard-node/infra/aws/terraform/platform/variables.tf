variable "environment" {
  description = "Deployment environment name."
  type        = string

  validation {
    condition     = contains(["testnet", "mainnet"], var.environment)
    error_message = "environment must be testnet or mainnet."
  }
}

variable "aws_region" {
  description = "AWS region for platform resources."
  type        = string
}

variable "api_domain" {
  description = "Public Sundial node domain for ACM and ALB DNS."
  type        = string
}

variable "grafana_domain" {
  description = "Public Grafana domain routed through the same ALB."
  type        = string
}

variable "vpc_cidr" {
  description = "VPC CIDR block."
  type        = string
}

variable "availability_zones" {
  description = "Availability zones used by this environment."
  type        = list(string)
}

variable "public_subnet_cidrs" {
  description = "Public subnet CIDR blocks aligned to availability_zones."
  type        = list(string)
}

variable "private_subnet_cidrs" {
  description = "Private subnet CIDR blocks aligned to availability_zones."
  type        = list(string)
}

variable "nat_type" {
  description = "NAT strategy per environment (instance for lower-cost environments, gateway for stricter HA)."
  type        = string
  default     = "instance"

  validation {
    condition     = contains(["instance", "gateway"], var.nat_type)
    error_message = "nat_type must be instance or gateway."
  }
}

variable "nat_instance_type" {
  description = "EC2 type for NAT instance when nat_type=instance."
  type        = string
  default     = "t3.micro"
}

variable "ecs_instance_type" {
  description = "EC2 type for ECS container instances."
  type        = string
}

variable "ecs_host_ebs_size_gb" {
  description = "ECS EC2 root EBS size in GiB."
  type        = number
}

variable "ecs_desired_count" {
  description = "Desired number of ECS container instances in the ASG."
  type        = number
}

variable "ecs_min_size" {
  description = "Minimum number of ECS container instances."
  type        = number
}

variable "ecs_max_size" {
  description = "Maximum number of ECS container instances."
  type        = number
}

variable "enable_ecs_services" {
  description = "When false, ECS services are not created. Use for first apply."
  type        = bool
  default     = false
}

variable "enable_https_listener" {
  description = "Create the ALB HTTPS listener after ACM certificate validation is complete."
  type        = bool
  default     = false
}

variable "sundial_node_desired_count" {
  description = "Desired count for sundial-node ECS service."
  type        = number
  default     = 1
}

variable "observability_desired_count" {
  description = "Desired count for each observability ECS service."
  type        = number
  default     = 1
}

variable "sundial_node_image" {
  description = "Full image URI for sundial-node."
  type        = string
}

variable "sundial_node_container_port" {
  description = "Sundial node public HTTP container port."
  type        = number
  default     = 3000
}

variable "sundial_node_metrics_port" {
  description = "Sundial node Prometheus metrics port."
  type        = number
  default     = 9464
}

variable "rds_instance_class" {
  description = "RDS instance class."
  type        = string
}

variable "rds_multi_az" {
  description = "Enable Multi-AZ for RDS instance."
  type        = bool
}

variable "rds_backup_retention_days" {
  description = "RDS backup retention in days."
  type        = number
}

variable "rds_storage_gb" {
  description = "RDS allocated storage in GiB."
  type        = number
}

variable "rds_db_name" {
  description = "RDS database name."
  type        = string
  default     = "sundial"
}

variable "rds_master_username" {
  description = "RDS master username."
  type        = string
  default     = "sundial"
}

variable "rds_deletion_protection" {
  description = "Enable deletion protection for the RDS instance."
  type        = bool
  default     = true
}

variable "rds_skip_final_snapshot" {
  description = "Whether to skip creating a final snapshot on RDS deletion."
  type        = bool
  default     = false
}

variable "rds_final_snapshot_identifier" {
  description = "Final snapshot identifier used when deleting RDS."
  type        = string
}

variable "network" {
  description = "Lucid/Cardano network value for sundial-node."
  type        = string
  default     = "Preprod"

  validation {
    condition     = contains(["Preprod", "Preview", "Mainnet", "Custom"], var.network)
    error_message = "network must be one of Preprod, Preview, Mainnet, Custom."
  }
}

variable "wait_between_block_commitments_ms" {
  type    = number
  default = 1000
}

variable "wait_between_block_submissions_ms" {
  type    = number
  default = 10000
}

variable "wait_between_user_event_fetches_ms" {
  type    = number
  default = 11000
}

variable "wait_between_merge_txs_ms" {
  type    = number
  default = 10000
}

variable "commitment_worker_timeout_ms" {
  type    = number
  default = 300000
}

variable "private_dns_namespace_name" {
  description = "Private DNS namespace used by ECS Cloud Map service discovery."
  type        = string
  default     = "sundial-node.local"
}

variable "prometheus_image" {
  type    = string
  default = "prom/prometheus:v3.5.1"
}

variable "loki_image" {
  type    = string
  default = "grafana/loki:3.6.4"
}

variable "alloy_image" {
  type    = string
  default = "grafana/alloy:v1.12.1"
}

variable "grafana_image" {
  type    = string
  default = "grafana/grafana:12.3.2"
}

variable "postgres_exporter_image" {
  type    = string
  default = "prometheuscommunity/postgres-exporter:v0.17.1"
}

variable "prometheus_retention" {
  type    = string
  default = "3d"
}

variable "loki_retention_days" {
  type    = number
  default = 3
}
