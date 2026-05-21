resource "aws_db_subnet_group" "main" {
  name       = "${local.name_prefix}-rds"
  subnet_ids = aws_subnet.private[*].id

  tags = {
    Name = "${local.name_prefix}-rds"
  }
}

resource "aws_db_parameter_group" "main" {
  name   = "${local.name_prefix}-postgres16"
  family = "postgres16"

  parameter {
    name  = "log_min_duration_statement"
    value = "1000"
  }
}

resource "aws_db_instance" "main" {
  identifier = local.name_prefix

  engine         = "postgres"
  engine_version = "16"
  instance_class = var.rds_instance_class

  allocated_storage      = var.rds_storage_gb
  storage_type           = "gp3"
  db_name                = var.rds_db_name
  username               = var.rds_master_username
  password               = data.aws_secretsmanager_secret_version.rds_master_password.secret_string
  db_subnet_group_name   = aws_db_subnet_group.main.name
  parameter_group_name   = aws_db_parameter_group.main.name
  vpc_security_group_ids = [aws_security_group.sg_rds.id]

  multi_az                = var.rds_multi_az
  backup_retention_period = var.rds_backup_retention_days
  enabled_cloudwatch_logs_exports = [
    "postgresql",
  ]
  deletion_protection       = var.rds_deletion_protection
  skip_final_snapshot       = var.rds_skip_final_snapshot
  final_snapshot_identifier = var.rds_final_snapshot_identifier
  publicly_accessible       = false
  storage_encrypted         = true

  tags = {
    Name = local.name_prefix
  }
}

data "aws_secretsmanager_secret_version" "rds_master_password" {
  secret_id = data.aws_secretsmanager_secret.rds_master_password.id
}
