resource "aws_security_group" "sg_nat" {
  name        = "${local.name_prefix}-sg-nat"
  description = "NAT instance security group"
  vpc_id      = aws_vpc.main.id
}

resource "aws_vpc_security_group_ingress_rule" "sg_nat_ingress_private" {
  for_each = { for idx, cidr in var.private_subnet_cidrs : idx => cidr }

  security_group_id = aws_security_group.sg_nat.id
  cidr_ipv4         = each.value
  ip_protocol       = "-1"
  description       = "Allow private subnet egress transit"
}

resource "aws_vpc_security_group_egress_rule" "sg_nat_egress_all" {
  security_group_id = aws_security_group.sg_nat.id
  cidr_ipv4         = "0.0.0.0/0"
  ip_protocol       = "-1"
  description       = "Allow internet egress"
}

resource "aws_security_group" "sg_alb" {
  name        = "${local.name_prefix}-sg-alb"
  description = "ALB ingress and ECS dynamic target egress"
  vpc_id      = aws_vpc.main.id
}

resource "aws_security_group" "sg_ecs" {
  name        = "${local.name_prefix}-sg-ecs"
  description = "ECS hosts and bridge-mode task traffic"
  vpc_id      = aws_vpc.main.id
}

resource "aws_security_group" "sg_rds" {
  name        = "${local.name_prefix}-sg-rds"
  description = "RDS PostgreSQL"
  vpc_id      = aws_vpc.main.id
}

resource "aws_security_group" "sg_efs" {
  name        = "${local.name_prefix}-sg-efs"
  description = "EFS for Sundial MPT storage"
  vpc_id      = aws_vpc.main.id
}

resource "aws_vpc_security_group_ingress_rule" "sg_alb_ingress_https" {
  security_group_id = aws_security_group.sg_alb.id
  cidr_ipv4         = "0.0.0.0/0"
  from_port         = 443
  to_port           = 443
  ip_protocol       = "tcp"
  description       = "HTTPS from internet"
}

resource "aws_vpc_security_group_ingress_rule" "sg_alb_ingress_http" {
  security_group_id = aws_security_group.sg_alb.id
  cidr_ipv4         = "0.0.0.0/0"
  from_port         = 80
  to_port           = 80
  ip_protocol       = "tcp"
  description       = "HTTP bootstrap or redirect from internet"
}

resource "aws_vpc_security_group_egress_rule" "sg_alb_egress_ecs_dynamic" {
  security_group_id            = aws_security_group.sg_alb.id
  referenced_security_group_id = aws_security_group.sg_ecs.id
  from_port                    = 32768
  to_port                      = 65535
  ip_protocol                  = "tcp"
  description                  = "ALB to ECS dynamic ports"
}

resource "aws_vpc_security_group_ingress_rule" "sg_ecs_ingress_from_alb" {
  security_group_id            = aws_security_group.sg_ecs.id
  referenced_security_group_id = aws_security_group.sg_alb.id
  from_port                    = 32768
  to_port                      = 65535
  ip_protocol                  = "tcp"
  description                  = "ALB forwarded requests and health checks"
}

resource "aws_vpc_security_group_ingress_rule" "sg_ecs_ingress_metrics_from_ecs" {
  security_group_id            = aws_security_group.sg_ecs.id
  referenced_security_group_id = aws_security_group.sg_ecs.id
  from_port                    = var.sundial_node_metrics_port
  to_port                      = var.sundial_node_metrics_port
  ip_protocol                  = "tcp"
  description                  = "Internal Sundial metrics scrape"
}

resource "aws_vpc_security_group_ingress_rule" "sg_ecs_ingress_from_ecs_dynamic" {
  security_group_id            = aws_security_group.sg_ecs.id
  referenced_security_group_id = aws_security_group.sg_ecs.id
  from_port                    = 32768
  to_port                      = 65535
  ip_protocol                  = "tcp"
  description                  = "Inter-task dynamic traffic"
}

resource "aws_vpc_security_group_egress_rule" "sg_ecs_egress_ecs_dynamic" {
  security_group_id            = aws_security_group.sg_ecs.id
  referenced_security_group_id = aws_security_group.sg_ecs.id
  from_port                    = 32768
  to_port                      = 65535
  ip_protocol                  = "tcp"
  description                  = "Inter-task dynamic traffic"
}

resource "aws_vpc_security_group_egress_rule" "sg_ecs_egress_metrics" {
  security_group_id            = aws_security_group.sg_ecs.id
  referenced_security_group_id = aws_security_group.sg_ecs.id
  from_port                    = var.sundial_node_metrics_port
  to_port                      = var.sundial_node_metrics_port
  ip_protocol                  = "tcp"
  description                  = "Prometheus to Sundial metrics"
}

resource "aws_vpc_security_group_egress_rule" "sg_ecs_egress_rds" {
  security_group_id            = aws_security_group.sg_ecs.id
  referenced_security_group_id = aws_security_group.sg_rds.id
  from_port                    = 5432
  to_port                      = 5432
  ip_protocol                  = "tcp"
  description                  = "PostgreSQL"
}

resource "aws_vpc_security_group_egress_rule" "sg_ecs_egress_efs" {
  security_group_id            = aws_security_group.sg_ecs.id
  referenced_security_group_id = aws_security_group.sg_efs.id
  from_port                    = 2049
  to_port                      = 2049
  ip_protocol                  = "tcp"
  description                  = "EFS NFS"
}

resource "aws_vpc_security_group_egress_rule" "sg_ecs_egress_https" {
  security_group_id = aws_security_group.sg_ecs.id
  cidr_ipv4         = "0.0.0.0/0"
  from_port         = 443
  to_port           = 443
  ip_protocol       = "tcp"
  description       = "ECR, SSM, Cardano provider, external HTTPS"
}

resource "aws_vpc_security_group_egress_rule" "sg_ecs_egress_http" {
  security_group_id = aws_security_group.sg_ecs.id
  cidr_ipv4         = "0.0.0.0/0"
  from_port         = 80
  to_port           = 80
  ip_protocol       = "tcp"
  description       = "Package mirror fallback"
}

resource "aws_vpc_security_group_egress_rule" "sg_ecs_egress_dns_udp" {
  security_group_id = aws_security_group.sg_ecs.id
  cidr_ipv4         = "${local.vpc_dns_resolver_ip}/32"
  from_port         = 53
  to_port           = 53
  ip_protocol       = "udp"
  description       = "VPC DNS resolver"
}

resource "aws_vpc_security_group_egress_rule" "sg_ecs_egress_dns_tcp" {
  security_group_id = aws_security_group.sg_ecs.id
  cidr_ipv4         = "${local.vpc_dns_resolver_ip}/32"
  from_port         = 53
  to_port           = 53
  ip_protocol       = "tcp"
  description       = "VPC DNS resolver"
}

resource "aws_vpc_security_group_ingress_rule" "sg_rds_ingress_from_ecs" {
  security_group_id            = aws_security_group.sg_rds.id
  referenced_security_group_id = aws_security_group.sg_ecs.id
  from_port                    = 5432
  to_port                      = 5432
  ip_protocol                  = "tcp"
  description                  = "PostgreSQL from ECS"
}

resource "aws_vpc_security_group_ingress_rule" "sg_efs_ingress_from_ecs" {
  security_group_id            = aws_security_group.sg_efs.id
  referenced_security_group_id = aws_security_group.sg_ecs.id
  from_port                    = 2049
  to_port                      = 2049
  ip_protocol                  = "tcp"
  description                  = "NFS from ECS"
}
