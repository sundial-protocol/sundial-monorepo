output "alb_dns_name" {
  description = "Public ALB DNS name for the Sundial node API."
  value       = aws_lb.api.dns_name
}

output "grafana_domain" {
  description = "Public Grafana domain routed via ALB host-based listener rule."
  value       = var.grafana_domain
}

output "grafana_url" {
  description = "Public Grafana URL."
  value       = "https://${var.grafana_domain}"
}

output "acm_certificate_arn" {
  description = "ACM certificate ARN for the public API domain."
  value       = aws_acm_certificate.api.arn
}

output "acm_validation_records" {
  description = "DNS validation records to add at the public DNS provider."
  value = [
    for option in aws_acm_certificate.api.domain_validation_options : {
      name  = option.resource_record_name
      type  = option.resource_record_type
      value = option.resource_record_value
    }
  ]
}

output "ecs_cluster_name" {
  description = "ECS cluster name."
  value       = aws_ecs_cluster.main.name
}

output "ecs_log_group_name" {
  description = "CloudWatch log group used by ECS tasks."
  value       = aws_cloudwatch_log_group.ecs.name
}

output "rds_endpoint" {
  description = "Private RDS endpoint."
  value       = aws_db_instance.main.address
}

output "private_dns_namespace" {
  description = "Private Cloud Map namespace."
  value       = var.private_dns_namespace_name
}

output "efs_file_system_id" {
  description = "EFS file system used for MPT storage."
  value       = aws_efs_file_system.mpt.id
}
