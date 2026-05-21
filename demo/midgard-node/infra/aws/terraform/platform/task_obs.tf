resource "aws_service_discovery_service" "prometheus" {
  name = "prometheus"

  dns_config {
    namespace_id = aws_service_discovery_private_dns_namespace.internal.id

    dns_records {
      type = "SRV"
      ttl  = 10
    }

    routing_policy = "MULTIVALUE"
  }
}

resource "aws_service_discovery_service" "loki" {
  name = "loki"

  dns_config {
    namespace_id = aws_service_discovery_private_dns_namespace.internal.id

    dns_records {
      type = "SRV"
      ttl  = 10
    }

    routing_policy = "MULTIVALUE"
  }
}

resource "aws_service_discovery_service" "alloy" {
  name = "alloy"

  dns_config {
    namespace_id = aws_service_discovery_private_dns_namespace.internal.id

    dns_records {
      type = "SRV"
      ttl  = 10
    }

    routing_policy = "MULTIVALUE"
  }
}

resource "aws_service_discovery_service" "grafana" {
  name = "grafana"

  dns_config {
    namespace_id = aws_service_discovery_private_dns_namespace.internal.id

    dns_records {
      type = "SRV"
      ttl  = 10
    }

    routing_policy = "MULTIVALUE"
  }
}

resource "aws_service_discovery_service" "postgres_exporter" {
  name = "postgres-exporter"

  dns_config {
    namespace_id = aws_service_discovery_private_dns_namespace.internal.id

    dns_records {
      type = "SRV"
      ttl  = 10
    }

    routing_policy = "MULTIVALUE"
  }
}

resource "aws_ecs_task_definition" "prometheus" {
  family                   = "${local.name_prefix}-prometheus"
  requires_compatibilities = ["EC2"]
  network_mode             = "bridge"
  cpu                      = "512"
  memory                   = "1024"
  execution_role_arn       = aws_iam_role.ecs_task_execution.arn
  task_role_arn            = aws_iam_role.ecs_task.arn

  volume {
    name      = "prometheus-data"
    host_path = "/var/lib/prometheus"
  }

  container_definitions = jsonencode([
    {
      name       = "prometheus"
      image      = var.prometheus_image
      essential  = true
      entryPoint = ["/bin/sh", "-ec"]
      command = [
        <<-CMD
          cat >/etc/prometheus/prometheus.yml <<'CFG'
          global:
            scrape_interval: 15s
            evaluation_interval: 15s

          scrape_configs:
            - job_name: prometheus
              static_configs:
                - targets: ["localhost:9090"]

            - job_name: sundial-node
              metrics_path: /metrics
              dns_sd_configs:
                - names: ["sundial-node.${var.private_dns_namespace_name}"]
                  type: SRV
                  refresh_interval: 30s

            - job_name: postgres-exporter
              metrics_path: /metrics
              dns_sd_configs:
                - names: ["postgres-exporter.${var.private_dns_namespace_name}"]
                  type: SRV
                  refresh_interval: 30s
          CFG

          exec /bin/prometheus \
            --config.file=/etc/prometheus/prometheus.yml \
            --storage.tsdb.path=/prometheus \
            --storage.tsdb.retention.time=${var.prometheus_retention}
        CMD
      ]
      portMappings = [{ containerPort = 9090, hostPort = 0, protocol = "tcp" }]
      mountPoints  = [{ sourceVolume = "prometheus-data", containerPath = "/prometheus", readOnly = false }]
      logConfiguration = {
        logDriver = "awslogs"
        options = {
          awslogs-group         = aws_cloudwatch_log_group.ecs.name
          awslogs-region        = var.aws_region
          awslogs-stream-prefix = "prometheus"
        }
      }
    }
  ])
}

resource "aws_ecs_service" "prometheus" {
  count = var.enable_ecs_services ? 1 : 0

  name            = "prometheus"
  cluster         = aws_ecs_cluster.main.id
  task_definition = aws_ecs_task_definition.prometheus.arn
  desired_count   = var.observability_desired_count
  launch_type     = "EC2"

  service_registries {
    registry_arn   = aws_service_discovery_service.prometheus.arn
    container_name = "prometheus"
    container_port = 9090
  }

  depends_on = [aws_ecs_cluster_capacity_providers.main]
}

resource "aws_ecs_task_definition" "loki" {
  family                   = "${local.name_prefix}-loki"
  requires_compatibilities = ["EC2"]
  network_mode             = "bridge"
  cpu                      = "256"
  memory                   = "512"
  execution_role_arn       = aws_iam_role.ecs_task_execution.arn
  task_role_arn            = aws_iam_role.ecs_task.arn

  volume {
    name      = "loki-data"
    host_path = "/var/lib/loki"
  }

  container_definitions = jsonencode([
    {
      name         = "loki"
      image        = var.loki_image
      essential    = true
      command      = ["-config.file=/etc/loki/local-config.yaml"]
      portMappings = [{ containerPort = 3100, hostPort = 0, protocol = "tcp" }]
      mountPoints  = [{ sourceVolume = "loki-data", containerPath = "/loki", readOnly = false }]
      logConfiguration = {
        logDriver = "awslogs"
        options = {
          awslogs-group         = aws_cloudwatch_log_group.ecs.name
          awslogs-region        = var.aws_region
          awslogs-stream-prefix = "loki"
        }
      }
    }
  ])
}

resource "aws_ecs_service" "loki" {
  count = var.enable_ecs_services ? 1 : 0

  name            = "loki"
  cluster         = aws_ecs_cluster.main.id
  task_definition = aws_ecs_task_definition.loki.arn
  desired_count   = var.observability_desired_count
  launch_type     = "EC2"

  service_registries {
    registry_arn   = aws_service_discovery_service.loki.arn
    container_name = "loki"
    container_port = 3100
  }

  depends_on = [aws_ecs_cluster_capacity_providers.main]
}

resource "aws_ecs_task_definition" "alloy" {
  family                   = "${local.name_prefix}-alloy"
  requires_compatibilities = ["EC2"]
  network_mode             = "bridge"
  cpu                      = "256"
  memory                   = "512"
  execution_role_arn       = aws_iam_role.ecs_task_execution.arn
  task_role_arn            = aws_iam_role.ecs_task.arn

  container_definitions = jsonencode([
    {
      name       = "alloy"
      image      = var.alloy_image
      essential  = true
      entryPoint = ["/bin/sh", "-ec"]
      command = [
        <<-CMD
          cat >/tmp/config.alloy <<'CFG'
          otelcol.receiver.otlp "default" {
            http {
              endpoint = "0.0.0.0:4318"
            }
            output {
              traces = [otelcol.processor.batch.default.input]
            }
          }

          otelcol.processor.batch "default" {
            output {
              traces = [otelcol.exporter.debug.default.input]
            }
          }

          otelcol.exporter.debug "default" {}
          CFG

          exec /bin/alloy run --server.http.listen-addr=0.0.0.0:12345 /tmp/config.alloy
        CMD
      ]
      portMappings = [{ containerPort = 4318, hostPort = 0, protocol = "tcp" }]
      logConfiguration = {
        logDriver = "awslogs"
        options = {
          awslogs-group         = aws_cloudwatch_log_group.ecs.name
          awslogs-region        = var.aws_region
          awslogs-stream-prefix = "alloy"
        }
      }
    }
  ])
}

resource "aws_ecs_service" "alloy" {
  count = var.enable_ecs_services ? 1 : 0

  name            = "alloy"
  cluster         = aws_ecs_cluster.main.id
  task_definition = aws_ecs_task_definition.alloy.arn
  desired_count   = var.observability_desired_count
  launch_type     = "EC2"

  service_registries {
    registry_arn   = aws_service_discovery_service.alloy.arn
    container_name = "alloy"
    container_port = 4318
  }

  depends_on = [aws_ecs_cluster_capacity_providers.main]
}

resource "aws_ecs_task_definition" "grafana" {
  family                   = "${local.name_prefix}-grafana"
  requires_compatibilities = ["EC2"]
  network_mode             = "bridge"
  cpu                      = "256"
  memory                   = "512"
  execution_role_arn       = aws_iam_role.ecs_task_execution.arn
  task_role_arn            = aws_iam_role.ecs_task.arn

  volume {
    name      = "grafana-data"
    host_path = "/var/lib/grafana"
  }

  container_definitions = jsonencode([
    {
      name      = "grafana"
      image     = var.grafana_image
      essential = true
      environment = [
        { name = "GF_SECURITY_ADMIN_USER", value = "admin" },
        { name = "GF_AUTH_ANONYMOUS_ENABLED", value = "true" },
        { name = "GF_AUTH_ANONYMOUS_ORG_ROLE", value = "Viewer" },
        { name = "GF_AUTH_DISABLE_LOGIN_FORM", value = "true" }
      ]
      secrets = [
        { name = "GF_SECURITY_ADMIN_PASSWORD", valueFrom = data.aws_secretsmanager_secret.grafana_admin_password.arn }
      ]
      portMappings = [{ containerPort = 3000, hostPort = 0, protocol = "tcp" }]
      mountPoints  = [{ sourceVolume = "grafana-data", containerPath = "/var/lib/grafana", readOnly = false }]
      logConfiguration = {
        logDriver = "awslogs"
        options = {
          awslogs-group         = aws_cloudwatch_log_group.ecs.name
          awslogs-region        = var.aws_region
          awslogs-stream-prefix = "grafana"
        }
      }
    }
  ])
}

resource "aws_ecs_service" "grafana" {
  count = var.enable_ecs_services ? 1 : 0

  name            = "grafana"
  cluster         = aws_ecs_cluster.main.id
  task_definition = aws_ecs_task_definition.grafana.arn
  desired_count   = var.observability_desired_count
  launch_type     = "EC2"

  load_balancer {
    target_group_arn = aws_lb_target_group.grafana.arn
    container_name   = "grafana"
    container_port   = 3000
  }

  service_registries {
    registry_arn   = aws_service_discovery_service.grafana.arn
    container_name = "grafana"
    container_port = 3000
  }

  depends_on = [
    aws_ecs_cluster_capacity_providers.main,
    aws_lb_target_group.grafana,
  ]
}

resource "aws_ecs_task_definition" "postgres_exporter" {
  family                   = "${local.name_prefix}-postgres-exporter"
  requires_compatibilities = ["EC2"]
  network_mode             = "bridge"
  cpu                      = "128"
  memory                   = "256"
  execution_role_arn       = aws_iam_role.ecs_task_execution.arn
  task_role_arn            = aws_iam_role.ecs_task.arn

  container_definitions = jsonencode([
    {
      name      = "postgres-exporter"
      image     = var.postgres_exporter_image
      essential = true
      environment = [
        { name = "DATA_SOURCE_URI", value = "${aws_db_instance.main.address}:5432/${var.rds_db_name}?sslmode=require" },
        { name = "DATA_SOURCE_USER", value = var.rds_master_username }
      ]
      secrets = [
        { name = "DATA_SOURCE_PASS", valueFrom = data.aws_secretsmanager_secret.rds_master_password.arn }
      ]
      portMappings = [{ containerPort = 9187, hostPort = 0, protocol = "tcp" }]
      logConfiguration = {
        logDriver = "awslogs"
        options = {
          awslogs-group         = aws_cloudwatch_log_group.ecs.name
          awslogs-region        = var.aws_region
          awslogs-stream-prefix = "postgres-exporter"
        }
      }
    }
  ])
}

resource "aws_ecs_service" "postgres_exporter" {
  count = var.enable_ecs_services ? 1 : 0

  name            = "postgres-exporter"
  cluster         = aws_ecs_cluster.main.id
  task_definition = aws_ecs_task_definition.postgres_exporter.arn
  desired_count   = var.observability_desired_count
  launch_type     = "EC2"

  service_registries {
    registry_arn   = aws_service_discovery_service.postgres_exporter.arn
    container_name = "postgres-exporter"
    container_port = 9187
  }

  depends_on = [aws_ecs_cluster_capacity_providers.main]
}
