resource "aws_service_discovery_service" "sundial_node" {
  name = "sundial-node"

  dns_config {
    namespace_id = aws_service_discovery_private_dns_namespace.internal.id

    dns_records {
      type = "SRV"
      ttl  = 10
    }

    routing_policy = "MULTIVALUE"
  }
}

resource "aws_ecs_task_definition" "sundial_node" {
  family                   = "${local.name_prefix}-node"
  requires_compatibilities = ["EC2"]
  network_mode             = "bridge"
  cpu                      = "1024"
  memory                   = "2048"
  execution_role_arn       = aws_iam_role.ecs_task_execution.arn
  task_role_arn            = aws_iam_role.ecs_task.arn

  volume {
    name = "sundial-mpt"

    efs_volume_configuration {
      file_system_id     = aws_efs_file_system.mpt.id
      transit_encryption = "ENABLED"

      authorization_config {
        access_point_id = aws_efs_access_point.sundial_node.id
        iam             = "DISABLED"
      }
    }
  }

  container_definitions = jsonencode([
    {
      name      = "sundial-node"
      image     = var.sundial_node_image
      essential = true
      command   = ["node", "./dist/index.cjs", "listen", "--with-monitoring"]

      portMappings = [
        {
          containerPort = var.sundial_node_container_port
          hostPort      = var.sundial_node_container_port
          protocol      = "tcp"
        },
        {
          containerPort = var.sundial_node_metrics_port
          hostPort      = var.sundial_node_metrics_port
          protocol      = "tcp"
        }
      ]

      environment = local.sundial_node_environment

      secrets = concat([
        { name = "POSTGRES_PASSWORD", valueFrom = data.aws_secretsmanager_secret.rds_master_password.arn },
        { name = "L1_PROVIDER", valueFrom = data.aws_secretsmanager_secret.l1_provider.arn },
        { name = "L1_BLOCKFROST_API_URL", valueFrom = data.aws_secretsmanager_secret.l1_blockfrost_api_url.arn },
        { name = "L1_BLOCKFROST_KEY", valueFrom = data.aws_secretsmanager_secret.l1_blockfrost_key.arn },
        { name = "L1_OGMIOS_KEY", valueFrom = data.aws_secretsmanager_secret.l1_ogmios_key.arn },
        { name = "L1_KUPO_KEY", valueFrom = data.aws_secretsmanager_secret.l1_kupo_key.arn },
        { name = "L1_OPERATOR_SEED_PHRASE", valueFrom = data.aws_secretsmanager_secret.operator_seed_phrase.arn },
        { name = "L1_OPERATOR_SEED_PHRASE_FOR_BLOCK_COMMITMENT", valueFrom = data.aws_secretsmanager_secret.operator_seed_phrase_block_commitment.arn },
        { name = "L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX", valueFrom = data.aws_secretsmanager_secret.operator_seed_phrase_merge.arn },
        { name = "TESTNET_GENESIS_WALLET_SEED_PHRASE_A", valueFrom = data.aws_secretsmanager_secret.testnet_genesis_wallet_seed_phrase_a.arn },
        { name = "TESTNET_GENESIS_WALLET_SEED_PHRASE_B", valueFrom = data.aws_secretsmanager_secret.testnet_genesis_wallet_seed_phrase_b.arn },
        { name = "TESTNET_GENESIS_WALLET_SEED_PHRASE_C", valueFrom = data.aws_secretsmanager_secret.testnet_genesis_wallet_seed_phrase_c.arn }
        ],
        var.faucet_enabled ? [
          { name = "FAUCET_SEED_PHRASE", valueFrom = data.aws_secretsmanager_secret.faucet_seed_phrase[0].arn },
          { name = "FAUCET_API_KEY", valueFrom = data.aws_secretsmanager_secret.faucet_api_key[0].arn },
        ] : [],
      )

      mountPoints = [
        {
          sourceVolume  = "sundial-mpt"
          containerPath = "/var/lib/sundial-node"
          readOnly      = false
        }
      ]

      logConfiguration = {
        logDriver = "awslogs"
        options = {
          awslogs-group         = aws_cloudwatch_log_group.ecs.name
          awslogs-region        = var.aws_region
          awslogs-stream-prefix = "sundial-node"
        }
      }
    }
  ])
}

resource "aws_ecs_service" "sundial_node" {
  count = var.enable_ecs_services ? 1 : 0

  name            = "sundial-node"
  cluster         = aws_ecs_cluster.main.id
  task_definition = aws_ecs_task_definition.sundial_node.arn
  desired_count   = var.sundial_node_desired_count
  launch_type     = "EC2"

  deployment_minimum_healthy_percent = 0
  deployment_maximum_percent         = 200
  force_new_deployment               = true

  load_balancer {
    target_group_arn = aws_lb_target_group.sundial_node.arn
    container_name   = "sundial-node"
    container_port   = var.sundial_node_container_port
  }

  service_registries {
    registry_arn   = aws_service_discovery_service.sundial_node.arn
    container_name = "sundial-node"
    container_port = var.sundial_node_metrics_port
  }

  depends_on = [
    aws_ecs_cluster_capacity_providers.main,
    aws_efs_mount_target.mpt,
    aws_lb_target_group.sundial_node,
  ]
}
