data "aws_secretsmanager_secret" "rds_master_password" {
  name = "${local.secret_prefix}/rds-master-password"
}

data "aws_secretsmanager_secret" "l1_provider" {
  name = "${local.secret_prefix}/l1-provider"
}

data "aws_secretsmanager_secret" "l1_blockfrost_api_url" {
  name = "${local.secret_prefix}/l1-blockfrost-api-url"
}

data "aws_secretsmanager_secret" "l1_blockfrost_key" {
  name = "${local.secret_prefix}/l1-blockfrost-key"
}

data "aws_secretsmanager_secret" "l1_ogmios_key" {
  name = "${local.secret_prefix}/l1-ogmios-key"
}

data "aws_secretsmanager_secret" "l1_kupo_key" {
  name = "${local.secret_prefix}/l1-kupo-key"
}

data "aws_secretsmanager_secret" "operator_seed_phrase" {
  name = "${local.secret_prefix}/operator-seed-phrase"
}

data "aws_secretsmanager_secret" "operator_seed_phrase_block_commitment" {
  name = "${local.secret_prefix}/operator-seed-phrase-block-commitment"
}

data "aws_secretsmanager_secret" "operator_seed_phrase_merge" {
  name = "${local.secret_prefix}/operator-seed-phrase-merge"
}

data "aws_secretsmanager_secret" "grafana_admin_password" {
  name = "${local.secret_prefix}/grafana-admin-password"
}

data "aws_secretsmanager_secret" "testnet_genesis_wallet_seed_phrase_a" {
  name = "${local.secret_prefix}/testnet-genesis-wallet-seed-phrase-a"
}

data "aws_secretsmanager_secret" "testnet_genesis_wallet_seed_phrase_b" {
  name = "${local.secret_prefix}/testnet-genesis-wallet-seed-phrase-b"
}

data "aws_secretsmanager_secret" "testnet_genesis_wallet_seed_phrase_c" {
  name = "${local.secret_prefix}/testnet-genesis-wallet-seed-phrase-c"
}

locals {
  sundial_node_secret_arns = [
    data.aws_secretsmanager_secret.rds_master_password.arn,
    data.aws_secretsmanager_secret.l1_provider.arn,
    data.aws_secretsmanager_secret.l1_blockfrost_api_url.arn,
    data.aws_secretsmanager_secret.l1_blockfrost_key.arn,
    data.aws_secretsmanager_secret.l1_ogmios_key.arn,
    data.aws_secretsmanager_secret.l1_kupo_key.arn,
    data.aws_secretsmanager_secret.operator_seed_phrase.arn,
    data.aws_secretsmanager_secret.operator_seed_phrase_block_commitment.arn,
    data.aws_secretsmanager_secret.operator_seed_phrase_merge.arn,
    data.aws_secretsmanager_secret.grafana_admin_password.arn,
    data.aws_secretsmanager_secret.testnet_genesis_wallet_seed_phrase_a.arn,
    data.aws_secretsmanager_secret.testnet_genesis_wallet_seed_phrase_b.arn,
    data.aws_secretsmanager_secret.testnet_genesis_wallet_seed_phrase_c.arn,
  ]
}
