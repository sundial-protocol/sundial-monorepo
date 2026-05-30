resource "aws_s3_bucket" "grafana_assets" {
  bucket        = "${local.name_prefix}-grafana-assets"
  force_destroy = true

  tags = {
    Name = "${local.name_prefix}-grafana-assets"
  }
}

resource "aws_s3_bucket_server_side_encryption_configuration" "grafana_assets" {
  bucket = aws_s3_bucket.grafana_assets.id

  rule {
    apply_server_side_encryption_by_default {
      sse_algorithm = "AES256"
    }
  }
}

resource "aws_s3_bucket_public_access_block" "grafana_assets" {
  bucket                  = aws_s3_bucket.grafana_assets.id
  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}

resource "aws_iam_role_policy" "ecs_task_grafana_s3" {
  name = "${local.name_prefix}-grafana-s3"
  role = aws_iam_role.ecs_task.id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Effect   = "Allow"
      Action   = ["s3:GetObject"]
      Resource = "${aws_s3_bucket.grafana_assets.arn}/*"
    }]
  })
}
