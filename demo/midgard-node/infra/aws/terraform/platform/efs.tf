resource "aws_kms_key" "efs" {
  description             = "${local.name_prefix} EFS"
  deletion_window_in_days = 7
  enable_key_rotation     = true
}

resource "aws_efs_file_system" "mpt" {
  creation_token   = "${local.name_prefix}-mpt"
  encrypted        = true
  kms_key_id       = aws_kms_key.efs.arn
  performance_mode = "generalPurpose"
  throughput_mode  = "bursting"

  tags = {
    Name = "${local.name_prefix}-mpt"
  }
}

resource "aws_efs_mount_target" "mpt" {
  count = local.private_subnet_count

  file_system_id  = aws_efs_file_system.mpt.id
  subnet_id       = aws_subnet.private[count.index].id
  security_groups = [aws_security_group.sg_efs.id]
}

resource "aws_efs_access_point" "sundial_node" {
  file_system_id = aws_efs_file_system.mpt.id

  posix_user {
    gid = 1000
    uid = 1000
  }

  root_directory {
    path = "/sundial-node"

    creation_info {
      owner_gid   = 1000
      owner_uid   = 1000
      permissions = "0755"
    }
  }
}

# Persistent storage for the observability stack so extended Prometheus/Loki
# retention actually survives ECS task rescheduling (host_path volumes do not).
resource "aws_efs_access_point" "prometheus_data" {
  file_system_id = aws_efs_file_system.mpt.id

  posix_user {
    gid = 0
    uid = 0
  }

  root_directory {
    path = "/prometheus-data"

    creation_info {
      owner_gid   = 0
      owner_uid   = 0
      permissions = "0755"
    }
  }
}

resource "aws_efs_access_point" "loki_data" {
  file_system_id = aws_efs_file_system.mpt.id

  posix_user {
    gid = 0
    uid = 0
  }

  root_directory {
    path = "/loki-data"

    creation_info {
      owner_gid   = 0
      owner_uid   = 0
      permissions = "0755"
    }
  }
}
