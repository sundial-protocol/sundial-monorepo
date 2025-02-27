# Midgard Manager

Tools for managing Midgard MVP L2 node and generating test transactions.

## Structure

- **CLI** (`packages/cli`): Command-line interface for Midgard nodes
- **TX Generator** (`packages/tx-generator`): Transaction generator for testing

### Interactive Mode

```bash
pnpm start interactive
```

### Direct CLI Commands

```bash
# Show help
pnpm start --help

# Transaction generator
pnpm tx-generator start --test-wallet --type mixed --batch-size 10 --interval 5 --concurrency 1
```

## Configuration

The CLI uses a centralized configuration system thats dynamically is changed on actions.

```json
{
  "node": {
    "endpoint": "http://localhost:3000"
  },
  "generator": {
    "enabled": false,
    "maxConcurrent": 10,
    "batchSize": 100,
    "intervalMs": 1000
  },
  "logging": {
    "level": "info",
    "format": "pretty"
  }
}
```

Configuration can be modified through:

- Interactive mode menus
- CLI commands
- Manual edit of `config/settings.json`
