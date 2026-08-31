# Prometheus rules

Files in this directory are loaded by Prometheus via `rule_files` in
[`../prometheus.yml`](../prometheus.yml) and mounted at `/etc/prometheus/rules`.

`slo-recording.rules.yml` and `slo-alerts.rules.yml` are **generated** from
[`../slo/slo.json`](../slo/slo.json) by [`../slo/gen-rules.mjs`](../slo/gen-rules.mjs).
Do not edit them by hand. After changing `slo.json`:

```sh
pnpm run slo:check   # regenerate, promtool check + test, fail if rules/ drifted
```

or `pnpm run slo:gen` to just regenerate. `rules/tests/slo.test.yml` holds the
`promtool test rules` fixtures.

The cloud Prometheus
([`../infra/aws/terraform/platform/task_obs.tf`](../infra/aws/terraform/platform/task_obs.tf))
embeds these two files at `tofu apply` time (base64, via `file()`), so the
committed + regenerated rules are exactly what ships.
