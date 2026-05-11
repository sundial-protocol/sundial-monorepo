<!--
Thanks for the PR. Please fill this out to speed up review.

Tips:
- Link issues with "Closes #123" / "Fixes #123"
- Keep scope focused; split unrelated changes into separate PRs
-->

# Summary

<!-- What does this change do, and why? Keep it concise. -->

## Type of change

<!-- Check all that apply. -->

- [ ] ✨ New feature
- [ ] 🐛 Bug fix
- [ ] 🧹 Refactor (no functional change)
- [ ] ⚡ Performance improvement
- [ ] 📚 Documentation
- [ ] 🧪 Tests
- [ ] 🔧 Build/CI/DevEx
- [ ] 🛡️ Security fix
- [ ] ⏪ Revert

## Related work

<!-- Link issues, PRs, RFCs, tickets. -->

- Issue(s):
- PR/RFC:
- Notes:

## Risk & impact

<!-- What can break? Any rollout/backward-compat considerations? -->

## How to test

<!-- Provide exact steps and command output summary. -->

1.
2.
3.

## API / contract changes (if applicable)

- [ ] No API/contract change
- [ ] Swagger/OpenAPI behavior changed and docs are updated
- [ ] Request/response validation updated (DTOs/validators)

## Database changes (if applicable)

- [ ] No DB/schema change
- [ ] Prisma schema changed
- [ ] Backfill/data migration plan documented

## Env/config changes (if applicable)

- [ ] No env/config change
- [ ] `.env.example` updated
- [ ] README updated
- [ ] Docker Compose/runtime config updated

# Checklist

## Implementation

- [ ] Scope is limited to the intended change
- [ ] Code follows project conventions and style guidelines
- [ ] No secrets/tokens/sensitive data included (`NODE_AUTH_TOKEN`, keys, DB creds)
- [ ] Backward compatibility considered (defaults, migrations, deprecations)

## Tests

<!-- Check what applies and include links to CI runs if useful. -->

- [ ] Tests are not required for this change (explain below)
- [ ] Unit tests added/updated
- [ ] Integration tests added/updated
- [ ] E2E tests added/updated
- [ ] Existing tests updated to reflect behavior changes
- [ ] `npm run test:all` executed successfully
- [ ] Relevant checks pass locally and/or in CI

### If tests were not added, explain why

<!-- e.g., docs-only change, no behavior change, covered by existing tests -->

## Observability / operations (if applicable)

- [ ] Logging is sufficient for troubleshooting
- [ ] Health/readiness behavior considered (`/health/live`, `/health/ready`)
- [ ] Runbook/dashboard/alert impact considered

## Security & privacy (if applicable)

- [ ] Input validation / authz / access control reviewed
- [ ] Dependency changes reviewed for risk (`npm audit` impact)
- [ ] No PII exposure introduced

## Release notes

- [ ] No release note needed
- [ ] Release note provided below

### Release note (if needed)

<!-- One sentence in user-facing language. -->
