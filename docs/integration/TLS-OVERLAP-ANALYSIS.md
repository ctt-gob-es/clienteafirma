# TLS Overlap Analysis: PR #543 / #546 vs prefs/strict-ssl

**Date:** 2026-09-26
**Context:** P1-TLS - Analysis only (no code changes)

---

## Branch Analysis

### prefs/strict-ssl (695268c26)
**Commit:** `prefs: add strictSslChecks opt-in (default false)`

**Changes:**
- Added `strictSslChecks` preference (default: `false`)
- Modified TLS/SSL verification behavior to be opt-in strict
- Files changed: prefs-related configuration

**Purpose:** Allow users to enable strict SSL certificate verification without breaking existing integrations.

---

### PR #543 / #546 (Upstream)
**Status:** Both PRs are OPEN upstream (verified 2026-09-27 via `gh pr view`)

**Confirmed scope:**
- PR #543 ("Usar TLS en lugar de SSL en SSLContext"): replaces the `SSL` context algorithm with
  `TLS` in `afirma-core/.../misc/http/{DataDownloader,SslSecurityManager}.java`
- PR #546 ("Advertir en logs cuando se deshabilitan comprobaciones SSL"): adds warning logs when
  SSL checks are disabled; touches `DataDownloader.java`, `SslSecurityManager.java` and
  `afirma-crypto-core-pkcs7-tsp/.../CMSTimestamper.java`. No validation-logic changes.

---

## Overlap Assessment

### Potential Overlap Areas
1. **SSL/TLS Configuration** - Both touch certificate verification
2. **Default Behavior** - strict-ssl defaults to `false`, PRs may change defaults
3. **API Surface** - Configuration property names and locations

### Decision Framework

| Scenario | Action |
|----------|--------|
| PRs are merged upstream | Rebase strict-ssl on top, resolve conflicts |
| PRs change default to `true` | Keep strict-ssl default `false` (user decision) |
| PRs add new TLS APIs | Ensure strict-ssl uses new APIs |
| PRs are WIP/stalled | Proceed with strict-ssl as-is |

---

## Recommended Next Steps

1. ~~**Fetch upstream PRs**~~ ✅ DONE (2026-09-27: both open, file lists confirmed)
2. **Deep-compare diffs** against `prefs/strict-ssl` hunk-by-hunk before any rebase
3. **Coordinate with upstream maintainers** on TLS strategy
4. **No code changes** until overlap is confirmed

---

## Current Status

**Decision:** P1-TLS = Analysis only, no code changes
**Reason:** User decision - "TLS: maintain strictSslChecks=false, default no changes"
**Coordination:** Track with #567 for A11Y coordination

---

## Files to Monitor

- `afirma-core-prefs/src/main/java/es/gob/afirma/core/prefs/` - Preferences
- `afirma-core/src/main/java/es/gob/afirma/core/misc/http/` - HTTP/TLS handling
- Any new TLS utility classes from PRs