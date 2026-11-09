# BC Merge Strategy: crypto/bouncycastle-jdk18on → upstream/master

**Branch:** crypto/bouncycastle-jdk18on (874570fbd)
**Target:** upstream/master (0d7f3cf01)
**Date:** 2026-09-26

---

## Strategy Options

### Strategy A: Squash Merge (Single Commit)
**Pros:**
- Clean history, single reviewable commit
- Easy to revert if issues

**Cons:**
- Loses commit history/granularity
- Harder to bisect regressions
- Large diff in one PR (~25K lines raw / ~2K clean)

**Recommendation:** ❌ Not recommended for this scale

---

### Strategy B: Clean + Rebase (Recommended)
**Steps:**
1. Create cleanup branch from upstream/master
2. Apply automated whitespace normalization to BC branch files
3. Rebase functional commits on top
4. Force-push cleaned BC branch
5. PR with clean diff

**Pros:**
- Preserves commit history
- Clean diff for review
- Noise separated from function
- Bisectable

**Cons:**
- Requires force-push (coordinate with team)
- Rebase conflicts possible

**Recommendation:** ✅ **PRIMARY STRATEGY**

---

### Strategy C: Incremental Module PRs
**Split by module groups:**
1. PR 1: Core dependencies (POM changes only)
2. PR 2: CMS/CAdES crypto modules
3. PR 3: PDF/XAdES/Validation modules
4. PR 4: Triphase/Simple/Product modules

**Pros:**
- Small, reviewable PRs
- Isolated risk per module
- Can merge independently

**Cons:**
- 4+ PRs to manage
- Cross-module dependencies
- Temporary broken state between PRs

**Recommendation:** ✅ **ALTERNATIVE** if team prefers small PRs

---

### Strategy D: Feature Flag + Gradual Rollout
**Approach:**
1. Add BC as optional profile (`-Pbouncycastle`)
2. Keep SpongyCastle as default
3. Validate with test vectors
4. Switch default after validation

**Pros:**
- Zero-risk rollout
- Side-by-side comparison
- Rollback trivial

**Cons:**
- Double maintenance burden
- Complex build configuration
- Not aligned with migration goal

**Recommendation:** ❌ Over-engineering for this migration

---

## Recommended Execution Plan (Strategy B)

### Phase 1: Automated Cleanup (Day 1)
```bash
# In clienteafirma-bc worktree
cd ../clienteafirma-bc

# 1. Identify changed Java files
CHANGED_JAVA=$(git diff 0d7f3cf0..HEAD --name-only -- '*.java')

# 2. Apply formatter (google-java-format or spotless)
# Requires: google-java-format JAR or maven plugin
for f in $CHANGED_JAVA; do
  google-java-format -i "$f"
done

# 3. Fix trailing whitespace
for f in $CHANGED_JAVA; do
  sed -i 's/[[:space:]]*$//' "$f"
done

# 4. Stage and commit
git add $CHANGED_JAVA
git commit -m "style: normalize whitespace in BC migration files"

# 5. Verify build still passes
mvn -B clean install -Dmaven.test.skip=true
```

### Phase 2: Test Dependency Fix (Day 1-2)
Add JUnit to all affected crypto module POMs:
```xml
<dependency>
    <groupId>junit</groupId>
    <artifactId>junit</artifactId>
    <scope>test</scope>
</dependency>
```
**Modules:** 13 modules (see BC-DIFF-ANALYSIS.md)

### Phase 3: Test Compilation & Execution (Day 2)
```bash
# Compile tests
mvn -B test-compile -pl <crypto-modules>

# Run tests
mvn -B test -pl <crypto-modules>
```

### Phase 4: Validation Harness (Day 3)
Run validation vectors against BC JAR:
```bash
# Using afirma-crypto-validation vectors
# Compare BC output vs upstream output
```

### Phase 5: PR Creation (Day 3-4)
1. Push cleaned branch to fork
2. Create PR against upstream/master
3. Request review from crypto maintainers

---

## Validation Checklist

### Pre-Merge Gates
- [ ] Full build passes (`mvn clean install -DskipTests`)
- [ ] All test dependencies declared
- [ ] Test compilation passes
- [ ] Core module tests pass (32/32)
- [ ] Crypto module tests pass (target: 100%)
- [ ] Validation vectors match (CAdES, PAdES, XAdES, CMS)
- [ ] Product JAR builds and runs (smoke test)
- [ ] No regression in F2 tests (if available)

### Risk Mitigation
| Risk | Mitigation |
|------|------------|
| BC API incompatibility | Run validation vectors; check BC 1.78.1 migration guide |
| Performance regression | Benchmark sign/verify operations |
| Certificate chain issues | Test with real DNIe/FNMT certs |
| Timestamping failures | Validate RFC 3161 compliance |

---

## Timeline Estimate

| Phase | Duration | Owner |
|-------|----------|-------|
| Automated cleanup | 2 hours | Automation |
| Test dependency fix | 1 hour | Dev |
| Test compilation/execution | 4 hours | Dev/QA |
| Validation vectors | 4 hours | QA |
| PR review/merge | 2-5 days | Team |
| **Total** | **3-5 days** | |

---

## Decision Matrix

| Factor | Weight | Strategy B | Strategy C |
|--------|--------|------------|------------|
| Reviewability | High | ✅ Clean diff | ✅ Small diffs |
| History preservation | Medium | ✅ Full history | ✅ Full history |
| Implementation effort | Medium | Low (scripted) | High (manual split) |
| Merge conflict risk | Medium | Low (single rebase) | Medium (sequential) |
| Rollback simplicity | High | Single revert | Multi revert |
| Team coordination | Low | Force-push once | Multiple PRs |

**Final Recommendation:** **Strategy B (Clean + Rebase)** with automated whitespace normalization, followed by test dependency fixes and validation vector execution.