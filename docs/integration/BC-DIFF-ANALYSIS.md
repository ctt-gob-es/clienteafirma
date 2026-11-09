# BC Diff Analysis: crypto/bouncycastle-jdk18on vs upstream/master

**Branch:** crypto/bouncycastle-jdk18on (874570fbd)
**Base:** upstream/master (0d7f3cf01)
**Date:** 2026-09-26

---

## Executive Summary

The BouncyCastle migration branch contains **95 changed files** with **2,027 lines changed**
(1,001 added, 1,026 removed) after Strategy-B cleanup (see BC-STRATEGY.md).

- **Whitespace-only files:** 0 (verified: 95/95 files content-identical after stripping whitespace)
- **Functional changes:** 1,001 added / 1,026 removed
- **Modules affected:** 17 modules + root POM

> **Historical note:** the raw pre-cleanup diff was 12,698 added / 12,723 removed (~25,421 lines,
> ~92% whitespace-only churn from CRLF/indent noise). The module breakdown below reflects the
> **clean** scope and sums exactly to the totals above.

---

## Module Breakdown

| Module | Files | Added | Removed | Type |
|--------|-------|-------|---------|------|
| afirma-core-massive | 1 | 1 | 1 | Batch/massive |
| afirma-crypto-cades | 12 | 114 | 116 | Core crypto |
| afirma-crypto-cades-multi | 8 | 94 | 96 | Core crypto |
| afirma-crypto-cms | 8 | 116 | 119 | Core crypto |
| afirma-crypto-cms-enveloper | 23 | 246 | 246 | Core crypto |
| afirma-crypto-core-pkcs7 | 10 | 85 | 87 | Core crypto |
| afirma-crypto-core-pkcs7-tsp | 3 | 22 | 25 | Core crypto |
| afirma-crypto-jarverifier | 2 | 10 | 13 | JAR verify |
| afirma-crypto-pdf | 2 | 2 | 2 | Core crypto |
| afirma-crypto-validation | 4 | 32 | 34 | Validation |
| afirma-crypto-xades | 2 | 2 | 2 | Core crypto |
| afirma-keystores-filters | 4 | 19 | 21 | Keystore |
| afirma-server-triphase-signer-core | 4 | 62 | 62 | Triphase |
| afirma-simple | 5 | 79 | 82 | Product |
| afirma-simple-plugin-validatecerts | 3 | 43 | 46 | Validation |
| afirma-ui-applet | 1 | 34 | 34 | UI |
| afirma-ui-simple-configurator | 2 | 30 | 30 | UI |
| Root POM | 1 | 10 | 10 | Config |
| **Total** | **95** | **1,001** | **1,026** | |

---

## Dependency Migration (POM Changes)

All crypto modules migrate from **SpongyCastle** to **BouncyCastle jdk18on**:

### Before (SpongyCastle)
```xml
<dependency>
    <groupId>com.madgag.spongycastle</groupId>
    <artifactId>core</artifactId>
</dependency>
<dependency>
    <groupId>com.madgag.spongycastle</groupId>
    <artifactId>prov</artifactId>
</dependency>
<dependency>
    <groupId>com.madgag.spongycastle</groupId>
    <artifactId>bcpkix-jdk15on</artifactId>
</dependency>
```

### After (BouncyCastle jdk18on)
```xml
<dependency>
    <groupId>org.bouncycastle</groupId>
    <artifactId>bcprov-jdk18on</artifactId>
</dependency>
<dependency>
    <groupId>org.bouncycastle</groupId>
    <artifactId>bcutil-jdk18on</artifactId>
</dependency>
<dependency>
    <groupId>org.bouncycastle</groupId>
    <artifactId>bcpkix-jdk18on</artifactId>
</dependency>
```

**Affected modules:** afirma-crypto-cades, afirma-crypto-cades-multi, afirma-crypto-cms, afirma-crypto-cms-enveloper, afirma-crypto-core-pkcs7, afirma-crypto-core-pkcs7-tsp, afirma-crypto-pdf, afirma-crypto-xades, afirma-crypto-validation, afirma-crypto-jarverifier, afirma-keystores-filters, afirma-server-triphase-signer-core, afirma-simple-plugin-validatecerts, afirma-simple, afirma-ui-simple-configurator, afirma-ui-applet, afirma-core-massive

---

## Java Import Migration Pattern

**Systematic find/replace across all 83 Java files:**

| Old Import | New Import |
|------------|------------|
| `org.spongycastle.*` | `org.bouncycastle.*` |

**Examples:**
- `org.spongycastle.asn1.*` → `org.bouncycastle.asn1.*`
- `org.spongycastle.cms.*` → `org.bouncycastle.cms.*`
- `org.spongycastle.jce.*` → `org.bouncycastle.jce.*`
- `org.spongycastle.operator.*` → `org.bouncycastle.operator.*`
- `org.spongycastle.pkcs.*` → `org.bouncycastle.pkcs.*`
- `org.spongycastle.tsp.*` → `org.bouncycastle.tsp.*`
- `org.spongycastle.util.*` → `org.bouncycastle.util.*`
- `org.spongycastle.x509.*` → `org.bouncycastle.x509.*`

---

## Functional Change Categories

### 1. Core CMS/CAdES Signing (High Impact)
- `CounterSigner.java` - Complete rewrite due to import changes
- `CMSAuthenticatedData.java` / `CMSAuthenticatedEnvelopedData.java` - Authenticated enveloped data
- `GenSignedData.java` - Signed data generation
- `CoSigner.java` / `CounterSigner.java` - Co-signing logic
- `AOCMSSigner.java` / `AOPDFSigner.java` / `AOXMLAdvancedSignature.java` - High-level signers

### 2. Timestamping (High Impact)
- `CMSTimestamper.java` - RFC 3161 timestamping

### 3. Certificate Validation (Medium Impact)
- `CrlHelper.java` / `OcspHelper.java` - CRL/OCSP validation
- `CertHolderBySignerIdSelector.java` - Cert selector
- `ValidateBinarySignature.java` / `ValidateCMS.java` - Validation entry points

### 4. Triphase Signing (Medium Impact)
- `AOCAdESTriPhaseCoSigner.java` / `AOCAdESTriPhaseCounterSigner.java` - Triphase co-signing
- `KeyHelperEcdsa.java` / `KeyHelperFactory.java` - ECDSA key handling

### 5. Utilities (Lower Impact)
- `Utils.java` (CMS enveloper) - Large utility class
- `TimestampsAnalyzer.java` - Timestamp analysis
- `CertUtil.java` (simple & configurator) - Certificate utilities

---

## Test Coverage Gap

**Critical Finding:** The CI (`build-fork-bc.yml`) uses `-Dmaven.test.skip=true` which:
1. Skips test compilation entirely
2. Skips test execution
3. **Test dependencies (JUnit) are missing from crypto module POMs**

Modules missing JUnit test dependency:
- afirma-crypto-cades
- afirma-crypto-cades-multi
- afirma-crypto-cms
- afirma-crypto-cms-enveloper
- afirma-crypto-core-pkcs7
- afirma-crypto-core-pkcs7-tsp
- afirma-crypto-pdf
- afirma-crypto-xades
- afirma-crypto-validation
- afirma-crypto-jarverifier
- afirma-keystores-filters
- afirma-server-triphase-signer-core
- afirma-simple-plugin-validatecerts

Only `afirma-core` has JUnit dependency declared.

---

## Build Verification

✅ **Full build with `-Dmaven.test.skip=true`:** SUCCESS
✅ **Product JAR (`afirma-simple/target/autofirma.jar`):** Built (54MB)
✅ **All modules compile:** SUCCESS
❌ **Test compilation:** FAILS (missing JUnit dependencies)
❌ **Test execution:** NOT VERIFIED

---

## Risk Assessment

| Risk | Severity | Mitigation |
|------|----------|------------|
| Whitespace noise (~92% of raw diff) | Medium | ✅ Strategy B applied: clean diff 1 001+/1 026−, 0 whitespace-only files |
| Missing test dependencies | High | Add JUnit to affected POMs |
| No test execution in CI | High | Enable test phase in CI |
| Large functional surface | High | Incremental validation with test vectors |
| Triphase modules untested | Medium | Mark as EXPERIMENTAL (P3) |

---

## Recommendations

See [BC-STRATEGY.md](./BC-STRATEGY.md) for merge strategy options.