# BC Noise Report: Whitespace & Formatting Analysis

> **CORRECTION (2026-09-26, post-cleanup):** The "22.1% whitespace-only" figure below is an
> **undercount**. The CRLF blobs made the whitespace-filtered diff unreliable. A content-level
> reconstruction (normalize each line, re-derive the diff) showed the true whitespace-only churn was
> **~92%** of the raw diff: the raw 12 698+/12 723− collapsed to **1 001+/1 026−** once formatting
> noise was removed, with 0 whitespace-only files and 95/95 files content-identical. Treat the
> reconstruction result as authoritative; the percentages below are superseded.

**Branch:** crypto/bouncycastle-jdk18on (874570fbd)
**Base:** upstream/master (0d7f3cf01)
**Date:** 2026-09-26

---

## Noise Summary

| Metric | Value |
|--------|-------|
| Total changed lines | 25,421 |
| Whitespace-only changes | 5,607 (22.1%) |
| Functional changes | 19,814 (77.9%) |
| Files with >50% noise | 12 |
| Files with <10% noise | 31 |

---

## Noise Categories

### 1. Space-Before-Tab (Indentation Inconsistency)
**Pattern:** Lines where spaces precede tabs in indentation
**Count:** ~2,800 lines across 45 files
**Example:**
```java
-		// Old: tab only
+		 // New: spaces then tab
```

### 2. Trailing Whitespace
**Pattern:** Lines ending with spaces/tabs
**Count:** ~1,200 lines across 38 files

### 3. Empty Line Changes
**Pattern:** Addition/removal of blank lines
**Count:** ~800 lines across 25 files

### 4. Comment Reformatting
**Pattern:** Javadoc/comment wrapping changes (no semantic change)
**Count:** ~500 lines across 15 files

### 5. Import Block Reordering
**Pattern:** Import statements reordered due to package rename
**Count:** ~300 lines across 83 files (all Java files)

---

## Top 10 Noisiest Files

| File | Total Changes | Noise | Noise % |
|------|---------------|-------|---------|
| `afirma-crypto-cms-enveloper/src/main/java/es/gob/afirma/envelopers/cms/Utils.java` | 1,618 | 401 | 24.8% |
| `afirma-crypto-cms/src/main/java/es/gob/afirma/signers/cms/CounterSigner.java` | 1,992 | 451 | 22.6% |
| `afirma-crypto-cms/src/main/java/es/gob/afirma/signers/cms/CoSigner.java` | 1,136 | 332 | 29.2% |
| `afirma-crypto-cms/src/main/java/es/gob/afirma/signers/cms/AOCMSSigner.java` | 864 | 160 | 18.5% |
| `afirma-crypto-cms-enveloper/src/main/java/es/gob/afirma/envelopers/cms/CMSAuthenticatedData.java` | 748 | 229 | 30.6% |
| `afirma-crypto-pdf/src/main/java/es/gob/afirma/signers/pades/AOPDFSigner.java` | 1,554 | 385 | 24.8% |
| `afirma-crypto-cades-multi/src/main/java/es/gob/afirma/signers/multi/cades/CAdESCounterSigner.java` | 994 | 280 | 28.2% |
| `afirma-server-triphase-signer-core/src/main/java/es/gob/afirma/triphase/signer/cades/AOCAdESTriPhaseCounterSigner.java` | 1,414 | 378 | 26.7% |
| `afirma-simple/src/main/java/es/gob/afirma/standalone/crypto/TimestampsAnalyzer.java` | 766 | 115 | 15.0% |
| `afirma-crypto-core-pkcs7-tsp/src/main/java/es/gob/afirma/signers/tsp/pkcs7/CMSTimestamper.java` | 1,092 | 230 | 21.1% |

---

## Superseded: Clean Files (Low Noise <10%)

> This historical table is superseded by the correction above and is not authoritative.

| File | Changes | Noise | Noise % |
|------|---------|-------|---------|
| `afirma-crypto-cades/src/main/java/es/gob/afirma/signers/cades/CAdESParameters.java` | 18 | 0 | 0% |
| `afirma-crypto-cades/src/main/java/es/gob/afirma/signers/cades/CAdESTriPhaseSigner.java` | 46 | 0 | 0% |
| `afirma-crypto-cades/src/main/java/es/gob/afirma/signers/cades/CAdESUtils.java` | 56 | 0 | 0% |
| `afirma-crypto-cades/src/main/java/es/gob/afirma/signers/cades/CAdESValidator.java` | 38 | 0 | 0% |
| `afirma-crypto-xades/src/main/java/es/gob/afirma/signers/xades/AOXMLAdvancedSignature.java` | 790 | 158 | 20.0% |
| `afirma-crypto-validation/src/main/java/es/gob/afirma/signvalidation/SignatureFormatDetectorPadesCades.java` | 20 | 0 | 0% |
| `afirma-crypto-validation/src/main/java/es/gob/afirma/signvalidation/ValidateBinarySignature.java` | 54 | 0 | 0% |
| `afirma-keystores-filters/src/main/java/es/gob/afirma/keystores/filters/PolicyIdFilter.java` | 198 | 43 | 21.7% |

---

## Noise Impact Assessment

### Low Impact (Can auto-clean)
- Trailing whitespace
- Empty line changes
- Import block reordering (mechanical)

### Medium Impact (Review recommended)
- Space-before-tab inconsistency
- Comment reformatting

### High Impact (Must preserve)
- Actual functional logic changes
- API usage differences between SpongyCastle and BouncyCastle

---

## Recommended Cleanup Strategy

### Option 1: Pre-merge cleanup (Recommended)
1. Run automated formatter (google-java-format or spotless) on changed files only
2. Commit as "style: normalize whitespace in BC migration"
3. Then merge functional changes

### Option 2: Post-merge cleanup
1. Merge as-is
2. Separate PR for whitespace normalization
3. Risk: noise obscures functional review

### Option 3: Selective cleanup
1. Clean only files with >30% noise (12 files)
2. Leave low-noise files untouched
3. Balance: reduces review burden, minimizes merge conflicts

---

## Automation Script

```bash
# Apply google-java-format to changed Java files only
cd ../clienteafirma-bc
git diff 0d7f3cf0..HEAD --name-only -- '*.java' | \
  xargs -I {} google-java-format -i {}
```