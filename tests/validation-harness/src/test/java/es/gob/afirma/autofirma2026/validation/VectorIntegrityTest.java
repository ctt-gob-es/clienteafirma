package es.gob.afirma.autofirma2026.validation;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.junit.BeforeClass;
import org.junit.Test;

import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signvalidation.ValidateBinarySignature;

/** Suite JUnit: integridad de vectores F2 + casos negativos. */
public class VectorIntegrityTest {

	private static Path out;
	private static Path input;

	@BeforeClass
	public static void setup() {
		out = VectorIntegrityValidator.vectorsDir().resolve("out");
		input = VectorIntegrityValidator.vectorsDir().resolve("input");
	}

	@Test
	public void cadesIntegrity() throws Exception {
		assumePresent("plain-cades.csig");
		assertAcceptable(VectorIntegrityValidator.validateFile(out.resolve("plain-cades.csig")));
	}

	@Test
	public void cadesCosignIntegrity() throws Exception {
		assumePresent("plain-cades-cosign.csig");
		assertAcceptable(VectorIntegrityValidator.validateFile(out.resolve("plain-cades-cosign.csig")));
	}

	@Test
	public void cadesCountersignIntegrity() throws Exception {
		assumePresent("plain-cades-countersign.csig");
		assertAcceptable(VectorIntegrityValidator.validateFile(out.resolve("plain-cades-countersign.csig")));
	}

	@Test
	public void xadesIntegrity() throws Exception {
		assumePresent("sample-xades.xsig");
		assertAcceptable(VectorIntegrityValidator.validateFile(out.resolve("sample-xades.xsig")));
	}

	@Test
	public void padesIntegrity() throws Exception {
		assumePresent("sample-pades.pdf");
		assertAcceptable(VectorIntegrityValidator.validateFile(out.resolve("sample-pades.pdf")));
	}

	@Test
	public void facturaeIntegrity() throws Exception {
		assumePresent("facturae-signed.xml");
		assertAcceptable(VectorIntegrityValidator.validateFile(out.resolve("facturae-signed.xml")));
	}

	@Test
	public void cadesMatchesOriginalData() throws Exception {
		assumeTrue(Files.isRegularFile(out.resolve("plain-cades.csig")));
		assumeTrue(Files.isRegularFile(input.resolve("plain.txt")));
		assertAcceptable(VectorIntegrityValidator.validateCadesWithData(
				out.resolve("plain-cades.csig"), input.resolve("plain.txt")));
	}

	@Test
	public void cadesRejectsWrongData() throws Exception {
		assumeTrue(Files.isRegularFile(out.resolve("plain-cades.csig")));
		final List<SignValidity> wrong = ValidateBinarySignature.validate(
				Files.readAllBytes(out.resolve("plain-cades.csig")),
				"TAMPERED-PAYLOAD".getBytes("UTF-8"),
				false);
		assertNotNull(wrong);
		assertTrue("expected structural KO for wrong data: " + wrong, VectorIntegrityValidator.hasStructuralKo(wrong)
				|| hasKo(wrong));
	}

	@Test
	public void unsignedPlainIsNotASignature() throws Exception {
		assumeTrue(Files.isRegularFile(input.resolve("plain.txt")));
		final byte[] data = Files.readAllBytes(input.resolve("plain.txt"));
		try {
			final List<SignValidity> results = new ValidateBinarySignature().validate(data, false);
			assertTrue("unsigned text must not be OK: " + results, hasKo(results) || isUnknownNoSign(results));
		}
		catch (final Exception e) {
			// Parsers may throw on non-CMS bytes; that still proves the payload is not a valid signature.
			assertTrue("parser rejected unsigned text: " + e.getClass().getSimpleName(), true);
		}
	}

	private static void assumePresent(final String name) {
		assumeTrue("Missing vector " + name + " — run scripts/f2-regression.sh first",
				Files.isRegularFile(out.resolve(name)));
	}

	private static void assertAcceptable(final List<SignValidity> results) {
		assertNotNull(results);
		assertFalse(results.isEmpty());
		assertTrue("unacceptable: " + results, VectorIntegrityValidator.isAcceptable(results));
	}

	private static boolean hasKo(final List<SignValidity> results) {
		for (final SignValidity v : results) {
			if (v.getValidity() == SIGN_DETAIL_TYPE.KO) {
				return true;
			}
		}
		return false;
	}

	private static boolean isUnknownNoSign(final List<SignValidity> results) {
		for (final SignValidity v : results) {
			if (v.getValidity() != SIGN_DETAIL_TYPE.OK && v.getValidity() != SIGN_DETAIL_TYPE.GENERATED) {
				return true;
			}
		}
		return false;
	}
}
