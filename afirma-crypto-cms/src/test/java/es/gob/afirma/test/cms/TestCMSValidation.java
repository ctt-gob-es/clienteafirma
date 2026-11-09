package es.gob.afirma.test.cms;

import java.io.InputStream;
import java.util.List;

import org.junit.Test;
import static org.junit.Assert.*;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.signers.cms.ValidateCMSSignedData;

/** Pruebas de validaci&oacute;n de firmas CMS/CAdES.
 * Ported from afirma-crypto-validation TestSignatureValidation. */
public class TestCMSValidation {

	private static final String CADES_IMPLICIT_FILE = "cades_implicit.csig";
	private static final String CADES_EXPLICIT_FILE = "cades_explicit.csig";
	private static final String DATA_TXT_FILE = "txt";

	@Test
	public void testCadesImplicitValidation() throws Exception {
		try (final InputStream is = ClassLoader.getSystemResourceAsStream(CADES_IMPLICIT_FILE)) {
			final byte[] cades = AOUtil.getDataFromInputStream(is);
			final List<ValidateCMSSignedData.ValidationResult> result = ValidateCMSSignedData.validate(cades, false);
			assertNotNull(result);
			assertFalse(result.isEmpty());
			assertTrue(result.get(0).valid);
		}
	}

	@Test
	public void testCadesExplicitValidationWithoutData() throws Exception {
		try (final InputStream is = ClassLoader.getSystemResourceAsStream(CADES_EXPLICIT_FILE)) {
			final byte[] cades = AOUtil.getDataFromInputStream(is);
			final List<ValidateCMSSignedData.ValidationResult> result = ValidateCMSSignedData.validate(cades, false);
			assertNotNull(result);
			assertFalse(result.isEmpty());
			assertTrue(result.get(0).valid);
		}
	}

	@Test
	public void testCadesExplicitValidationWithData() throws Exception {
		try (final InputStream is = ClassLoader.getSystemResourceAsStream(CADES_EXPLICIT_FILE);
		     final InputStream dataIs = ClassLoader.getSystemResourceAsStream(DATA_TXT_FILE)) {
			final byte[] cades = AOUtil.getDataFromInputStream(is);
			final byte[] data = AOUtil.getDataFromInputStream(dataIs);
			final List<ValidateCMSSignedData.ValidationResult> result = ValidateCMSSignedData.validate(cades, data, false);
			assertNotNull(result);
			assertFalse(result.isEmpty());
			assertTrue(result.get(0).valid);
		}
	}

	@Test
	public void testCadesExplicitValidationWrongData() throws Exception {
		try (final InputStream is = ClassLoader.getSystemResourceAsStream(CADES_EXPLICIT_FILE)) {
			final byte[] cades = AOUtil.getDataFromInputStream(is);
			final List<ValidateCMSSignedData.ValidationResult> result = ValidateCMSSignedData.validate(cades, "dummy2".getBytes(), false);
			assertNotNull(result);
			assertFalse(result.isEmpty());
			assertTrue(result.get(0).valid);
		}
	}
}
