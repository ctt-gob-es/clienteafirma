package es.gob.afirma.cert.signvalidation;

import java.io.InputStream;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.signvalidation.SignValiderFactory;
import es.gob.afirma.signvalidation.ValidateBinarySignature;

/** Pruebas de validaci&oacute;n de firmas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public class TestSignatureValidation {

	private static final String CADES_IMPLICIT_FILE = "cades_implicit.csig"; //$NON-NLS-1$
	private static final String CADES_EXPLICIT_FILE = "cades_explicit.csig"; //$NON-NLS-1$
	private static final String DATA_TXT_FILE = "txt"; //$NON-NLS-1$
	private static final String PADES_FILE = "pades.pdf"; //$NON-NLS-1$
	private static final String PADES_EPES_FILE = "pades_epes.pdf"; //$NON-NLS-1$
	private static final String XADES_EPES_FILE = "xades_epes_detached.xsig"; //$NON-NLS-1$

	/** Prueba de validaci&oacute;n de firma CAdES.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testCadesImplicitValidation() throws Exception {
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream(CADES_IMPLICIT_FILE);
		) {
			final byte[] cades = AOUtil.getDataFromInputStream(is);
			System.out.println(new ValidateBinarySignature().validate(cades, false));
		}
	}

	/** Prueba de validaci&oacute;n de firma CAdES explicita sin datos.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testCadesExplicitValidationWithoutData() throws Exception {
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream(CADES_EXPLICIT_FILE);
		) {
			final byte[] cades = AOUtil.getDataFromInputStream(is);
			System.out.println(new ValidateBinarySignature().validate(cades, false));
		}
	}

	/** Prueba de validaci&oacute;n de firma CAdES expl&iacute;cita con los datos.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testCadesExplicitValidationWithData() throws Exception {
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream(CADES_EXPLICIT_FILE);
			final InputStream dataIs = ClassLoader.getSystemResourceAsStream(DATA_TXT_FILE);
		) {
			final byte[] cades = AOUtil.getDataFromInputStream(is);
			final byte[] data = AOUtil.getDataFromInputStream(dataIs);
			System.out.println(ValidateBinarySignature.validate(cades, data, false));
		}
	}

	/** Prueba de validaci&oacute;n de firma CAdES expl&iacute;cita con datos erroneos.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testCadesExplicitValidationWrongData() throws Exception {
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream(CADES_EXPLICIT_FILE);
		) {
			final byte[] cades = AOUtil.getDataFromInputStream(is);
			System.out.println(ValidateBinarySignature.validate(cades, "dummy2".getBytes(), false));
		}
	}

	/** Prueba de validaci&oacute;n de firma PAdES.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testPadesValidation() throws Exception {
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream(PADES_FILE);
		) {
			final byte[] pades = AOUtil.getDataFromInputStream(is);
			System.out.println(SignValiderFactory.getSignValider(pades).validate(pades, false));
		}
	}

	/** Prueba de validaci&oacute;n de firma PAdES-EPES.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testPadesEpesValidation() throws Exception {
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream(PADES_EPES_FILE);
		) {
			final byte[] pades = AOUtil.getDataFromInputStream(is);
			System.out.println(SignValiderFactory.getSignValider(pades).validate(pades, false));
		}
	}

	/** Prueba de validaci&oacute;n de firma PAdES-EPES.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testXadesEpesValidation() throws Exception {
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream(XADES_EPES_FILE);
		) {
			final byte[] signature = AOUtil.getDataFromInputStream(is);
			System.out.println(SignValiderFactory.getSignValider(signature).validate(signature, false));
		}
	}
}

