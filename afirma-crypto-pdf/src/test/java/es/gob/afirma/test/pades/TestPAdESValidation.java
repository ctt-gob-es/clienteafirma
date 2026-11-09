package es.gob.afirma.test.pades;

import java.io.InputStream;

import org.junit.Test;
import static org.junit.Assert.*;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.signers.pades.AOPDFSigner;

/** Pruebas de validaci&oacute;n b&aacute;sica de firmas PAdES.
 * Ported from afirma-crypto-validation TestSignatureValidation. */
public class TestPAdESValidation {

	private static final String PADES_FILE = "pades.pdf";
	private static final String PADES_EPES_FILE = "pades_epes.pdf";
	private static final String PADES_NO_AUTOFIRMA_FILE = "pades_sin_autofirma.pdf";

	@Test
	public void testPadesValidation() throws Exception {
		try (final InputStream is = ClassLoader.getSystemResourceAsStream(PADES_FILE)) {
			final byte[] pades = AOUtil.getDataFromInputStream(is);
			assertNotNull("PAdES signature should not be null", pades);
			assertTrue("PAdES signature should not be empty", pades.length > 0);
			assertTrue("Debe reconocerse como un PDF firmado", new AOPDFSigner().isSign(pades));
		}
	}

	@Test
	public void testPadesEpesValidation() throws Exception {
		try (final InputStream is = ClassLoader.getSystemResourceAsStream(PADES_EPES_FILE)) {
			final byte[] pades = AOUtil.getDataFromInputStream(is);
			assertNotNull("PAdES-EPES signature should not be null", pades);
			assertTrue("PAdES-EPES signature should not be empty", pades.length > 0);
			assertTrue("Debe reconocerse como un PDF firmado", new AOPDFSigner().isSign(pades));
		}
	}

	@Test
	public void testValidarPadesGeneradaSinAutofirma() throws Exception {
		try (final InputStream is = ClassLoader.getSystemResourceAsStream(PADES_NO_AUTOFIRMA_FILE)) {
			final byte[] signature = AOUtil.getDataFromInputStream(is);
			assertNotNull("Signature should not be null", signature);
			assertTrue("Signature should not be empty", signature.length > 0);
			assertTrue("Debe reconocerse como un PDF firmado", new AOPDFSigner().isSign(signature));
		}
	}
}
