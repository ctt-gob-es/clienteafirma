package es.gob.afirma.cert.signvalidation;

import static org.junit.Assert.assertEquals;

import java.io.InputStream;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.signvalidation.SignValiderFactory;
import es.gob.afirma.signvalidation.SignValidity;

public class Test_PAdES_Inc475804 {

	private static final String PADES_NO_GENERADA_POR_AUTOFIRMA = "pades_sin_autofirma.pdf"; //$NON-NLS-1$

	/**
	 * La firma debe fallar por certificado caducado y no por cualquier otro error.
	 * @throws Exception Cuando ocurre algun error no previsto.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void testValidarPadesGeneradaSinAutofirma() throws Exception {
		try (
				final InputStream is = ClassLoader.getSystemResourceAsStream(PADES_NO_GENERADA_POR_AUTOFIRMA);
			) {
				final byte[] signature = AOUtil.getDataFromInputStream(is);
				final SignValidity validity = SignValiderFactory.getSignValider(signature).validate(signature).get(0);
				assertEquals(SignValidity.VALIDITY_ERROR.CERTIFICATE_EXPIRED, validity.getError());
			}
	}
}
