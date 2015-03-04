package es.gob.afirma.test.simple;

import java.io.IOException;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.signature.SignValidity;
import es.gob.afirma.signature.ValidateXMLSignature;

/** Realiza pruebas de validaci&oacute;n del core de firma. */
public class ValidateSignatureTest {

	private static final String XMLDSIG_ENVELOPED = "xmldsig_enveloped_SHA1.xml"; //$NON-NLS-1$

	/** Prueba de validaci&oacute;n de firmas.
	 * @throws IOException */
	@SuppressWarnings("static-method")
	@Test
	public void testValidateSign() throws IOException {

		final byte[] signature = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(XMLDSIG_ENVELOPED));
		SignValidity validity = ValidateXMLSignature.validate(signature);
		System.out.println(validity);
	}
}
