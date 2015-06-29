package es.gob.afirma.test.simple;

import java.io.IOException;
import java.io.InputStream;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.cert.signvalidation.SignValidity;
import es.gob.afirma.cert.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.cert.signvalidation.ValidateBinarySignature;
import es.gob.afirma.cert.signvalidation.ValidateXMLSignature;
import es.gob.afirma.core.misc.AOUtil;

/** Realiza pruebas de validaci&oacute;n del core de firma. */
public class ValidateSignatureTest {

	private static final String XMLDSIG_ENVELOPED = "xmldsig_enveloped_SHA1.xml"; //$NON-NLS-1$
	private static final String CADES_EXPLICIT = "cades_explicit.csig"; //$NON-NLS-1$

	/** Prueba de validaci&oacute;n de firmas.
	 * @throws IOException En errores de tratamiento de datos. */
	@SuppressWarnings("static-method")
	@Test
	public void testValidateXMLSign() throws IOException {

			final String signaturePath = XMLDSIG_ENVELOPED;
			final byte[] signature;
			try ( final InputStream is = ClassLoader.getSystemResourceAsStream(signaturePath); ) {
				signature = AOUtil.getDataFromInputStream(is);
			}
			final SignValidity validity = ValidateXMLSignature.validate(signature);
			System.out.println(signaturePath + ":\n\t" + validity + "\n====================");  //$NON-NLS-1$//$NON-NLS-2$
			Assert.assertEquals("No es valida la firma " + signaturePath, SIGN_DETAIL_TYPE.OK, validity.getValidity()); //$NON-NLS-1$
	}

	/** Prueba de validaci&oacute;n de firmas.
	 * @throws IOException En errores de tratamiento de datos. */
	@SuppressWarnings("static-method")
	@Test
	public void testValidateBinarySign() throws IOException {

			final String signaturePath = CADES_EXPLICIT;
			final byte[] signature;
			try ( final InputStream is = ClassLoader.getSystemResourceAsStream(signaturePath); ) {
				signature = AOUtil.getDataFromInputStream(is);
			}
			final SignValidity validity = ValidateBinarySignature.validate(signature, null);
			System.out.println(signaturePath + ":\n\t" + validity + "\n===================="); //$NON-NLS-1$ //$NON-NLS-2$
			Assert.assertEquals("No es valida la firma " + signaturePath, SIGN_DETAIL_TYPE.UNKNOWN, validity.getValidity()); //$NON-NLS-1$
	}
}
