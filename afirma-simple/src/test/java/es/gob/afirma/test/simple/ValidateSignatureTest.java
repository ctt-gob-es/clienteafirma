package es.gob.afirma.test.simple;

import java.io.IOException;
import java.io.InputStream;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.signature.SignValidity;
import es.gob.afirma.signature.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signature.ValidateBinarySignature;
import es.gob.afirma.signature.ValidateXMLSignature;

/** Realiza pruebas de validaci&oacute;n del core de firma. */
public class ValidateSignatureTest {

	private static final String XMLDSIG_ENVELOPED = "xmldsig_enveloped_SHA1.xml"; //$NON-NLS-1$
	private static final String CADES_EXPLICIT = "cades_explicit.csig"; //$NON-NLS-1$

	private static final String[] SIGNATURES = new String[] {
		XMLDSIG_ENVELOPED,
		CADES_EXPLICIT
	};
	
	/** Prueba de validaci&oacute;n de firmas.
	 * @throws IOException */
	@SuppressWarnings("static-method")
	@Test
	public void testValidateXMLSign() throws IOException {

			String signaturePath = XMLDSIG_ENVELOPED;
		
			InputStream is = ClassLoader.getSystemResourceAsStream(signaturePath);
			final byte[] signature = AOUtil.getDataFromInputStream(is);
			is.close();
			SignValidity validity = ValidateXMLSignature.validate(signature);
			System.out.println(signaturePath + ":\n\t" + validity + "\n====================");
			Assert.assertEquals("No es valida la firma " + signaturePath, SIGN_DETAIL_TYPE.OK, validity.getValidity());
	}
	
	/** Prueba de validaci&oacute;n de firmas.
	 * @throws IOException */
	@SuppressWarnings("static-method")
	@Test
	public void testValidateBinarySign() throws IOException {

			String signaturePath = CADES_EXPLICIT;
		
			InputStream is = ClassLoader.getSystemResourceAsStream(signaturePath);
			final byte[] signature = AOUtil.getDataFromInputStream(is);
			is.close();
			SignValidity validity = ValidateBinarySignature.validate(signature, null);
			System.out.println(signaturePath + ":\n\t" + validity + "\n====================");
			Assert.assertEquals("No es valida la firma " + signaturePath, SIGN_DETAIL_TYPE.UNKNOWN, validity.getValidity());
	}
}
