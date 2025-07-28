package es.gob.afirma.test.simple;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.RuntimeConfigNeededException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.signvalidation.SignValider;
import es.gob.afirma.signvalidation.SignValiderFactory;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.standalone.SimpleAfirma;

/** Realiza pruebas de validaci&oacute;n del core de firma. */
public class ValidateSignatureTest {

	private static final String XMLDSIG_ENVELOPED = "xmldsig_enveloped_SHA1.xml"; //$NON-NLS-1$
	private static final String CADES_EXPLICIT = "cades_explicit.csig"; //$NON-NLS-1$

	//private static final String CERT_PEM = "testCer-B64.cer"; //$NON-NLS-1$
	//private static final String CERT_DER = "google.com.cer"; //$NON-NLS-1$

	//private static final String TSPDF = "TSA-5989068450533665227.pdf"; //$NON-NLS-1$
	//private static final String TSPDF2 = "TSA-2.pdf"; //$NON-NLS-1$
	//private static final String COPDF = "DOSFIRMAS.pdf"; //$NON-NLS-1$
	private static final String CADEST = "CAdES-T.asn1"; //$NON-NLS-1$


	/** Prueba de arranque del UI de validaci&oacute;n desde l&iacute;nea de comandos.
	 * @param args No se usa.
	 * @throws Exception En cualquier error. */
	public static void main(final String[] args) throws Exception {
		SimpleAfirma.main(
			new String[] {
				//"verify", "-gui", "-i", new java.io.File(ClassLoader.getSystemResource(XMLDSIG_ENVELOPED).toURI()).getAbsolutePath()  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				//"verify", "-gui", "-i", new java.io.File(ClassLoader.getSystemResource(CERT_DER).toURI()).getAbsolutePath()  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				//"verify", "-gui", "-i", new java.io.File(ClassLoader.getSystemResource(CERT_PEM).toURI()).getAbsolutePath()  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				"verify", "-gui", "-i", new java.io.File(ClassLoader.getSystemResource(CADEST).toURI()).getAbsolutePath()  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			}
		);
	}

	/** Prueba de validaci&oacute;n de firmas.
	 * @throws IOException En errores de tratamiento de datos.
	 * @throws RuntimeConfigNeededException No deberia ocurrir nunca. */
	@SuppressWarnings("static-method")
	@Test
	public void testValidateXMLSign() throws IOException, RuntimeConfigNeededException {

			final String signaturePath = XMLDSIG_ENVELOPED;
			final byte[] signature;
			try ( final InputStream is = ClassLoader.getSystemResourceAsStream(signaturePath); ) {
				signature = AOUtil.getDataFromInputStream(is);
			}
			final SignValider valider = SignValiderFactory.getSignValider(signature);
			if (valider == null) {
				throw new IOException(
					"La firma del fichero no se puede comprobar" //$NON-NLS-1$
				);
			}
			final List<SignValidity> validityList = valider.validate(signature, false);
			Assert.assertNotNull("El listado de resultado de validacion es nulo", validityList); //$NON-NLS-1$
			Assert.assertFalse("No se han devuelto resultados de validacion", validityList.isEmpty()); //$NON-NLS-1$

			boolean isValid = false;
			for (final SignValidity validity : validityList) {
				if (validity.getValidity() == SIGN_DETAIL_TYPE.OK) {
					isValid = true;
				}
			}

			Assert.assertTrue("No es valida la firma XAdES: " + signaturePath, isValid); //$NON-NLS-1$
	}

	/** Prueba de validaci&oacute;n de firmas.
	 * @throws IOException En errores de tratamiento de datos.
	 * @throws RuntimeConfigNeededException No deberia ocurrir nunca. */
	@SuppressWarnings("static-method")
	@Test
	public void testValidateBinarySign() throws IOException, RuntimeConfigNeededException {

			final String signaturePath = CADES_EXPLICIT;
			final byte[] signature;
			try ( final InputStream is = ClassLoader.getSystemResourceAsStream(signaturePath); ) {
				signature = AOUtil.getDataFromInputStream(is);
			}
			final SignValider valider = SignValiderFactory.getSignValider(signature);
			if (valider == null) {
				throw new IOException(
					"La firma del fichero no se puede comprobar" //$NON-NLS-1$
				);
			}
			final List<SignValidity> validityList = valider.validate(signature, true);
			Assert.assertNotNull("El listado de resultado de validacion es nulo", validityList); //$NON-NLS-1$
			Assert.assertFalse("No se han devuelto resultados de validacion", validityList.isEmpty()); //$NON-NLS-1$

			boolean isIndeterminate = false;
			for (final SignValidity validity : validityList) {
				if (validity.getValidity() == SIGN_DETAIL_TYPE.UNKNOWN) {
					isIndeterminate = true;
				}
			}

			Assert.assertTrue("La validacion de firmas CAdES explicitas deberia dar un resultado indeterminado: " + signaturePath, isIndeterminate); //$NON-NLS-1$
	}
}
