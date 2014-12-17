package es.gob.afirma.applet;

import java.io.IOException;
import java.util.Properties;

import javax.xml.crypto.dsig.DigestMethod;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;

/**
 * Comprueba las incidencias 137182 y 175231 abierta contra el Applet de firma.
 */
public class Ticket137182_175231Test {

	private static final String URN_OID_PREFIX = "urn:oid:"; //$NON-NLS-1$

	private static final String INPUT_DATA_OID = "2.16.724.1.3.1.1.2.1.8"; //$NON-NLS-1$

	private static final String INPUT_DATA_URN = URN_OID_PREFIX + INPUT_DATA_OID;

	private static final String INPUT_DATA_URL = "http://java.com/es/download/"; //$NON-NLS-1$

	private static final String INPUT_DATA_ERROR = "Hola Mundo!!"; //$NON-NLS-1$

	private static final String POLICY_IDENTIFIER_PREFIX = "<xades:SigPolicyId><xades:Identifier>"; //$NON-NLS-1$

    private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

    private static final String DATA_FILE = "bin"; //$NON-NLS-1$

    private static final String FORMAT = AOSignConstants.SIGN_FORMAT_XADES_DETACHED;

    private static final Properties CONFIG = new Properties();

    static {
    	CONFIG.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_DETACHED); //$NON-NLS-1$
    	CONFIG.setProperty("policyIdentifierHash", "ESTEESUNHASH=");  //$NON-NLS-1$//$NON-NLS-2$
    	CONFIG.setProperty("policyIdentifierHashAlgorithm", DigestMethod.SHA1);         //$NON-NLS-1$
    	CONFIG.setProperty("policyDescription", "Politica de firma electronica para las Administraciones Publicas en Espana"); //$NON-NLS-1$ //$NON-NLS-2$
    	CONFIG.setProperty("policyQualifier", "http://blogs.adobe.com/security/91014620_eusig_wp_ue.pdf"); //$NON-NLS-1$ //$NON-NLS-2$
    }

    /**
     * Genera una firma XAdES estableciendo una pol&iacute;tica en donde el Identifier
     * es una OID.
     */
    @SuppressWarnings("static-method")
	@Test
	public void configuraPoliticaConIdentifierOID() {
		try {
			CONFIG.setProperty("policyIdentifier", INPUT_DATA_OID); //$NON-NLS-1$

			final SignApplet applet = new SignApplet();

			generateSign(applet, CONFIG);

			checkPolicyIdentifier(applet, URN_OID_PREFIX + INPUT_DATA_OID);
		}
		catch(final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}
	}

    /**
     * Genera una firma XAdES estableciendo una pol&iacute;tica en donde el Identifier
     * es una URN.
     */
    @SuppressWarnings("static-method")
	@Test
	public void configuraPoliticaConIdentifierURN() {
		try {
			CONFIG.setProperty("policyIdentifier", INPUT_DATA_URN); //$NON-NLS-1$

			final SignApplet applet = new SignApplet();

			generateSign(applet, CONFIG);

			checkPolicyIdentifier(applet, INPUT_DATA_URN);
		}
		catch(final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}
	}

    /**
     * Genera una firma XAdES estableciendo una pol&iacute;tica en donde el Identifier
     * es una URL.
     */
    @SuppressWarnings("static-method")
	@Test
	public void configuraPoliticaConIdentifierURL() {
		try {
			CONFIG.setProperty("policyIdentifier", INPUT_DATA_URL); //$NON-NLS-1$

			final SignApplet applet = new SignApplet();

			generateSign(applet, CONFIG);

			checkPolicyIdentifier(applet, INPUT_DATA_URL);
		}
		catch(final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}
	}

    /**
     * Genera una firma XAdES estableciendo una pol&iacute;tica en donde el Identifier
     * es err&oacute;neo.
     */
	@SuppressWarnings("static-method")
	@Test
	public void configuraPoliticaConIdentifierErroneo() {
		try {
			CONFIG.setProperty("policyIdentifier", INPUT_DATA_ERROR); //$NON-NLS-1$

			final SignApplet applet = new SignApplet();

			generateSign(applet, CONFIG);

			checkPolicyIdentifier(applet, INPUT_DATA_ERROR);
		}
		catch(final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}
	}

	private static String generateSign(final SignApplet applet, final Properties extraParams) {
    	final String ksPath = Ticket137182_175231Test.getResourcePath(CERT_PATH);
    	applet.setKeyStore(ksPath, CERT_PASS, "PKCS12"); //$NON-NLS-1$
    	applet.setSelectedCertificateAlias(CERT_ALIAS);

    	applet.setFileuri(Ticket137182_175231Test.getResourcePathToApplet(DATA_FILE));
		applet.setSignatureFormat(FORMAT);

		applet.setPolicy(
				extraParams.getProperty("policyIdentifier"), //$NON-NLS-1$
				extraParams.getProperty("policyDescription"), //$NON-NLS-1$
				extraParams.getProperty("policyQualifier"), //$NON-NLS-1$
				extraParams.getProperty("policyIdentifierHash")); //$NON-NLS-1$

		applet.sign();
		Assert.assertFalse("Error en la firma", applet.isError()); //$NON-NLS-1$

		return applet.getSignatureBase64Encoded();
	}

	private static void checkPolicyIdentifier(final SignApplet applet, final String identifier) {

		String result;
		try {
			result = new String(Base64.decode(applet.getSignatureBase64Encoded()));
		} catch (final IOException e) {
			Assert.fail("Error al decodificar el base 64 de la firma"); //$NON-NLS-1$
			return;
		}

		int i = result.indexOf(POLICY_IDENTIFIER_PREFIX);
		Assert.assertTrue("No se ha encontrado el identificador de la politica", i != -1); //$NON-NLS-1$

		i += POLICY_IDENTIFIER_PREFIX.length();

		final int j = result.indexOf('<', i);
		Assert.assertTrue("No se ha encontrado el cierre de la etiqueta identificador de la politica", j != -1); //$NON-NLS-1$

		Assert.assertEquals(identifier, result.substring(i, j));
	}

    private static String getResourcePath(final String filename) {
    	return GenerateAllSigns.class.getResource("/" + filename).toString().substring(6); //$NON-NLS-1$
    }

    private static String getResourcePathToApplet(final String filename) {
    	return GenerateAllSigns.class.getResource("/" + filename).toString(); //$NON-NLS-1$
    }
}
