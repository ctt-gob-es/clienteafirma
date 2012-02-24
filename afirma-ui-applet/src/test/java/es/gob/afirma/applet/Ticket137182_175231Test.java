package es.gob.afirma.applet;

import java.io.File;
import java.util.Properties;

import javax.xml.crypto.dsig.DigestMethod;

import junit.framework.Assert;

import org.junit.Ignore;
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
    @Test
	public void configuraPoliticaConIdentifierOID() {
		try {
			CONFIG.setProperty("policyIdentifier", INPUT_DATA_OID); //$NON-NLS-1$

			final String path = new File("").getAbsolutePath(); //$NON-NLS-1$

			final SignApplet applet = new SignApplet();

			generateSign(applet, CONFIG);

//			applet.setOutFilePath(path + "/137182_175231_OID_signature.xml"); //$NON-NLS-1$
//			applet.saveSignToFile();

			checkPolicyIdentifier(applet, URN_OID_PREFIX + INPUT_DATA_OID);

//			final String result = new String(Base64.decode(applet.getSignatureBase64Encoded()));
//			result.substring(result.indexOf("<xades:SigPolicyId><xades:Identifier>")).equals(URN_OID_PREFIX + INPUT_DATA_OID);
		}
		catch(final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}
	}

    /**
     * Genera una firma XAdES estableciendo una pol&iacute;tica en donde el Identifier
     * es una URN.
     */
    @Test
    @Ignore
	public void configuraPoliticaConIdentifierURN() {
		try {
			CONFIG.setProperty("policyIdentifier", INPUT_DATA_URN); //$NON-NLS-1$

			final String path = new File("").getAbsolutePath(); //$NON-NLS-1$

			final SignApplet applet = new SignApplet();

			generateSign(applet, CONFIG);

			applet.setOutFilePath(path + "/137182_175231_URN_signature.xml"); //$NON-NLS-1$
			applet.saveSignToFile();
		}
		catch(final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}
	}

    /**
     * Genera una firma XAdES estableciendo una pol&iacute;tica en donde el Identifier
     * es una URL.
     */
    @Test
    @Ignore
	public void configuraPoliticaConIdentifierURL() {
		try {
			CONFIG.setProperty("policyIdentifier", "http://java.com/es/download/"); //$NON-NLS-1$ //$NON-NLS-2$

			final String path = new File("").getAbsolutePath(); //$NON-NLS-1$

			final SignApplet applet = new SignApplet();

			generateSign(applet, CONFIG);

			applet.setOutFilePath(path + "/137182_175231_URL_signature.xml"); //$NON-NLS-1$
			applet.saveSignToFile();
		}
		catch(final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}
	}

    /**
     * Genera una firma XAdES estableciendo una pol&iacute;tica en donde el Identifier
     * es err&oacute;neo.
     */
	@Test
	@Ignore
	public void configuraPoliticaConIdentifierErroneo() {
		try {
			CONFIG.setProperty("policyIdentifier", "Hola Mundo!!"); //$NON-NLS-1$ //$NON-NLS-2$

			final String path = new File("").getAbsolutePath(); //$NON-NLS-1$

			final SignApplet applet = new SignApplet();

			generateSign(applet, CONFIG);

			applet.setOutFilePath(path + "/137182_175231_Erroneo_signature.xml"); //$NON-NLS-1$
			applet.saveSignToFile();
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

	private void checkPolicyIdentifier(final SignApplet applet, final String identifier) {

		final String result = new String(Base64.decode(applet.getSignatureBase64Encoded()));

		System.out.println();

		final int i = result.indexOf("<xades:SigPolicyId><xades:Identifier>"); //$NON-NLS-1$
		Assert.assertTrue("No se ha encontrado el identificador de la politica", i != -1); //$NON-NLS-1$

		final int j = result.indexOf('<', i);
		Assert.assertTrue("No se ha encontrado el cierre de la etiqueta identificador de la politica", j != -1); //$NON-NLS-1$

		Assert.assertEquals(result.substring(i, j), identifier + "2");
	}

    private static String getResourcePath(final String filename) {
    	return GenerateAllSigns.class.getResource("/" + filename).toString().substring(6); //$NON-NLS-1$
    }

    private static String getResourcePathToApplet(final String filename) {
    	return GenerateAllSigns.class.getResource("/" + filename).toString(); //$NON-NLS-1$
    }
}
