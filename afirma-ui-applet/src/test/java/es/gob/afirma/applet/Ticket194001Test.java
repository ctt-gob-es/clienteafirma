package es.gob.afirma.applet;

import java.io.File;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.afirma.core.signers.AOSignConstants;

/**
 * Genera firmas para la comprobaci&oacute;n manual de la resoluci&oacute;n del Ticket 194001.
 */
public class Ticket194001Test {

	private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

    private static final String DATA_FILE = "bin"; //$NON-NLS-1$

    /** Genera una firma acorde al est&aacute;ndar XAdES 1.2.2. */
    @SuppressWarnings("static-method")
	@Test
	public void signTest() {

    	try {

	    	final String path = new File("").getAbsolutePath(); //$NON-NLS-1$

	    	final SignApplet applet = new SignApplet();
	    	applet.initialize();
	    	final String ksPath = Ticket194001Test.getResourcePath(CERT_PATH);
	    	applet.setKeyStore(ksPath, CERT_PASS, "PKCS12"); //$NON-NLS-1$
	    	applet.setSelectedCertificateAlias(CERT_ALIAS);

	    	applet.setSignatureFormat(AOSignConstants.SIGN_FORMAT_XADES);
	    	applet.setSignatureMode(AOSignConstants.SIGN_MODE_IMPLICIT);
	    	applet.addExtraParam("xadesNamespace", "http://uri.etsi.org/01903/v1.2.2#"); //$NON-NLS-1$ //$NON-NLS-2$
	    	applet.addExtraParam("signedPropertiesTypeUrl", "http://uri.etsi.org/01903/v1.2.2#SignedProperties"); //$NON-NLS-1$ //$NON-NLS-2$


	    	applet.setFileuri(Ticket194001Test.getResourcePathToApplet(DATA_FILE));

	    	applet.sign();
	    	Assert.assertFalse(applet.getErrorMessage(), applet.isError());
	    	applet.setOutFilePath(path + "/" + "194001_sign.xsig"); //$NON-NLS-1$ //$NON-NLS-2$
	    	applet.saveSignToFile();
    	}
		catch(final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}
	}

    /** Genera una cofirma acorde al est&aacute;ndar XAdES 1.2.2. */
    @SuppressWarnings("static-method")
	@Test
	public void cosignTest() {

    	try {

	    	final String path = new File("").getAbsolutePath(); //$NON-NLS-1$

	    	final SignApplet applet = new SignApplet();
	    	applet.initialize();
	    	final String ksPath = Ticket194001Test.getResourcePath(CERT_PATH);
	    	applet.setKeyStore(ksPath, CERT_PASS, "PKCS12"); //$NON-NLS-1$
	    	applet.setSelectedCertificateAlias(CERT_ALIAS);

	    	applet.setSignatureFormat(AOSignConstants.SIGN_FORMAT_XADES);
	    	applet.setSignatureMode(AOSignConstants.SIGN_MODE_IMPLICIT);
	    	applet.addExtraParam("xadesNamespace", "http://uri.etsi.org/01903/v1.2.2#"); //$NON-NLS-1$ //$NON-NLS-2$
	    	applet.addExtraParam("signedPropertiesTypeUrl", "http://uri.etsi.org/01903/v1.2.2#SignedProperties"); //$NON-NLS-1$ //$NON-NLS-2$


	    	applet.setFileuri(Ticket194001Test.getResourcePathToApplet(DATA_FILE));

	    	applet.sign();
	    	Assert.assertFalse(applet.getErrorMessage(), applet.isError());

	    	String signatureB64 = applet.getSignatureBase64Encoded();
	    	Assert.assertNotNull(signatureB64);

	    	applet.initialize();

	    	applet.setKeyStore(ksPath, CERT_PASS, "PKCS12"); //$NON-NLS-1$
	    	applet.setSelectedCertificateAlias(CERT_ALIAS);

	    	applet.setSignatureFormat(AOSignConstants.SIGN_FORMAT_XADES);
	    	applet.setSignatureMode(AOSignConstants.SIGN_MODE_IMPLICIT);
	    	applet.addExtraParam("xadesNamespace", "http://uri.etsi.org/01903/v1.2.2#"); //$NON-NLS-1$ //$NON-NLS-2$
	    	applet.addExtraParam("signedPropertiesTypeUrl", "http://uri.etsi.org/01903/v1.2.2#SignedProperties"); //$NON-NLS-1$ //$NON-NLS-2$

	    	applet.setFileuri(Ticket194001Test.getResourcePathToApplet(DATA_FILE));
	    	applet.setElectronicSignature(signatureB64);

	    	applet.coSign();
	    	Assert.assertFalse(applet.getErrorMessage(), applet.isError());

	    	signatureB64 = applet.getSignatureBase64Encoded();
	    	Assert.assertNotNull(signatureB64);

	    	applet.setOutFilePath(path + "/" + "194001_cosign.xsig"); //$NON-NLS-1$ //$NON-NLS-2$
	    	applet.saveSignToFile();
    	}
		catch(final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}
	}

    /** Genera una cofirma acorde al est&aacute;ndar XAdES 1.2.2. */
    @SuppressWarnings("static-method")
	@Test
	public void countersignTest() {

    	try {

	    	final String path = new File("").getAbsolutePath(); //$NON-NLS-1$

	    	final SignApplet applet = new SignApplet();
	    	applet.initialize();
	    	final String ksPath = Ticket194001Test.getResourcePath(CERT_PATH);
	    	applet.setKeyStore(ksPath, CERT_PASS, "PKCS12"); //$NON-NLS-1$
	    	applet.setSelectedCertificateAlias(CERT_ALIAS);

	    	applet.setSignatureFormat(AOSignConstants.SIGN_FORMAT_XADES);
	    	applet.setSignatureMode(AOSignConstants.SIGN_MODE_IMPLICIT);
	    	applet.addExtraParam("xadesNamespace", "http://uri.etsi.org/01903/v1.2.2#"); //$NON-NLS-1$ //$NON-NLS-2$
	    	applet.addExtraParam("signedPropertiesTypeUrl", "http://uri.etsi.org/01903/v1.2.2#SignedProperties"); //$NON-NLS-1$ //$NON-NLS-2$

	    	applet.setFileuri(Ticket194001Test.getResourcePathToApplet(DATA_FILE));

	    	applet.sign();
	    	Assert.assertFalse(applet.getErrorMessage(), applet.isError());

	    	String signatureB64 = applet.getSignatureBase64Encoded();
	    	Assert.assertNotNull(signatureB64);

	    	applet.initialize();

	    	applet.setKeyStore(ksPath, CERT_PASS, "PKCS12"); //$NON-NLS-1$
	    	applet.setSelectedCertificateAlias(CERT_ALIAS);

	    	applet.setSignatureFormat(AOSignConstants.SIGN_FORMAT_XADES);
	    	applet.setSignatureMode(AOSignConstants.SIGN_MODE_IMPLICIT);
	    	applet.addExtraParam("xadesNamespace", "http://uri.etsi.org/01903/v1.2.2#"); //$NON-NLS-1$ //$NON-NLS-2$
	    	applet.addExtraParam("signedPropertiesTypeUrl", "http://uri.etsi.org/01903/v1.2.2#SignedProperties"); //$NON-NLS-1$ //$NON-NLS-2$

	    	applet.setFileuri(Ticket194001Test.getResourcePathToApplet(DATA_FILE));
	    	applet.setElectronicSignature(signatureB64);

	    	applet.coSign();
	    	Assert.assertFalse(applet.getErrorMessage(), applet.isError());

	    	signatureB64 = applet.getSignatureBase64Encoded();
	    	Assert.assertNotNull(signatureB64);

	    	applet.initialize();

	    	applet.setKeyStore(ksPath, CERT_PASS, "PKCS12"); //$NON-NLS-1$
	    	applet.setSelectedCertificateAlias(CERT_ALIAS);

	    	applet.setSignatureFormat(AOSignConstants.SIGN_FORMAT_XADES);
	    	applet.setSignatureMode(AOSignConstants.SIGN_MODE_IMPLICIT);
	    	applet.addExtraParam("xadesNamespace", "http://uri.etsi.org/01903/v1.2.2#"); //$NON-NLS-1$ //$NON-NLS-2$
	    	applet.addExtraParam("signedPropertiesTypeUrl", "http://uri.etsi.org/01903/v1.2.2#SignedProperties"); //$NON-NLS-1$ //$NON-NLS-2$

	    	applet.setElectronicSignature(signatureB64);

	    	applet.counterSignLeafs();
	    	Assert.assertFalse(applet.getErrorMessage(), applet.isError());

	    	signatureB64 = applet.getSignatureBase64Encoded();
	    	Assert.assertNotNull(signatureB64);

	    	applet.setOutFilePath(path + "/" + "194001_countersign.xsig"); //$NON-NLS-1$ //$NON-NLS-2$
	    	applet.saveSignToFile();
    	}
		catch(final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}
	}

    private static String getResourcePath(final String filename) {
    	return GenerateAllSigns.class.getResource("/" + filename).toString().substring(6); //$NON-NLS-1$
    }

    private static String getResourcePathToApplet(final String filename) {
    	return GenerateAllSigns.class.getResource("/" + filename).toString(); //$NON-NLS-1$
    }
}
