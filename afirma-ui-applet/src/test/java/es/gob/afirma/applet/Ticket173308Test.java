package es.gob.afirma.applet;

import java.io.File;
import java.io.FileOutputStream;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.afirma.core.signers.AOSignConstants;

public class Ticket173308Test {

    private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

    private static final String DATA_FILE = "bin"; //$NON-NLS-1$

	private static final String FORMAT = AOSignConstants.SIGN_FORMAT_XADES_DETACHED;
	private static final String MODE = AOSignConstants.SIGN_MODE_EXPLICIT;

	/**
	 * Genera la firma de un documento y su guardado mediante el m&eacute;todo
	 * saveSignToFile().
	 */
	@SuppressWarnings("static-method")
	@Test
	public void saveSignToFileTest() {

		final String path = new File("").getAbsolutePath(); //$NON-NLS-1$

		final SignApplet applet = new SignApplet();

		getSignB64(applet);

		applet.setOutFilePath(path + "/173308_SaveSignToFile.xsig"); //$NON-NLS-1$
		applet.saveSignToFile();
	}

	/**
	 * Genera la firma de un documento y su guardado al recuperarla con el
	 * m&eacute;todo getSignatureBase64Encoded().
	 */
	@SuppressWarnings("static-method")
	@Test
	public void getSignatureBase64EncodedTest() {

		final String path = new File("").getAbsolutePath(); //$NON-NLS-1$

		final SignApplet applet = new SignApplet();

		getSignB64(applet);

		applet.setData(applet.getSignatureBase64Encoded());
		applet.saveDataToFile(path + "/173308_GetSignatureBase64Encoded.xsig"); //$NON-NLS-1$
	}

	/**
	 * Genera la firma de un documento y su guardado al recuperarla con el
	 * m&eacute;todo getTextFromBase64().
	 */
	@SuppressWarnings("static-method")
	@Test
	public void getTextFromBase64Test() {

		final String path = new File("").getAbsolutePath(); //$NON-NLS-1$

		final SignApplet applet = new SignApplet();

		getSignB64(applet);

		try {
			System.out.println(path + "/173308_GetTextFromBase64.xsig"); //$NON-NLS-1$
			final FileOutputStream fos = new FileOutputStream(new File(path, "173308_GetTextFromBase64.xsig")); //$NON-NLS-1$
			fos.write(applet.getTextFromBase64(applet.getSignatureBase64Encoded()).getBytes());
			try {
				fos.close();
			} catch (final Exception e) {
				// Se ignora el error
			}
		} catch (final Exception e) {
			Assert.fail("No se pudo generar la firma de prueba"); //$NON-NLS-1$
		}
	}

	/**
	 * Genera la firma de un documento y su guardado al recuperarla con el
	 * m&eacute;todo getBase64FromText().
	 */
	@SuppressWarnings("static-method")
	@Test
	public void getSignatureTextTest() {

		final String path = new File("").getAbsolutePath(); //$NON-NLS-1$

		final SignApplet applet = new SignApplet();

		getSignB64(applet);

		try {
			final FileOutputStream fos = new FileOutputStream(path + "/173308_GetSignatureText.xsig"); //$NON-NLS-1$
			fos.write(applet.getSignatureText().getBytes());
			try {
				fos.close();
			} catch (final Exception e) {
				// Se ignora el error
			}
		} catch (final Exception e) {
			Assert.fail("No se pudo generar la firma de prueba"); //$NON-NLS-1$
		}
	}

	/**
	 * Genera la firma de un documento y su guardado al recuperarla con el
	 * m&eacute;todo getSignatureText() y transformar a Base64 con getBase64FromText().
	 */
	@SuppressWarnings("static-method")
	@Test
	public void getBase64FromTextTest() {

		final String path = new File("").getAbsolutePath(); //$NON-NLS-1$

		final SignApplet applet = new SignApplet();

		getSignB64(applet);

		applet.setData(applet.getBase64FromText(applet.getSignatureText()));
		applet.saveDataToFile(path + "/173308_GetBase64FromText.xsig"); //$NON-NLS-1$
	}


	private static String getSignB64(final SignApplet applet) {
    	final String ksPath = Ticket173308Test.getResourcePath(CERT_PATH);
    	applet.setKeyStore(ksPath, CERT_PASS, "PKCS12"); //$NON-NLS-1$
    	applet.setSelectedCertificateAlias(CERT_ALIAS);

    	applet.setFileuri(Ticket173308Test.getResourcePathToApplet(DATA_FILE));
		applet.setSignatureFormat(FORMAT);
		applet.setSignatureMode(MODE);

		applet.sign();
		Assert.assertFalse("Error en la firma", applet.isError()); //$NON-NLS-1$

		return applet.getSignatureBase64Encoded();
	}

    private static String getResourcePath(final String filename) {
    	return GenerateAllSigns.class.getResource("/" + filename).toString().substring(6); //$NON-NLS-1$
    }

    private static String getResourcePathToApplet(final String filename) {
    	return GenerateAllSigns.class.getResource("/" + filename).toString(); //$NON-NLS-1$
    }
}
