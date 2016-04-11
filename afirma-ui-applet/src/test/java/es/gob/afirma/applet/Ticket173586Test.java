package es.gob.afirma.applet;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;

/**
 * Prueba el problema descrito en la incidencia 173586 (Ticket Mantis #0000029)
 */
public class Ticket173586Test {

    private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

    private static final String DATA_FILE = "bin7"; //$NON-NLS-1$

	private static final String FORMAT = AOSignConstants.SIGN_FORMAT_XADES_DETACHED;
	private static final String MODE = AOSignConstants.SIGN_MODE_IMPLICIT;

	/**
	 * Prueba que el sistema de firma masiva programatica no se queda colgado con ficheros
	 * mayores de 7 megas.
	 * @throws Exception Cuando se produce alg&uacute;n error durante la prueba.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void pruebaComprobacionPasoBase64() throws Exception {

		final byte[] data = getResource(DATA_FILE);

		Assert.assertFalse(Base64.isBase64(data));
		Assert.assertTrue(Base64.isBase64(Base64.encode(data).getBytes()));
	}

	/**
	 * Prueba que el sistema de firma masiva programatica no se queda colgado con ficheros
	 * mayores de 7 megas.
	 * @throws Exception Cuando se produce alg&uacute;n error durante la prueba.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void pruebaFirmaMasivaDeFicheroMayorDe7Megas() throws Exception {
    	final String ksPath = getResourcePath(CERT_PATH);


    	final SignApplet applet = new SignApplet();
    	applet.setKeyStore(ksPath, CERT_PASS, "PKCS12"); //$NON-NLS-1$
    	applet.setSelectedCertificateAlias(CERT_ALIAS);

    	applet.setSignatureFormat(FORMAT);
		applet.setSignatureMode(MODE);

		applet.initMassiveSignature();

		applet.setMassiveOperation(AOSignConstants.MASSIVE_OPERATION_SIGN);
		final String signatureB64 = applet.massiveSignatureFile(getResourcePathToApplet(DATA_FILE));
		Assert.assertFalse("Error en la firma masiva: ", applet.isError()); //$NON-NLS-1$

		System.out.println(applet.getMassiveSignatureCurrentLog());

		applet.endMassiveSignature();

		final File outFile = File.createTempFile("AfirmaTest173586", null); //$NON-NLS-1$
		final FileOutputStream fos = new FileOutputStream(outFile);
		fos.write(Base64.decode(signatureB64));
		fos.flush();
		fos.close();

		System.out.println("Fichero de salida: " + outFile.getAbsolutePath()); //$NON-NLS-1$
	}

    private static String getResourcePath(final String filename) {
    	return GenerateAllSigns.class.getResource("/" + filename).toString().substring(6); //$NON-NLS-1$
    }

    private static byte[] getResource(final String filename) throws IOException {
    	return AOUtil.getDataFromInputStream(GenerateAllSigns.class.getResourceAsStream("/" + filename)); //$NON-NLS-1$
    }

    private static String getResourcePathToApplet(final String filename) {
    	return GenerateAllSigns.class.getResource("/" + filename).toString(); //$NON-NLS-1$
    }
}
