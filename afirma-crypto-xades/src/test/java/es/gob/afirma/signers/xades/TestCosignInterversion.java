package es.gob.afirma.signers.xades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Before;
import org.junit.Test;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;

public class TestCosignInterversion {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final String CERT_PATH_TO_SIGN = "00_empleado_publico-hsm.p12"; //$NON-NLS-1$
    private static final String CERT_PASS_TO_SIGN = "12345"; //$NON-NLS-1$
    private static final String CERT_ALIAS_TO_SIGN = "nombre apellido1 apellido2 - dni 12345678z"; //$NON-NLS-1$

	private static final String XADES_122_FACTURAE_FILE = "xades_122_facturae.xml"; //$NON-NLS-1$
	private static final String XADES_122_IGAE_FILE = "xades_122_igae.xml"; //$NON-NLS-1$
	private static final String XADES_132_AFIRMA_FILE = "xades_132_afirma.xml"; //$NON-NLS-1$

	private byte[] xades_122_facturae;
	private byte[] xades_122_igae;
	private byte[] xades_132_afirma;

	private Properties baselineParams;

	private PrivateKeyEntry pke = null;

	private AOXAdESSigner signer = null;

	/** Antes de ejecutar cualquier prueba se ejecutar&aacute; este m&eacute;todo que cargar6aacute;
	 * todos los objetos que se vaya a necesitar en las distintas pruebas.
	 * @throws Exception En cualquier error. */
	@Before
	public void loadParams() throws Exception {

		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

		this.baselineParams = new Properties();
		this.baselineParams.setProperty(XAdESExtraParams.PROFILE, AOSignConstants.SIGN_PROFILE_BASELINE);

		final KeyStore ksCosign = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ksCosign.load(ClassLoader.getSystemResourceAsStream(CERT_PATH_TO_SIGN), CERT_PASS_TO_SIGN.toCharArray());
		this.pke = (PrivateKeyEntry) ksCosign.getEntry(CERT_ALIAS_TO_SIGN, new KeyStore.PasswordProtection(CERT_PASS_TO_SIGN.toCharArray()));

		try {
			this.xades_122_facturae = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(XADES_122_FACTURAE_FILE));
			this.xades_122_igae = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(XADES_122_IGAE_FILE));
			this.xades_132_afirma = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(XADES_132_AFIRMA_FILE));
		}
		catch (final IOException e) {
			LOGGER.log(Level.SEVERE, "No se han podido cargar los ficheros de prueba", e);  //$NON-NLS-1$
		}

		this.signer = new AOXAdESSigner();
	}

	/** Prueba de cofirma de una factura electronica mediante una firma XAdES 1.2.2
	 * (sin respetar los requisitos de factura electronica).
     * @throws Exception Cuando ocurre un error */
    @Test
    public void testCoSignXAdES122Facturae() throws Exception {

		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;

		final byte[] result = this.signer.cosign(
				this.xades_122_facturae, algorithm, this.pke.getPrivateKey(), this.pke.getCertificateChain(),
				null);

		final File saveFile = saveTempFile(result);
		System.out.println("Prueba " + new TestCosignInterversion() { /* Vacio */ }.getClass().getEnclosingMethod().getName() + ": " + saveFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$

		checkCosign(result, algorithm, true);
    }

    /** Prueba de cofirma baseline de una factura electronica mediante una firma XAdES 1.2.2
	 * (sin respetar los requisitos de factura electronica). El par&aacute;metro baseline se
	 * deber&iacute;a ignorar.
     * @throws Exception Cuando ocurre un error */
    @Test
    public void testCoSignBaselineXAdES122Facturae() throws Exception {

		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;

		final byte[] result = this.signer.cosign(
				this.xades_122_facturae, algorithm, this.pke.getPrivateKey(), this.pke.getCertificateChain(),
				this.baselineParams);

		final File saveFile = saveTempFile(result);
		System.out.println("Prueba " + new TestCosignInterversion() { /* Vacio */ }.getClass().getEnclosingMethod().getName() + ": " + saveFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$

		checkCosign(result, algorithm, true);
    }

	/** Prueba de cofirma de una firma XAdES 1.2.2 generada por la IGAE.
     * @throws Exception Cuando ocurre un error */
    @Test
    public void testCoSignXAdES122Igae() throws Exception {

		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;

		final byte[] result = this.signer.cosign(
				this.xades_122_igae, algorithm, this.pke.getPrivateKey(), this.pke.getCertificateChain(),
				null);

		final File saveFile = saveTempFile(result);
		System.out.println("Prueba " + new TestCosignInterversion() { /* Vacio */ }.getClass().getEnclosingMethod().getName() + ": " + saveFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$

		checkCosign(result, algorithm, true);
    }

    /** Prueba de cofirma baseline de una firma XAdES 1.2.2 generada por la IGAE.
     * El par&aacute;metro baseline se deber&iacute;a ignorar.
     * @throws Exception Cuando ocurre un error */
    @Test
    public void testCoSignBaselineXAdES122Igae() throws Exception {

		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;

		final byte[] result = this.signer.cosign(
				this.xades_122_igae, algorithm, this.pke.getPrivateKey(), this.pke.getCertificateChain(),
				this.baselineParams);

		final File saveFile = saveTempFile(result);
		System.out.println("Prueba " + new TestCosignInterversion() { /* Vacio */ }.getClass().getEnclosingMethod().getName() + ": " + saveFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$

		checkCosign(result, algorithm, true);
    }

	/** Prueba de cofirma de una firma XAdES 1.3.2 generada por el Cliente @firma.
     * @throws Exception Cuando ocurre un error */
    @Test
    public void testCoSignXAdES132Afirma() throws Exception {

		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;

		final byte[] result = this.signer.cosign(
				this.xades_132_afirma, algorithm, this.pke.getPrivateKey(), this.pke.getCertificateChain(),
				null);

		final File saveFile = saveTempFile(result);
		System.out.println("Prueba " + new TestCosignInterversion() { /* Vacio */ }.getClass().getEnclosingMethod().getName() + ": " + saveFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$

		checkCosign(result, algorithm, true);
    }

    /** Prueba de cofirma baseline de una firma XAdES 1.3.2 generada por el Cliente @firma.
     * @throws Exception Cuando ocurre un error */
    @Test
    public void testCoSignBaselineXAdES132Afirma() throws Exception {

		final String algorithm = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;

		final byte[] result = this.signer.cosign(
				this.xades_132_afirma, algorithm, this.pke.getPrivateKey(), this.pke.getCertificateChain(),
				this.baselineParams);

		final File saveFile = saveTempFile(result);
		System.out.println("Prueba " + new TestCosignInterversion() { /* Vacio */ }.getClass().getEnclosingMethod().getName() + ": " + saveFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$

		checkCosign(result, algorithm, true);
    }

    /**
	 * Guarda datos temporales en disco.
	 * @param data Datos a guardar.
	 * @return Fichero temporal generado.
	 * @throws IOException Cuando ocurre un error en el guardado del fichero en disco.
	 */
	private static File saveTempFile(final byte[] data) throws IOException {
		final File saveFile = File.createTempFile("testCoSign-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try ( final OutputStream os = new FileOutputStream(saveFile) ) {
			os.write(data);
			os.flush();
		}
		return saveFile;
	}

	/**
	 * Realiza las comprobaciones necesarias sobre la firma generada.
	 * @param signature Firma generada.
	 * @param algorithm Algoritmo de firma empleado en la nueva firma.
	 * @param hasData Indica si la firma debe contener o no los datos originalmente firmados.
	 * @throws IOException Cuando ocurre un error al leer la firma.
	 * @throws AOException Cuando ocurre un error al extraer los datos originalmente firmados.
	 */
	private void checkCosign(final byte[] signature, final String algorithm, final boolean hasData) throws IOException, AOException {

//		Assert.assertNotNull("Se ha obtenido una cofirma nula", signature); //$NON-NLS-1$
//		Assert.assertTrue("La cofirma no se reconoce como firma CAdES", this.signer.isSign(signature)); //$NON-NLS-1$
//
//		final byte[] contentData = this.signer.getData(signature);
//		if (hasData) {
//			Assert.assertNotNull("No se han encontrado los datos en la cofirma", contentData); //$NON-NLS-1$
//			Assert.assertNotEquals("El campo de datos de la firma esta vacio", contentData.length, 0); //$NON-NLS-1$
//			Assert.assertTrue("Los datos extraidos de la firma no son los que originalmente se firmaron", Arrays.equals(contentData, this.data)); //$NON-NLS-1$
//		}
//		else {
//			Assert.assertNull("El campo de datos de la firma deberia estar vacio", contentData); //$NON-NLS-1$
//		}
//
//		final AOTreeModel tree = this.signer.getSignersStructure(signature, true);
//
//		final AOTreeNode rootNode = (AOTreeNode) tree.getRoot();
//		Assert.assertEquals("No se interpreto correctamente la estructura de la cofirma", "Datos", rootNode.getUserObject()); //$NON-NLS-1$ //$NON-NLS-2$
//
//		AOSimpleSignInfo cosignInfo = null;
//
//		for (int i = 0; i < rootNode.getChildCount(); i++) {
//			final AOSimpleSignInfo signInfo = (AOSimpleSignInfo) rootNode.getChildAt(i).getUserObject();
//			if (this.pke.getCertificate().equals(signInfo.getCerts()[0])) {
//				cosignInfo = signInfo;
//				break;
//			}
//		}
//
//		Assert.assertNotNull("No se encontro la cofirma generada en la nueva estructura de firma", cosignInfo); //$NON-NLS-1$
//		Assert.assertEquals("El algoritmo de firma no es el esperado", algorithm, cosignInfo.getSignAlgorithm()); //$NON-NLS-1$
//		Assert.assertNotNull("No se encontro la hora de firma", cosignInfo.getSigningTime()); //$NON-NLS-1$
	}
}
