package es.gob.afirma.signers.cadestri.client;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.cadestri.client.asic.AOCAdESASiCSTriPhaseSigner;

/** Pruebas de firma CAdES trif&aacute;sica.
 * @author Tom&acute;s Garc&iacute;a-Mer&aacute;s */
public final class TestCadesTriphase {

	/** Nombre de la propiedad de URL del servidor de firma trif&aacute;sica. */
	private static final String PROPERTY_NAME_SIGN_SERVER_URL = "serverUrl"; //$NON-NLS-1$
	private static final String PROPERTY_VALUE_SIGN_SERVER_URL = "http://localhost:8080/afirma-server-triphase-signer/SignatureService"; //$NON-NLS-1$

	// ID del documento, en este caso el documento en si
	private static final String PROPERTY_NAME_DOC_ID = "documentId"; //$NON-NLS-1$
	//private static final String PROPERTY_VALUE_DOC_ID = "SG9sYSBNdW5kbw=="; //$NON-NLS-1$
	private static final String PROPERTY_VALUE_DOC_ID = "Entrada.pdf"; //$NON-NLS-1$


	// Almacen de pruebas
	private static final String CERT_PATH = "PFActivoFirSHA1.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
	private static final String CERT_ALIAS = "fisico activo prueba"; //$NON-NLS-1$

	private static final String CERT_PATH_2 = "PJActivoFirSHA1.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS_2 = "12341234"; //$NON-NLS-1$
	private static final String CERT_ALIAS_2 = "juridico activo prueba-b12345678"; //$NON-NLS-1$

	private Properties serverConfig;
	private PrivateKeyEntry pke;
	private PrivateKeyEntry pke2;

	/**
	 * Prueba de firma CAdES trif&aacute;sica.
	 * @throws AOException Cuando falla la firma.
	 * @throws IOException Cuando ocurre un error al cargar o guardar datos.
	 */
	@Test
	@Ignore // Necesita el servidor
	public void testTriPhaseSignCAdESASiCS() throws AOException, IOException {
		final AOSigner signer = new AOCAdESASiCSTriPhaseSigner();

		final Properties config = new Properties(this.serverConfig);
		for (final String key : this.serverConfig.keySet().toArray(new String[this.serverConfig.size()])) {
			config.setProperty(key, this.serverConfig.getProperty(key));
		}

		final byte[] result = signer.sign("Hola Mundo".getBytes(), "SHA512withRSA", this.pke.getPrivateKey(), this.pke.getCertificateChain(), config); //$NON-NLS-1$ //$NON-NLS-2$

		Assert.assertNotNull("Error durante el proceso de firma, resultado nulo", result); //$NON-NLS-1$

		try (
			final OutputStream os = new FileOutputStream(File.createTempFile("firma_tri_", ".csig")); //$NON-NLS-1$ //$NON-NLS-2$
		) {
			os.write(result);
		}

		System.out.println("OK"); //$NON-NLS-1$

		final File f = File.createTempFile("CAdES-ASiC-S_", ".zip"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(f);
		) {
			fos.write(result);
		}
		java.util.logging.Logger.getLogger("es.gob.afirma").info("El resultado de la firma se almaceno en: " + f.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$

	}

	/** Prueba de firma CAdES trif&aacute;sica.
	 * @throws Exception en cualquier error. */
	@Test
	@Ignore // Necesita el servidor
	public void testTriPhaseSignCAdES() throws Exception {
		final AOSigner signer = new AOCAdESTriPhaseSigner();

		final Properties config = new Properties(this.serverConfig);
		for (final String key : this.serverConfig.keySet().toArray(new String[this.serverConfig.size()])) {
			config.setProperty(key, this.serverConfig.getProperty(key));
		}

		final byte[] result = signer.sign("Hola Mundo".getBytes(), "SHA512withRSA", this.pke.getPrivateKey(), this.pke.getCertificateChain(), config); //$NON-NLS-1$ //$NON-NLS-2$

		Assert.assertNotNull("Error durante el proceso de firma, resultado nulo", result); //$NON-NLS-1$

		try (
			final OutputStream os = new FileOutputStream(File.createTempFile("firma_tri_", ".csig")); //$NON-NLS-1$ //$NON-NLS-2$
		) {
			os.write(result);
		}

//		System.out.println("OK");
//		Logger.getLogger("es.gob.afirma").info("El resultado de la firma es: " + new String(result)); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Prueba de cofirma CAdES trif&aacute;sica.
	 * @throws Exception en cualquier error. */
	@Test
	@Ignore // Necesita el servidor
	public void cofirma() throws Exception {
		final AOSigner signer = new AOCAdESTriPhaseSigner();

		final Properties config = new Properties(this.serverConfig);
		for (final String key : this.serverConfig.keySet().toArray(new String[this.serverConfig.size()])) {
			config.setProperty(key, this.serverConfig.getProperty(key));
		}

		final byte[] signature;
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream("firma_tri.csig"); //$NON-NLS-1$
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final byte[] result = signer.cosign(
				signature,
				"SHA512withRSA", //$NON-NLS-1$
				this.pke2.getPrivateKey(),
				this.pke2.getCertificateChain(),
				config);

		Assert.assertNotNull("Error durante el proceso de firma, resultado nulo", result); //$NON-NLS-1$

		try (
			final OutputStream os = new FileOutputStream(File.createTempFile("cofirma_tri_", ".csig")); //$NON-NLS-1$ //$NON-NLS-2$
		) {
			os.write(result);
		}

//		System.out.println("OK");
//		Logger.getLogger("es.gob.afirma").info("El resultado de la firma es: " + new String(result)); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Prueba de contrafirma CAdES trif&aacute;sica.
	 * @throws Exception en cualquier error. */
	@Test
	@Ignore // Necesita el servidor
	public void contrafirma() throws Exception {
		final AOSigner signer = new AOCAdESTriPhaseSigner();

		final Properties config = new Properties(this.serverConfig);
		for (final String key : this.serverConfig.keySet().toArray(new String[this.serverConfig.size()])) {
			config.setProperty(key, this.serverConfig.getProperty(key));
		}

		final byte[] signature;
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream("cofirma_tri.csig"); //$NON-NLS-1$
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final byte[] result = signer.countersign(
			signature,
			"SHA512withRSA", //$NON-NLS-1$
			CounterSignTarget.TREE,
			null,
			this.pke2.getPrivateKey(),
			this.pke2.getCertificateChain(),
			config
		);

		Assert.assertNotNull("Error durante el proceso de firma, resultado nulo", result); //$NON-NLS-1$

		try (
			final OutputStream os = new FileOutputStream(File.createTempFile("contrafirma_tri_", ".csig")); //$NON-NLS-1$ //$NON-NLS-2$
		) {
			os.write(result);
		}

//		System.out.println("OK");
//		Logger.getLogger("es.gob.afirma").info("El resultado de la firma es: " + new String(result)); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Carga el almac&acute;n de pruebas.
	 * @throws Exception en cualquier error. */
	@Before
	public void loadKeystore() throws Exception {

		// Cargamos la referencia a la clave privada 1
		KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		this.pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		// Cargamos la referencia a la clave privada 2
		ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH_2), CERT_PASS_2.toCharArray());
		this.pke2 = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS_2, new KeyStore.PasswordProtection(CERT_PASS_2.toCharArray()));

		// Establecemos la configuracion
		this.serverConfig = new Properties();
		this.serverConfig.setProperty(PROPERTY_NAME_SIGN_SERVER_URL, PROPERTY_VALUE_SIGN_SERVER_URL);
		this.serverConfig.setProperty(PROPERTY_NAME_DOC_ID, PROPERTY_VALUE_DOC_ID);
	}

}
