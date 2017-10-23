
package es.gob.afirma.signers.multi.cades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.util.Properties;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.cades.AOCAdESSigner;

/** Pruebas de cofirmas CAdES.
 * @author Carlos Gamuci. */
public final class TestCosign {

	private static final String PKCS12_KEYSTORE = "ANCERTCCP_FIRMA.p12"; //$NON-NLS-1$

	private static final String PASSWORD = "1111"; //$NON-NLS-1$

	private static final String IMPLICIT_SHA1_SIGN_FILE = "firma_implicita.csig"; //$NON-NLS-1$
	private static final String EXPLICIT_SHA1_SIGN_FILE = "firma_explicita.csig"; //$NON-NLS-1$
	private static final String IMPLICIT_SHA1_CADES_A_FILE = "cadesA.csig"; //$NON-NLS-1$
	private static final String IMPLICIT_SHA1_CADES_T_FILE = "CADES-T.csig"; //$NON-NLS-1$

	private static final String DATA_FILE = "data"; //$NON-NLS-1$
	private static final String DATA_FILE_PDF = "Original.pdf"; //$NON-NLS-1$

	private static InputStream ksIs;
	private static KeyStore ks;

	private static byte[] data;

	/** Carga el almac&eacute;n de certificados.
	 * @throws Exception Cuando ocurre algun problema al cargar el almac&eacute;n o los datos. */
	@Before
	public void cargaAlmacen() throws Exception {
		ksIs = getClass().getClassLoader().getResourceAsStream(PKCS12_KEYSTORE);
		ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ksIs, PASSWORD.toCharArray());

		data = AOUtil.getDataFromInputStream(getClass().getClassLoader().getResourceAsStream(DATA_FILE));
	}

	/** Prueba de cofirma implicita de una firma impl&iacute;cita sin indicar los datos de firma.
	 * @throws Exception Cuando se produce un error. */
	@Test
	public void prueba_cofirmar_firma_implicita_sin_indicar_datos() throws Exception {
		final byte[] sign;
		try (
			final InputStream is = getClass().getClassLoader().getResourceAsStream(IMPLICIT_SHA1_SIGN_FILE);
		) {
			sign = AOUtil.getDataFromInputStream(is);
		}

		final Properties config = new Properties();
		config.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$

		final AOCAdESSigner signer = new AOCAdESSigner();
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PASSWORD.toCharArray()));
		final byte[] cosign = signer.cosign(
				sign,
				AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				config);

		final File tempFile = File.createTempFile("CosignCades", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("Prueba de cofirma implicita sobre firma implicita sin indicar los datos."); //$NON-NLS-1$
		System.out.println("El resultado se almacena en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(cosign);
		}
	}

	/** Prueba de cofirma impl&iacute;cita de una firma impl&iacute;cita CAdES-T sin indicar los datos de firma.
	 * @throws Exception Cuando se produce un error. */
	@Test
	public void prueba_cofirmar_cades_T() throws Exception {
		final byte[] sign;
		try (
			final InputStream is = getClass().getClassLoader().getResourceAsStream(
				IMPLICIT_SHA1_CADES_T_FILE
			);
		) {
			sign = AOUtil.getDataFromInputStream(is);
		}

		final byte[] originalData;
		try (
			final InputStream is2 = getClass().getClassLoader().getResourceAsStream(
				"FicheroOriginal_cades_t_csig.pdf" //$NON-NLS-1$
			);
		) {
			originalData = AOUtil.getDataFromInputStream(is2);
		}

		final Properties config = new Properties();
		config.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$

		final AOCAdESSigner signer = new AOCAdESSigner();
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(
			ks.aliases().nextElement(),
			new KeyStore.PasswordProtection(PASSWORD.toCharArray())
		);

		byte[] signature;
		try {
			signature = signer.cosign(
					originalData,
					sign,
				AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				config
			);
		}
		catch(final AOFormatFileException e) {
			Assert.fail("Se ha indicado que la firma no puede ser cofirmada: " + e); //$NON-NLS-1$
			return;
		}

		final File tempFile = File.createTempFile("CosignCades", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(signature);
		}

		System.out.println("Prueba de cofirma sobre firma CAdES-T"); //$NON-NLS-1$
		System.out.println("El resultado se almacena en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/** Prueba de cofirma impl&iacute;cita de una firma impl&iacute;cita CAdES-T sin indicar los datos de firma.
	 * @throws Exception Cuando se produce un error. */
	@Test
	public void prueba_cofirmar_cades_A_() throws Exception {
		final byte[] sign;
		try (
			final InputStream is = getClass().getClassLoader().getResourceAsStream(
				IMPLICIT_SHA1_CADES_A_FILE
			);
		) {
			sign = AOUtil.getDataFromInputStream(is);
		}

		final Properties config = new Properties();
		config.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$

		final AOCAdESSigner signer = new AOCAdESSigner();
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(
			ks.aliases().nextElement(),
			new KeyStore.PasswordProtection(PASSWORD.toCharArray())
		);
		try {
			signer.cosign(
				AOUtil.getDataFromInputStream(getClass().getClassLoader().getResourceAsStream(DATA_FILE_PDF)),
				sign,
				AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				config
			);
		}
		catch(final AOFormatFileException e) {
			System.out.println("Se ha indicado correctamente que la firma no puede ser cofirmada: " + e); //$NON-NLS-1$
			return;
		}
		Assert.fail("Deberia haber encontrado sellos y fallar"); //$NON-NLS-1$
	}

	/** Prueba de cofirma implicita de una firma implicita indicando los datos firmados.
	 * @throws Exception Cuando se produce un error.
	 */
	@Test
	public void prueba_cofirmar_firma_implicita_indicando_datos() throws Exception {
		final byte[] sign;
		try (
			final InputStream is = getClass().getClassLoader().getResourceAsStream(IMPLICIT_SHA1_SIGN_FILE);
		) {
			sign = AOUtil.getDataFromInputStream(is);
		}

		final Properties config = new Properties();
		config.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$

		final AOCAdESSigner signer = new AOCAdESSigner();
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PASSWORD.toCharArray()));
		final byte[] cosign = signer.cosign(
			signer.getData(sign),
			sign,
			AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		final File tempFile = File.createTempFile("CosignCades", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("Prueba de cofirma implicita sobre firma implicita indicando datos."); //$NON-NLS-1$
		System.out.println("El resultado se almacena en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(cosign);
		}
	}

	/** Prueba de cofirma impl&iacute;cita de una firma expl&iacute;cita sin indicar los datos de firma.
	 * @throws Exception Cuando se produce un error. */
	@Test
	public void prueba_cofirma_implicita_de_firma_explicita_sin_indicar_datos() throws Exception {
		final byte[] sign;
		try (
			final InputStream is = getClass().getClassLoader().getResourceAsStream(EXPLICIT_SHA1_SIGN_FILE);
		) {
			sign = AOUtil.getDataFromInputStream(is);
		}

		final Properties config = new Properties();
		config.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$

		final AOCAdESSigner signer = new AOCAdESSigner();
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PASSWORD.toCharArray()));
		final byte[] cosign = signer.cosign(
			sign,
			AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		final File tempFile = File.createTempFile("CosignCades", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("Prueba de cofirma implicita sobre firma explicita sin indicar los datos."); //$NON-NLS-1$
		System.out.println("El resultado se almacena en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(cosign);
		}
	}

	/**
	 * Prueba de cofirma implicita de una firma explicita indicando los datos firmados.
	 * @throws Exception Cuando se produce un error.
	 */
	@Test
	public void prueba_cofirma_implicita_de_firma_explicita_indicando_datos() throws Exception {
		final byte[] sign;
		try (
			final InputStream is = getClass().getClassLoader().getResourceAsStream(EXPLICIT_SHA1_SIGN_FILE);
		) {
			sign = AOUtil.getDataFromInputStream(is);
		}

		final Properties config = new Properties();
		config.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$

		final AOCAdESSigner signer = new AOCAdESSigner();
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PASSWORD.toCharArray()));
		final byte[] cosign = signer.cosign(
			data,
			sign,
			AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		final File tempFile = File.createTempFile("CosignCades", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("Prueba de cofirma implicita sobre firma explicita indicando datos."); //$NON-NLS-1$
		System.out.println("El resultado se almacena en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(cosign);
		}
	}

	/** Prueba de cofirma expl&iacute;cita de una firma explicita indicando el hash de los datos firmados.
	 * @throws Exception Cuando se produce un error. */
	@Test
	public void prueba_cofirma_explicita_de_firma_explicita_indicando_hash() throws Exception {
		final byte[] sign;
		try (
			final InputStream is = getClass().getClassLoader().getResourceAsStream(EXPLICIT_SHA1_SIGN_FILE);
		) {
			sign = AOUtil.getDataFromInputStream(is);
		}

		final MessageDigest md = MessageDigest.getInstance("SHA1"); //$NON-NLS-1$
		md.update(data);
		final byte[] messageDigest = md.digest();

		final Properties config = new Properties();
		config.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$
		config.setProperty("precalculatedHashAlgorithm", "SHA1"); //$NON-NLS-1$ //$NON-NLS-2$

		final AOCAdESSigner signer = new AOCAdESSigner();
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PASSWORD.toCharArray()));
		final byte[] cosign = signer.cosign(
			messageDigest,
			sign,
			AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		final File tempFile = File.createTempFile("CosignCades", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("Prueba de cofirma implicita sobre firma explicita indicando datos."); //$NON-NLS-1$
		System.out.println("El resultado se almacena en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(cosign);
		}
	}

	/** Prueba de cofirma expl&iacute;cita de una firma expl&iacute;cita ambas generadas con el mismo algoritmo.
	 * @throws Exception Cuando se produce un error. */
	@Test
	public void prueba_cofirma_explicita_de_firma_explicita_mismo_algoritmo_sin_datos() throws Exception {
		final byte[] sign;
		try (
			final InputStream is = getClass().getClassLoader().getResourceAsStream(EXPLICIT_SHA1_SIGN_FILE);
		) {
			sign = AOUtil.getDataFromInputStream(is);
		}

		final Properties config = new Properties();
		config.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$

		final AOCAdESSigner signer = new AOCAdESSigner();
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PASSWORD.toCharArray()));
		final byte[] cosign = signer.cosign(
			sign,
			AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		final File tempFile = File.createTempFile("CosignCades", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("Prueba de cofirma explicita sobre firma explicita ambas generadas con el mismo algoritmo y sin indicar los datos."); //$NON-NLS-1$
		System.out.println("El resultado se almacena en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(cosign);
		}
	}

	/** Prueba de cofirma expl&iacute;cita de una firma expl&iacute;cita ambas generadas con distinto algoritmo.
	 * @throws Exception Cuando se produce un error. */
	@Test
	public void prueba_cofirma_explicita_de_firma_explicita_distinto_algoritmo_sin_datos() throws Exception {
		final byte[] sign;
		try (
			final InputStream is = getClass().getClassLoader().getResourceAsStream(EXPLICIT_SHA1_SIGN_FILE);
		) {
			sign = AOUtil.getDataFromInputStream(is);
		}

		final Properties config = new Properties();
		config.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$

		final AOCAdESSigner signer = new AOCAdESSigner();

		try {
			final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PASSWORD.toCharArray()));
			signer.cosign(
					sign,
					AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
					pke.getPrivateKey(),
					pke.getCertificateChain(),
					config);
		}
		catch (final AOException e) {
			e.printStackTrace();
			return;
		}

		Assert.fail("La firma tuvo que haber lanzado una excepcion de tipo AOException"); //$NON-NLS-1$
	}

	/** Cierra el flujo de lectura del almac&eacute;n de certificados.
	 * @throws IOException Cuando ocurre alg&uacute;n problema al cerrar el flujo de datos. */
	@SuppressWarnings("static-method")
	@After
	public void cerrar() throws IOException {
		ksIs.close();
	}
}
