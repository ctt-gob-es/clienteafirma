
package es.gob.afirma.signers.multi.cades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import junit.framework.Assert;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.cades.AOCAdESSigner;

/**
 * Prueba de cofirmas CAdES.
 * @author Carlos Gamuci
 */
public class TestCosign {

	private static final String PKCS12_KEYSTORE = "ANF_PF_Activo.pfx"; //$NON-NLS-1$

	private static final String PASSWORD = "12341234"; //$NON-NLS-1$

	private static final String IMPLICIT_SHA1_SIGN_FILE = "firma_implicita.csig"; //$NON-NLS-1$

	private static final String EXPLICIT_SHA1_SIGN_FILE = "firma_explicita.csig"; //$NON-NLS-1$

	private static final String DATA_FILE = "data"; //$NON-NLS-1$

	private static InputStream ksIs;
	private static KeyStore ks;

	private static byte[] data;

	@Before
	public void cargaAlmacen() throws Exception {
		ksIs = AOUtil.getCleanClassLoader().getResourceAsStream(PKCS12_KEYSTORE);
		ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ksIs, PASSWORD.toCharArray());

		data = AOUtil.getDataFromInputStream(AOUtil.getCleanClassLoader().getResourceAsStream(DATA_FILE));
	}

	/**
	 * Prueba de cofirma implicita de una firma implicita sin indicar los datos de firma.
	 * @throws Exception Cuando se produce un error.
	 */
	@Test
	public void prueba_cofirmar_firma_implicita_sin_indicar_datos() throws Exception {

		final InputStream is = AOUtil.getCleanClassLoader().getResourceAsStream(IMPLICIT_SHA1_SIGN_FILE);

		final byte[] sign = AOUtil.getDataFromInputStream(is);

		final Properties config = new Properties();
		config.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT);

		final AOCAdESSigner signer = new AOCAdESSigner();
		final byte[] cosign = signer.cosign(
				sign,
				AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
				(PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PASSWORD.toCharArray())),
				config);

		try {
			is.close();
		} catch (final Exception e) {
			// Se obvia el error
		}

		final File tempFile = File.createTempFile("CosignCades", ".csig");

		System.out.println("Prueba de cofirma implicita sobre firma implicita sin indicar los datos.");
		System.out.println("El resultado de almacena en: " + tempFile.getAbsolutePath());

		final FileOutputStream fos = new FileOutputStream(tempFile);
		fos.write(cosign);
		try {
			fos.close();
		} catch (final Exception e) {
			// Se obvia el error
		}
	}

	/**
	 * Prueba de cofirma implicita de una firma implicita indicando los datos firmados.
	 * @throws Exception Cuando se produce un error.
	 */
	@Test
	public void prueba_cofirmar_firma_implicita_indicando_datos() throws Exception {

		final InputStream is = AOUtil.getCleanClassLoader().getResourceAsStream(IMPLICIT_SHA1_SIGN_FILE);

		final byte[] sign = AOUtil.getDataFromInputStream(is);

		final Properties config = new Properties();
		config.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT);

		final AOCAdESSigner signer = new AOCAdESSigner();
		final byte[] cosign = signer.cosign(
				signer.getData(sign),
				sign,
				AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
				(PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PASSWORD.toCharArray())),
				config);

		try {
			is.close();
		} catch (final Exception e) {
			// Se obvia el error
		}

		final File tempFile = File.createTempFile("CosignCades", ".csig");

		System.out.println("Prueba de cofirma implicita sobre firma implicita indicando datos.");
		System.out.println("El resultado de almacena en: " + tempFile.getAbsolutePath());

		final FileOutputStream fos = new FileOutputStream(tempFile);
		fos.write(cosign);
		try {
			fos.close();
		} catch (final Exception e) {
			// Se obvia el error
		}
	}

	/**
	 * Prueba de cofirma implicita de una firma explicita sin indicar los datos de firma.
	 * @throws Exception Cuando se produce un error.
	 */
	@Test
	public void prueba_cofirma_implicita_de_firma_explicita_sin_indicar_datos() throws Exception {

		final InputStream is = AOUtil.getCleanClassLoader().getResourceAsStream(EXPLICIT_SHA1_SIGN_FILE);

		final byte[] sign = AOUtil.getDataFromInputStream(is);

		final Properties config = new Properties();
		config.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT);

		final AOCAdESSigner signer = new AOCAdESSigner();
		final byte[] cosign = signer.cosign(
				sign,
				AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
				(PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PASSWORD.toCharArray())),
				config);

		try {
			is.close();
		} catch (final Exception e) {
			// Se obvia el error
		}

		final File tempFile = File.createTempFile("CosignCades", ".csig");

		System.out.println("Prueba de cofirma implicita sobre firma explicita sin indicar los datos.");
		System.out.println("El resultado de almacena en: " + tempFile.getAbsolutePath());

		final FileOutputStream fos = new FileOutputStream(tempFile);
		fos.write(cosign);
		try {
			fos.close();
		} catch (final Exception e) {
			// Se obvia el error
		}
	}

	/**
	 * Prueba de cofirma implicita de una firma explicita indicando los datos firmados.
	 * @throws Exception Cuando se produce un error.
	 */
	@Test
	public void prueba_cofirma_implicita_de_firma_explicita_indicando_datos() throws Exception {

		final InputStream is = AOUtil.getCleanClassLoader().getResourceAsStream(EXPLICIT_SHA1_SIGN_FILE);

		final byte[] sign = AOUtil.getDataFromInputStream(is);

		final Properties config = new Properties();
		config.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT);

		final AOCAdESSigner signer = new AOCAdESSigner();
		final byte[] cosign = signer.cosign(
				data,
				sign,
				AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
				(PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PASSWORD.toCharArray())),
				config);

		try {
			is.close();
		} catch (final Exception e) {
			// Se obvia el error
		}

		final File tempFile = File.createTempFile("CosignCades", ".csig");

		System.out.println("Prueba de cofirma implicita sobre firma explicita indicando datos.");
		System.out.println("El resultado de almacena en: " + tempFile.getAbsolutePath());

		final FileOutputStream fos = new FileOutputStream(tempFile);
		fos.write(cosign);
		try {
			fos.close();
		} catch (final Exception e) {
			// Se obvia el error
		}
	}

	/**
	 * Prueba de cofirma explicita de una firma explicita ambas generadas con el mismo algoritmo.
	 * @throws Exception Cuando se produce un error.
	 */
	@Test
	public void prueba_cofirma_explicita_de_firma_explicita_mismo_algoritmo_sin_datos() throws Exception {

		final InputStream is = AOUtil.getCleanClassLoader().getResourceAsStream(EXPLICIT_SHA1_SIGN_FILE);

		final byte[] sign = AOUtil.getDataFromInputStream(is);

		final Properties config = new Properties();
		config.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT);

		final AOCAdESSigner signer = new AOCAdESSigner();
		final byte[] cosign = signer.cosign(
				sign,
				AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
				(PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PASSWORD.toCharArray())),
				config);

		try {
			is.close();
		} catch (final Exception e) {
			// Se obvia el error
		}

		final File tempFile = File.createTempFile("CosignCades", ".csig");

		System.out.println("Prueba de cofirma explicita sobre firma explicita ambas generadas con el mismo algoritmo y sin indicar los datos.");
		System.out.println("El resultado de almacena en: " + tempFile.getAbsolutePath());

		final FileOutputStream fos = new FileOutputStream(tempFile);
		fos.write(cosign);
		try {
			fos.close();
		} catch (final Exception e) {
			// Se obvia el error
		}
	}

	/**
	 * Prueba de cofirma explicita de una firma explicita ambas generadas con distinto algoritmo.
	 * @throws Exception Cuando se produce un error.
	 */
	@Test
	public void prueba_cofirma_explicita_de_firma_explicita_distinto_algoritmo_sin_datos() throws Exception {

		final InputStream is = AOUtil.getCleanClassLoader().getResourceAsStream(EXPLICIT_SHA1_SIGN_FILE);

		final byte[] sign = AOUtil.getDataFromInputStream(is);

		final Properties config = new Properties();
		config.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT);

		final AOCAdESSigner signer = new AOCAdESSigner();

		try {
			final byte[] cosign = signer.cosign(
					sign,
					AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
					(PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PASSWORD.toCharArray())),
					config);
		} catch (final AOException e) {
			e.printStackTrace();
			return;
		}

		Assert.fail("La firma tuvo que haber lanzado una excepcion de tipo AOException");
	}

	@After
	public void cerrar() {
		try {
			ksIs.close();
		} catch (final Exception e) {
			// Se obvia el error
		}

	}
}
