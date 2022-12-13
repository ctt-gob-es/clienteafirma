package es.gob.afirma.signers.xades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;

/** Prueba asociada a la incidencia #78353 de cofirma y contrafirma de firmas XAdES-A. */
public class TestINC78353 {

	private static final String FILE_XADES_A_DETACHED = "78353__XAdES_A_Detached.xml"; //$NON-NLS-1$
	private static final String FILE_XADES_A_ENVELOPING = "78353__XAdES_A_Enveloping.xml"; //$NON-NLS-1$
	private static final String PKCS12_KEYSTORE = "PFActivoFirSHA256.pfx"; //$NON-NLS-1$
	private static final String PWD = "12341234"; //$NON-NLS-1$

	private static InputStream ksIs;
	private static KeyStore ks;

	/** Carga el almac&eacute;n de certificados.
	 * @throws Exception Cuando ocurre algun problema al cargar el almac&eacute;n o los datos. */
	@Before
	public void cargaAlmacen() throws Exception {
		ksIs = getClass().getClassLoader().getResourceAsStream(PKCS12_KEYSTORE);
		ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ksIs, PWD.toCharArray());
	}

	/**
	 * Cofirma de firma XAdES-A Detached.
	 * @throws Exception Cuando ocurre un error.
	 */
	@Test
	public void testCofirmaXAdESADetached() throws Exception {

		final byte[] signature;
		try (
			final InputStream is = getClass().getClassLoader().getResourceAsStream(FILE_XADES_A_DETACHED);
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PWD.toCharArray()));

		final Properties config = new Properties();
		config.setProperty(XAdESExtraParams.ALLOW_SIGN_LTS_SIGNATURES, "true"); //$NON-NLS-1$

		final AOXAdESSigner signer = new AOXAdESSigner();

		final byte[] cosign = signer.cosign(
				signature,
				AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				config
			);

		final File tempFile = File.createTempFile("XAdES-A-Detached-CoSign", ".xsig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("El resultado de cofirma de firma XAdES-A Detached se almacena en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

		try (
			final FileOutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(cosign);
		}
	}

	/**
	 * Contrafirma de firma XAdES-A Detached.
	 * @throws Exception Cuando ocurre un error.
	 */
	@Test
	public void testContrafirmaXAdESADetached() throws Exception {

		final byte[] signature;
		try (
			final InputStream is = getClass().getClassLoader().getResourceAsStream(FILE_XADES_A_DETACHED);
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PWD.toCharArray()));

		final Properties config = new Properties();
		config.setProperty(XAdESExtraParams.ALLOW_SIGN_LTS_SIGNATURES, "true"); //$NON-NLS-1$

		final AOXAdESSigner signer = new AOXAdESSigner();

		final byte[] countersign = signer.countersign(
			signature,
			AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
			CounterSignTarget.TREE,
			null,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		final File tempFile = File.createTempFile("XAdES-A-Detached-CounterSign", ".xsig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("El resultado de contrafirma de firma XAdES-A Detached se almacena en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

		try (
			final FileOutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(countersign);
		}
	}

	/**
	 * Cofirma de firma XAdES-A Enveloping.
	 * @throws Exception Cuando ocurre un error.
	 */
	@Test
	public void testCofirmaXAdESAEnveloping() throws Exception {

		final byte[] signature;
		try (
			final InputStream is = getClass().getClassLoader().getResourceAsStream(FILE_XADES_A_ENVELOPING);
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PWD.toCharArray()));

		final Properties config = new Properties();
		config.setProperty(XAdESExtraParams.ALLOW_SIGN_LTS_SIGNATURES, "true"); //$NON-NLS-1$

		final AOXAdESSigner signer = new AOXAdESSigner();

		final byte[] cosign = signer.cosign(
				signature,
				AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				config
			);

		final File tempFile = File.createTempFile("XAdES-A-Enveloping-CoSign", ".xsig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("El resultado de cofirma de firma XAdES-A Enveloping se almacena en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

		try (
			final FileOutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(cosign);
		}
	}

	/** Contrafirma de firma XAdES-A Enveloping.
	 * @throws Exception Cuando ocurre un error. */
	@Test
	public void testContrafirmaXAdESAEnveloping() throws Exception {

		final byte[] signature;
		try (
			final InputStream is = getClass().getClassLoader().getResourceAsStream(FILE_XADES_A_ENVELOPING);
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PWD.toCharArray()));

		final Properties config = new Properties();
		config.setProperty(XAdESExtraParams.ALLOW_SIGN_LTS_SIGNATURES, "true"); //$NON-NLS-1$

		final AOXAdESSigner signer = new AOXAdESSigner();

		final byte[] countersign = signer.countersign(
			signature,
			AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
			CounterSignTarget.TREE,
			null,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		final File tempFile = File.createTempFile("XAdES-A-Enveloping-CounterSign", ".xsig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("El resultado de contrafirma de firma XAdES-A Enveloping se almacena en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

		try (
			final FileOutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(countersign);
		}
	}

	/** Cierra el flujo de lectura del almac&eacute;n de certificados.
	 * @throws IOException Cuando ocurre alg&uacute;n problema al cerrar el flujo de datos. */
	@SuppressWarnings("static-method")
	@After
	public void cerrar() throws IOException {
		ksIs.close();
	}
}
