package es.gob.afirma.signers.multi.cades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.cades.AOCAdESSigner;

/** Prueba de contrafirma indicando el ContentType en la firma. */
public final class TestINC309356 {

	private static final String DATA = "data"; //$NON-NLS-1$
	private static final String SIGNATURE_WITH_CONTENT_TYPE = "firma_contentType_TIFF.csig"; //$NON-NLS-1$
	private static final String PKCS12_KEYSTORE = "ANCERTCCP_FIRMA.p12"; //$NON-NLS-1$
	private static final String PWD = "1111"; //$NON-NLS-1$

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

	/** Prueba asociada a la incidencia INC 309356. Permite comprobar que las contrafirmas
	 * CAdES no se generan con el contentType "data" por defecto.
	 * @throws Exception Cuando ocurre un error. */
	@Test
	public void testContentTypePorDefecto() throws Exception {
		final byte[] data;
		try (
			final InputStream is = getClass().getClassLoader().getResourceAsStream(DATA);
		) {
			data = AOUtil.getDataFromInputStream(is);
		}

		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PWD.toCharArray()));

		final Properties config = new Properties();
		final AOCAdESSigner signer = new AOCAdESSigner();

		final byte[] signature = signer.sign(
				data,
				AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				config
			);

		final byte[] countersign = signer.countersign(
			signature,
			AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
			CounterSignTarget.TREE,
			null,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		final File tempFile = File.createTempFile("Sign-CountersignCades", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("Prueba de firma y contrafirma consecutiva."); //$NON-NLS-1$
		System.out.println("El resultado se almacena en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(countersign);
		}
	}

	/** Prueba asociada a la incidencia INC 309356. Permite comprobar que las contrafirmas
	 * CAdES no se generan con el contentType "data" cuando se indica el OID del tipo de contenido.
	 * @throws Exception Cuando ocurre un error. */
	@Test
	public void testIndicandoContentType() throws Exception {
		final byte[] data;
		try (
			final InputStream is = getClass().getClassLoader().getResourceAsStream(DATA);
		) {
			data = AOUtil.getDataFromInputStream(is);
		}

		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PWD.toCharArray()));

		final Properties config = new Properties();
		config.setProperty("contentTypeOid", "1.2.840.10003.5.109.4"); //$NON-NLS-1$ //$NON-NLS-2$
		config.setProperty("contentTypeDescription", "image/tiff"); //$NON-NLS-1$ //$NON-NLS-2$

		final AOCAdESSigner signer = new AOCAdESSigner();

		final byte[] signature = signer.sign(
				data,
				AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				config
			);

		final byte[] countersign = signer.countersign(
			signature,
			AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
			CounterSignTarget.TREE,
			null,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		final File tempFile = File.createTempFile("Sign-CountersignCades", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("Prueba de firma y contrafirma consecutiva."); //$NON-NLS-1$
		System.out.println("El resultado se almacena en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(countersign);
		}
	}

	/** Prueba asociada a la incidencia INC 309356. Permite comprobar que las contrafirmas
	 * CAdES no se generan con el contentType "data" cuando se indica el OID del tipo de contenido.
	 * @throws Exception Cuando ocurre un error. */
	@Test
	public void testContrafirmaSobreFirmaConContentType() throws Exception {
		final byte[] signature;
		try (
			final InputStream is = getClass().getClassLoader().getResourceAsStream(SIGNATURE_WITH_CONTENT_TYPE);
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PWD.toCharArray()));

		final Properties config = new Properties();

		final AOCAdESSigner signer = new AOCAdESSigner();

		final byte[] countersign = signer.countersign(
			signature,
			AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
			CounterSignTarget.TREE,
			null,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		final File tempFile = File.createTempFile("Sign-CountersignCades", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("Prueba de contrafirma sobre firma con ContentType."); //$NON-NLS-1$
		System.out.println("El resultado se almacena en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

		try (
			final OutputStream fos = new FileOutputStream(tempFile);
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
