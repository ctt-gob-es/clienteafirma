
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

/** Prueba de contrafirmas CAdES.
 * @author Carlos Gamuci */
public class TestCountersignNode {

	private static final String PKCS12_KEYSTORE = "ANCERTCCP_FIRMA.p12"; //$NON-NLS-1$
	private static final String PWD = "1111"; //$NON-NLS-1$
	private static final String PKCS12_KEYSTORE2 = "ANCERTCE_FIRMA.p12"; //$NON-NLS-1$
	private static final String PWD2 = "1111"; //$NON-NLS-1$
	private static final String SIGN_FILE = "cades_3nodos.csig"; //$NON-NLS-1$

	private static InputStream ksIs;
	private static KeyStore ks;
	private static InputStream ksIs2;
	private static KeyStore ks2;

	/** Carga el almac&eacute;n de certificados.
	 * @throws Exception Cuando ocurre algun problema al cargar el almac&eacute;n o los datos. */
	@Before
	public void cargaAlmacen() throws Exception {
		ksIs = getClass().getClassLoader().getResourceAsStream(PKCS12_KEYSTORE);
		ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ksIs, PWD.toCharArray());
		ksIs2 = getClass().getClassLoader().getResourceAsStream(PKCS12_KEYSTORE2);
		ks2 = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks2.load(ksIs2, PWD2.toCharArray());
	}

	/** Prueba general.
	 * @throws Exception en cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void pruebaContrafirmaProgresiva() throws Exception {

		final PrivateKeyEntry pkeFirma = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PWD.toCharArray()));

		final AOCAdESSigner signer = new AOCAdESSigner();

		byte[] sign = signer.sign(
			"HOLA".getBytes(), //$NON-NLS-1$
			"SHA512withRSA", //$NON-NLS-1$
			pkeFirma.getPrivateKey(),
			pkeFirma.getCertificateChain(),
			new Properties()
		);

		sign = signer.cosign(
				sign,
				"SHA512withRSA", //$NON-NLS-1$
				pkeFirma.getPrivateKey(),
				pkeFirma.getCertificateChain(),
				new Properties()
		);


		final PrivateKeyEntry pkeContrafirma = (PrivateKeyEntry) ks2.getEntry(ks2.aliases().nextElement(), new KeyStore.PasswordProtection(PWD2.toCharArray()));

		sign = signer.countersign(
				sign,
				AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
				CounterSignTarget.TREE,
				null,
				pkeContrafirma.getPrivateKey(),
				pkeContrafirma.getCertificateChain(),
				null
		);

		sign = signer.countersign(
				sign,
				AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
				CounterSignTarget.LEAFS,
				null,
				pkeContrafirma.getPrivateKey(),
				pkeContrafirma.getCertificateChain(),
				null
		);

		final File tempFile = File.createTempFile("CountersignCades-1-Node-", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$

		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(sign);
		}
	}

	/** Prueba de contrafirma de hojas de la firma de entrada.
	 * @throws Exception Cuando se produce un error. */
	@Test
	public void pruebaContrafirmaHojas() throws Exception {
		final byte[] sign;
		try (
			final InputStream is = getClass().getClassLoader().getResourceAsStream(SIGN_FILE);
		) {
			sign = AOUtil.getDataFromInputStream(is);
		}

		final Properties config = new Properties();

		final AOCAdESSigner signer = new AOCAdESSigner();
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PWD.toCharArray()));
		byte[] countersign = signer.countersign(
			sign,
			AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
			CounterSignTarget.LEAFS,
			null,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		// Repetimos!
		countersign = signer.countersign(
			sign,
			AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
			CounterSignTarget.LEAFS,
			null,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		final File tempFile = File.createTempFile("CountersignCadesLeafs", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("Resultado de la prueba de contrafirma de hojas: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(countersign);
		}
	}

	/** Prueba de contrafirma de &aacute;rbol de la firma de entrada.
	 * @throws Exception Cuando se produce un error. */
	@Test
	public void pruebaContrafirmaArbol() throws Exception {
		final byte[] sign;
		try (
			final InputStream is = getClass().getClassLoader().getResourceAsStream(SIGN_FILE);
		) {
			sign = AOUtil.getDataFromInputStream(is);
		}

		final Properties config = new Properties();

		final AOCAdESSigner signer = new AOCAdESSigner();
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PWD.toCharArray()));
		byte[] countersign = signer.countersign(
				sign,
				AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
				CounterSignTarget.TREE,
				null,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				config
		);

		File tempFile = File.createTempFile("CountersignCadesTree-001-", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("Resultado de la prueba de contrafirma de arbol: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

		try (
			OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(countersign);
		}

		// Repetimos!
		countersign = signer.countersign(
			countersign,
			AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
			CounterSignTarget.TREE,
			null,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		tempFile = File.createTempFile("CountersignCadesTree-002-", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("Resultado de la prueba de contrafirma de arbol: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

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
		ksIs2.close();
	}
}
