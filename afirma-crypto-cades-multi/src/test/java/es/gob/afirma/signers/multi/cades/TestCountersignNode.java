
package es.gob.afirma.signers.multi.cades;

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
import es.gob.afirma.signers.cades.AOCAdESSigner;

/** Prueba de contrafirmas CAdES.
 * @author Carlos Gamuci */
public class TestCountersignNode {

	private static final String PKCS12_KEYSTORE = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
	private static final String PASSWORD = "12341234"; //$NON-NLS-1$
	private static final String SIGN_FILE = "cades_3nodos.csig"; //$NON-NLS-1$

	private static InputStream ksIs;
	private static KeyStore ks;

	/** Carga el almac&eacute;n de certificados.
	 * @throws Exception Cuando ocurre algun problema al cargar el almac&eacute;n o los datos. */
	@Before
	public void cargaAlmacen() throws Exception {
		ksIs = getClass().getClassLoader().getResourceAsStream(PKCS12_KEYSTORE);
		ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ksIs, PASSWORD.toCharArray());
	}

	/**
	 * Prueba de contrafirma del nodo 1 de la firma de entrada.
	 * @throws Exception Cuando se produce un error.
	 */
	@Test
	public void pruebaContrafirmaNode1() throws Exception {

		final InputStream is = getClass().getClassLoader().getResourceAsStream(SIGN_FILE);
		final byte[] sign = AOUtil.getDataFromInputStream(is);
		is.close();

		final Properties config = new Properties();

		final AOCAdESSigner signer = new AOCAdESSigner();
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PASSWORD.toCharArray()));
		final byte[] countersign = signer.countersign(
				sign,
				AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
				CounterSignTarget.NODES,
				new Integer[] { new Integer(1) },
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				config);

		final File tempFile = File.createTempFile("CountersignCadesNode1", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("Resultado de la prueba de contrafirma del Nodo 1: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

		final FileOutputStream fos = new FileOutputStream(tempFile);
		fos.write(countersign);
		fos.close();
	}

	/**
	 * Prueba de contrafirma del nodo 2 de la firma de entrada.
	 * @throws Exception Cuando se produce un error.
	 */
	@Test
	public void pruebaContrafirmaNode2() throws Exception {

		final InputStream is = getClass().getClassLoader().getResourceAsStream(SIGN_FILE);
		final byte[] sign = AOUtil.getDataFromInputStream(is);
		is.close();

		final Properties config = new Properties();

		final AOCAdESSigner signer = new AOCAdESSigner();
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PASSWORD.toCharArray()));
		final byte[] countersign = signer.countersign(
				sign,
				AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
				CounterSignTarget.NODES,
				new Integer[] { new Integer(2) },
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				config);

		final File tempFile = File.createTempFile("CountersignCadesNode2", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("Resultado de la prueba de contrafirma del Nodo 2: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

		final FileOutputStream fos = new FileOutputStream(tempFile);
		fos.write(countersign);
		fos.close();
	}

	/**
	 * Prueba de contrafirma del nodo 3 de la firma de entrada.
	 * @throws Exception Cuando se produce un error.
	 */
	@Test
	public void pruebaContrafirmaNode3() throws Exception {

		final InputStream is = getClass().getClassLoader().getResourceAsStream(SIGN_FILE);
		final byte[] sign = AOUtil.getDataFromInputStream(is);
		is.close();

		final Properties config = new Properties();

		final AOCAdESSigner signer = new AOCAdESSigner();
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PASSWORD.toCharArray()));
		final byte[] countersign = signer.countersign(
				sign,
				AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
				CounterSignTarget.NODES,
				new Integer[] { new Integer(3) },
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				config);

		final File tempFile = File.createTempFile("CountersignCadesNode3", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("Resultado de la prueba de contrafirma del Nodo 3: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

		final FileOutputStream fos = new FileOutputStream(tempFile);
		fos.write(countersign);
		fos.close();
	}

	/**
	 * Prueba de contrafirma de hojas de la firma de entrada.
	 * @throws Exception Cuando se produce un error.
	 */
	@Test
	public void pruebaContrafirmaHojas() throws Exception {

		final InputStream is = getClass().getClassLoader().getResourceAsStream(SIGN_FILE);
		final byte[] sign = AOUtil.getDataFromInputStream(is);
		is.close();

		final Properties config = new Properties();

		final AOCAdESSigner signer = new AOCAdESSigner();
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PASSWORD.toCharArray()));
		final byte[] countersign = signer.countersign(
				sign,
				AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
				CounterSignTarget.LEAFS,
				null,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				config);

		final File tempFile = File.createTempFile("CountersignCadesLeafs", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("Resultado de la prueba de contrafirma de hojas: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

		final FileOutputStream fos = new FileOutputStream(tempFile);
		fos.write(countersign);
		fos.close();
	}

	/**
	 * Prueba de contrafirma de &aacute;rbol de la firma de entrada.
	 * @throws Exception Cuando se produce un error.
	 */
	@Test
	public void pruebaContrafirmaArbol() throws Exception {

		final InputStream is = getClass().getClassLoader().getResourceAsStream(SIGN_FILE);
		final byte[] sign = AOUtil.getDataFromInputStream(is);
		is.close();

		final Properties config = new Properties();

		final AOCAdESSigner signer = new AOCAdESSigner();
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PASSWORD.toCharArray()));
		final byte[] countersign = signer.countersign(
				sign,
				AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
				CounterSignTarget.TREE,
				null,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				config);

		final File tempFile = File.createTempFile("CountersignCadesTree", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("Resultado de la prueba de contrafirma de arbol: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

		final FileOutputStream fos = new FileOutputStream(tempFile);
		fos.write(countersign);
		fos.close();
	}

	/** Cierra el flujo de lectura del almac&eacute;n de certificados.
	 * @throws IOException Cuando ocurre alg&uacute;n problema al cerrar el flujo de datos. */
	@SuppressWarnings("static-method")
	@After
	public void cerrar() throws IOException {
		ksIs.close();
	}
}
