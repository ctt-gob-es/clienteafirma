
package es.gob.afirma.signers.cadestri.client;

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
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOCounterSigner;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;

/** Prueba de cofirmas CAdES.
 * @author Carlos Gamuci. */
public class TestTriphaseCountersign {

	private static final String PKCS12_KEYSTORE = "ANF_PF_Activo.pfx"; //$NON-NLS-1$

	private static final String PWD = "12341234"; //$NON-NLS-1$

	private static final String SHA1_COUNTERSIGN_FILE = "contrafirma_implicita.csig"; //$NON-NLS-1$

	private static final String SERVLET_URL = "http://localhost:8080/afirma-server-triphase-signer/SignatureService"; //$NON-NLS-1$

	private static final String PARAM_NAME_SERVER_URL = "serverUrl"; //$NON-NLS-1$

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

	/** Prueba de contrafirma de todo el &aacute;rbol de firmas de una firma impl&iacute;cita.
	 * @throws Exception Cuando se produce un error. */
	@Test
	@Ignore // Necesita el servidor
	public void prueba_contrafirma_de_arbol_de_firma_implicita() throws Exception {

		final byte[] sign;
		try (
			final InputStream is = getClass().getClassLoader().getResourceAsStream(SHA1_COUNTERSIGN_FILE);
		) {
			sign = AOUtil.getDataFromInputStream(is);
		}

		final Properties config = new Properties();
		config.setProperty(PARAM_NAME_SERVER_URL, SERVLET_URL);

		final AOCounterSigner signer = new AOCAdESTriPhaseSigner();
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PWD.toCharArray()));
		final byte[] countersign = signer.countersign(
			sign,
			AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
			CounterSignTarget.TREE,
			null,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		final File tempFile = File.createTempFile("CountersignCades", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("Prueba de contrafirma de arbol sobre firma implicita."); //$NON-NLS-1$
		System.out.println("El resultado se almacena en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(countersign);
		}
	}

	/** Prueba de contrafirma de los nodos hoja de una firma impl&iacute;cita.
	 * @throws Exception Cuando se produce un error. */
	@Test
	@Ignore // Necesita el servidor
	public void prueba_contrafirma_de_firma_implicita_nodos_hoja() throws Exception {
		final byte[] sign;
		try (
			final InputStream is = getClass().getClassLoader().getResourceAsStream(SHA1_COUNTERSIGN_FILE);
		) {
			sign = AOUtil.getDataFromInputStream(is);
		}

		final Properties config = new Properties();
		config.setProperty(PARAM_NAME_SERVER_URL, SERVLET_URL);

		final AOCounterSigner signer = new AOCAdESTriPhaseSigner();
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PWD.toCharArray()));
		final byte[] countersign = signer.countersign(
				sign,
				AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
				CounterSignTarget.LEAFS,
				null,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				config);

		final File tempFile = File.createTempFile("CountersignCades", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$

		System.out.println("Prueba de contrafirma de nodos hoja sobre firma implicita."); //$NON-NLS-1$
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
