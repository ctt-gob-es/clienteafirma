
package es.gob.afirma.signers.multi.cades;

import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import junit.framework.Assert;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.multi.cades.triphase.AOCAdESTriPhaseCounterSigner;
import es.gob.afirma.signers.multi.cades.triphase.PreSignData;

/** Prueba de contrafirmas CAdES.
 * @author Carlos Gamuci */
public final class TestTriphaseCountersign {

	private static final String PKCS12_KEYSTORE = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
	private static final String PASSWORD = "12341234"; //$NON-NLS-1$
	private static final String IMPLICIT_SHA1_COUNTERSIGN_FILE = "contrafirma_implicita.csig"; //$NON-NLS-1$

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

	/** Prueba de contrafirma trif&aacute;sica de todo el &aacute;rbol de firmas de una firma
	 * expl&iacute;cita.
	 * @throws Exception Cuando se produce un error. */
	@Test
	public void testTriphaseTreeCountersign() throws Exception {

		final InputStream is = getClass().getClassLoader().getResourceAsStream(IMPLICIT_SHA1_COUNTERSIGN_FILE);
		final byte[] sign = AOUtil.getDataFromInputStream(is);
		is.close();

		final Properties config = new Properties();

		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(PASSWORD.toCharArray()));
		final String preCountersign = AOCAdESTriPhaseCounterSigner.preCountersign(
			sign,
			AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
			CounterSignTarget.TREE,
			null,
			pke.getCertificateChain(),
			config
		);

		System.out.println(preCountersign);

		Assert.assertNotNull(PreSignData.getInstance(preCountersign.getBytes()));
	}

	/** Cierra el flujo de lectura del almac&eacute;n de certificados.
	 * @throws IOException Cuando ocurre alg&uacute;n problema al cerrar el flujo de datos. */
	@SuppressWarnings("static-method")
	@After
	public void cerrar() throws IOException {
		ksIs.close();
	}
}
