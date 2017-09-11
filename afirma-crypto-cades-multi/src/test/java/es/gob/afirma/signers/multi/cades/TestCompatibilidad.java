package es.gob.afirma.signers.multi.cades;

import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.cades.AOCAdESSigner;

/** Pruebas de compatibilidad CAdES-CMS. */
public class TestCompatibilidad {

	private static final String CMS_SIGN_FILENAME = "cms_implicit.csig"; //$NON-NLS-1$

	private static final String CERT_PATH = "ANCERTCCP_FIRMA.p12"; //$NON-NLS-1$
	private static final String CERT_PASS = "1111"; //$NON-NLS-1$


	/** Prueba de cofirma CAdES sobre firma CMS.
	 * @throws Exception en caso de cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testCofirmaCadesSobreCms() throws Exception {

		final byte[] cmsSignature = loadFile(CMS_SIGN_FILENAME);

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ks.aliases().nextElement(), new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final Properties extraParams = new Properties();
		extraParams.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$

		new AOCAdESSigner().cosign(
				cmsSignature,
				AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				extraParams);
	}

	private static byte[] loadFile(final String filename) throws IOException {
		try (
			final InputStream is = TestCompatibilidad.class.getClassLoader().getResourceAsStream(filename);
		) {
			return AOUtil.getDataFromInputStream(is);
		}
	}
}
