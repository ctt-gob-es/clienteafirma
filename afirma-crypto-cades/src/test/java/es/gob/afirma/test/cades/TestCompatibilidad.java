package es.gob.afirma.test.cades;

import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.cades.AOCAdESSigner;

public class TestCompatibilidad {

	private static final String CMS_SIGN_FILENAME = "cms_implicit.csig";
	
	private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
	private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

	
	@Test
	public void testCofirmaCadesSobreCms() throws Exception {
		
		byte[] signature = loadFile(CMS_SIGN_FILENAME);
			
		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
		
		Properties extraParams = new Properties();
		extraParams.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT);
		
		new AOCAdESSigner().cosign(
				signature,
				AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				extraParams);
	}
	
	private static byte[] loadFile(String filename) throws IOException {
		
		final InputStream is = TestCompatibilidad.class.getClassLoader().getResourceAsStream(CMS_SIGN_FILENAME);
		byte[] data = AOUtil.getDataFromInputStream(is);
		is.close();
		
		return data;
	}
}
