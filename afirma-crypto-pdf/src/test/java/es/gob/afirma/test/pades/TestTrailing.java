package es.gob.afirma.test.pades;

import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.signers.pades.AOPDFSigner;

/** Pruebas espec&iacute;ficas para PDF con datos tras la marca de fin de fichero.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestTrailing {

	private final static String TEST_FILE = "TEST_PDF_Trailed.pdf"; //$NON-NLS-1$

	private final static String DEFAULT_SIGNATURE_ALGORITHM = "SHA512withRSA"; //$NON-NLS-1$

	private final static String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
	private final static String CERT_PASS = "12341234"; //$NON-NLS-1$
	private final static String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

	/** Prueba de opciones de creaci&oacute;n de revisiones en firmas de PDF con datos tras
	 * la marca de fin de fichero.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testRevisionOnTrailedPdf() throws Exception {
		
		final String EOF_INDICATOR = "%%EOF"; //$NON-NLS-1$

		final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILE));
		final AOPDFSigner signer = new AOPDFSigner();

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final Properties extraParams = new Properties();
        extraParams.put("alwaysCreateRevision", "true"); //$NON-NLS-1$ //$NON-NLS-2$

        Exception raised = null;
        try {
	        byte [] dataByteArray = signer.sign(
	    		testPdf,
	    		DEFAULT_SIGNATURE_ALGORITHM,
	    		pke.getPrivateKey(),
	    		pke.getCertificateChain(),
	    		extraParams
			);
	        
	        String result = new String(dataByteArray);
	        int count = 0;
	        int idx = 0;

	        while ((idx = result.indexOf(EOF_INDICATOR, idx)) != -1) {
	            count++;
	            idx += EOF_INDICATOR.length();
	        }

	        Assert.assertEquals(2, count);
        }
        catch(final Exception e) {
        	raised = e;
        }
        Assert.assertNull(raised);

        extraParams.put("alwaysCreateRevision", "false"); //$NON-NLS-1$ //$NON-NLS-2$

        try {
        	byte [] dataByteArray = signer.sign(
            		testPdf,
            		DEFAULT_SIGNATURE_ALGORITHM,
            		pke.getPrivateKey(),
            		pke.getCertificateChain(),
            		extraParams
        		);
        	
	        String result = new String(dataByteArray);
	        int count = 0;
	        int idx = 0;

	        while ((idx = result.indexOf(EOF_INDICATOR, idx)) != -1) {
	            count++;
	            idx += EOF_INDICATOR.length();
	        }

	        Assert.assertEquals(1, count);
        } catch(final Exception e) {
        	raised = e;
        }
        
        Assert.assertNull(raised);
        
	}

}
