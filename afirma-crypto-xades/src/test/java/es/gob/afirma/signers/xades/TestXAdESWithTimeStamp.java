package es.gob.afirma.signers.xades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.tsp.pkcs7.TsaParams;

/** Pruebas de XAdES con sellos de tiempo. */
public final class TestXAdESWithTimeStamp {

    private static final String CERT_PATH = "EIDAS_CERTIFICADO_PRUEBAS___99999999R__1234.p12"; //$NON-NLS-1$
    private static final String CERT_PASS = "1234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "eidas_certificado_pruebas___99999999r"; //$NON-NLS-1$

	private static final String CATCERT_POLICY = "0.4.0.2023.1.1"; //$NON-NLS-1$
	private static final String CATCERT_TSP = "http://psis.catcert.net/psis/catcert/tsp"; //$NON-NLS-1$
	private static final Boolean CATCERT_REQUIRECERT = Boolean.TRUE;

    private static final Properties EXTRAPARAMS = new Properties();
    static {
	    EXTRAPARAMS.put("tsaURL", CATCERT_TSP); //$NON-NLS-1$
	    EXTRAPARAMS.put("tsaPolicy", CATCERT_POLICY); //$NON-NLS-1$
	    EXTRAPARAMS.put("tsaRequireCert", CATCERT_REQUIRECERT); //$NON-NLS-1$
	    EXTRAPARAMS.put("tsaHashAlgorithm", "SHA-512"); //$NON-NLS-1$ //$NON-NLS-2$
	    EXTRAPARAMS.put("tsType", TsaParams.TS_SIGN_DOC); //$NON-NLS-1$
    }

    /** Pruebas de XAdES-T.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Ignore
	@Test
    public void testXAdEST() throws Exception {

    	final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
    	try (final InputStream is = ClassLoader.getSystemResourceAsStream(CERT_PATH)) {
    		ks.load(is, CERT_PASS.toCharArray());
    	}

        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(
    		CERT_ALIAS,
    		new KeyStore.PasswordProtection(CERT_PASS.toCharArray())
		);

    	final byte[] data;
    	try (final InputStream is = ClassLoader.getSystemResourceAsStream("xml_with_ids.xml")) { //$NON-NLS-1$
    		data = AOUtil.getDataFromInputStream(is);
    	}

    	final Properties extraParams = new Properties(EXTRAPARAMS);

    	final AOXAdESSigner signer = new AOXAdESSigner();
    	final byte[] signature = signer.sign(
    			data,
    			AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
    			pke.getPrivateKey(),
    			pke.getCertificateChain(),
    			extraParams);


    	final File tempFile = File.createTempFile("XAdES-T_", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
    	try (FileOutputStream fos = new FileOutputStream(tempFile);) {
    		fos.write(signature);
    	}

    	System.out.println("La firma XAdES-T se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
    }

    /** Pruebas de XAdES-T-Level.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Ignore
	@Test
    public void testXAdESTLevel() throws Exception {

    	final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
    	try (final InputStream is = ClassLoader.getSystemResourceAsStream(CERT_PATH)) {
    		ks.load(is, CERT_PASS.toCharArray());
    	}

        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(
    		CERT_ALIAS,
    		new KeyStore.PasswordProtection(CERT_PASS.toCharArray())
		);

    	final byte[] data;
    	try (final InputStream is = ClassLoader.getSystemResourceAsStream("xml_with_ids.xml")) { //$NON-NLS-1$
    		data = AOUtil.getDataFromInputStream(is);
    	}

    	final Properties extraParams = new Properties(EXTRAPARAMS);
    	extraParams.setProperty("profile", "B-B-Level"); //$NON-NLS-1$ //$NON-NLS-2$

    	final AOXAdESSigner signer = new AOXAdESSigner();
    	final byte[] signature = signer.sign(
    			data,
    			AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
    			pke.getPrivateKey(),
    			pke.getCertificateChain(),
    			extraParams);


    	final File tempFile = File.createTempFile("XAdES-T-Level_", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
    	try (FileOutputStream fos = new FileOutputStream(tempFile);) {
    		fos.write(signature);
    	}

    	System.out.println("La firma XAdES-T-Level se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
    }
}
