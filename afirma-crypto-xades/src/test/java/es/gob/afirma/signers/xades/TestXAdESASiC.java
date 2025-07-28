package es.gob.afirma.signers.xades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;

import org.junit.Test;

import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signers.xades.asic.AOXAdESASiCSSigner;

/** Pruebas de XAdES ASiC.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestXAdESASiC {

    private static final String CERT_PATH = "EIDAS_CERTIFICADO_PRUEBAS___99999999R__1234.p12"; //$NON-NLS-1$
    private static final String CERT_PASS = "1234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "eidas_certificado_pruebas___99999999r"; //$NON-NLS-1$

    /** Prueba de firma simple XAdES ASiC-S.
     * @throws Exception En culaquier error. */
    @SuppressWarnings("static-method")
    @Test
	public void TestSimpleASiCS() throws Exception {

        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final AOSigner signer = new AOXAdESASiCSSigner();

        final byte[] result = signer.sign(
        		"<?xml version=\"1.0\" encoding=\"UTF-8\" ?><NODO>Valor</NODO>".getBytes(StandardCharsets.UTF_8), //$NON-NLS-1$
        		"SHA512withRSA", //$NON-NLS-1$
        		pke.getPrivateKey(),
        		pke.getCertificateChain(),
        		null
		);

        final File outputFile = File.createTempFile("ASIC-XAdES-",  ".zip"); //$NON-NLS-1$ //$NON-NLS-2$
        try (
    		final OutputStream os = new FileOutputStream(outputFile);
		) {
        	os.write(result);
        }

        System.out.println("Firma XAdES ASiC-S almacenada en: " + outputFile.getAbsolutePath()); //$NON-NLS-1$
    }

    /** Prueba de firma simple XAdES ASiC-S.
     * @throws Exception En culaquier error. */
    @SuppressWarnings("static-method")
    @Test
	public void TestCosignASiCS() throws Exception {

        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final AOSigner signer = new AOXAdESASiCSSigner();

        final byte[] signature = signer.sign(
        		"<?xml version=\"1.0\" encoding=\"UTF-8\" ?><NODO>Valor</NODO>".getBytes(), //$NON-NLS-1$
        		"SHA512withRSA", //$NON-NLS-1$
        		pke.getPrivateKey(),
        		pke.getCertificateChain(),
        		null
		);


        final byte[] coSignature = signer.cosign(
        		signature,
        		"SHA512withRSA", //$NON-NLS-1$
        		pke.getPrivateKey(),
        		pke.getCertificateChain(),
        		null
		);

        final File outputFile = File.createTempFile("ASIC-XAdES-cosign-",  ".zip"); //$NON-NLS-1$ //$NON-NLS-2$
        try (
    		final OutputStream os = new FileOutputStream(outputFile);
		) {
        	os.write(coSignature);
        }

        System.out.println("Cofirma XAdES ASiC-S almacenada en: " + outputFile.getAbsolutePath()); //$NON-NLS-1$
    }

}
