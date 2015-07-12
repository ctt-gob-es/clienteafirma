package es.gob.afirma.signers.xades;

import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;

import org.junit.Test;

import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signers.xades.asic.AOXAdESASiCSSigner;

/** Pruebas de XAdES ASiC.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestXAdESASiC {

    private static final String CERT_PATH = "PFActivoFirSHA256.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "fisico activo prueba"; //$NON-NLS-1$

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
        		"HOLA".getBytes(), //$NON-NLS-1$
        		"SHA512withRSA", //$NON-NLS-1$
        		pke.getPrivateKey(),
        		pke.getCertificateChain(),
        		null
		);
    }

}
