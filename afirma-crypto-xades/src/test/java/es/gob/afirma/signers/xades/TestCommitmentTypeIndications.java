package es.gob.afirma.signers.xades;

import java.io.File;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;

/** Pruebas de XAdES Enveloped con inserci&oacute;n de CommitmentTypeIndications.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestCommitmentTypeIndications {

    private static final String CERT_PATH = "Carac_raros.p12"; //$NON-NLS-1$
    private static final String CERT_PASS = "cacertica08"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "1"; //$NON-NLS-1$

    private static final Properties p3 = new Properties();
    static {
	    p3.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED); //$NON-NLS-1$
	    p3.setProperty("commitmentTypeIndications", "2"); //$NON-NLS-1$ //$NON-NLS-2$
	    p3.setProperty("commitmentTypeIndication0Identifier", "2"); //$NON-NLS-1$ //$NON-NLS-2$
	    p3.setProperty("commitmentTypeIndication0Description", "Prueba"); //$NON-NLS-1$ //$NON-NLS-2$
	    p3.setProperty("commitmentTypeIndication0DocumentationReferences", "http://es.atos.net/es-es/home.html|http://atos.net/en-us/home.html"); //$NON-NLS-1$ //$NON-NLS-2$
	    p3.setProperty("commitmentTypeIndication0CommitmentTypeQualifiers", "Q1|Q2|Q3"); //$NON-NLS-1$ //$NON-NLS-2$
	    p3.setProperty("commitmentTypeIndication1Identifier", "3"); //$NON-NLS-1$ //$NON-NLS-2$
	    p3.setProperty("commitmentTypeIndication1Description", "Cafre"); //$NON-NLS-1$ //$NON-NLS-2$
	    p3.setProperty("commitmentTypeIndication1DocumentationReferences", "http://google.com"); //$NON-NLS-1$ //$NON-NLS-2$
	    p3.setProperty("commitmentTypeIndication1CommitmentTypeQualifiers", "U1|U2|U3"); //$NON-NLS-1$ //$NON-NLS-2$
	    p3.setProperty("signerClaimedRoles", "Papa de Roma|Cardenal emerito de Pamplona"); //$NON-NLS-1$ //$NON-NLS-2$
	    p3.setProperty("addKeyInfoKeyValue", Boolean.TRUE.toString()); //$NON-NLS-1$
	    p3.setProperty("addKeyInfoKeyName", Boolean.TRUE.toString()); //$NON-NLS-1$
    }

	/** Prueba de XAdES Enveloped con inserci&oacute;n de CommitmentTypeIndication.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testCommitmentTypeIndication() throws Exception {

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;
        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$

        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
        final AOSigner signer = new AOXAdESSigner();

        final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("xpathnodenveloped.xml")); //$NON-NLS-1$

        final byte[] result = signer.sign(
    		data,
    		"SHA512withRSA", //$NON-NLS-1$
    		pke.getPrivateKey(),
    		pke.getCertificateChain(),
    		p3
		);

        final File f = File.createTempFile("xpathCommitmentTypeIndication-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
        try (
			final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
		) {
			fos.write(result);
			fos.flush();
		}
        System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$

	}



}
