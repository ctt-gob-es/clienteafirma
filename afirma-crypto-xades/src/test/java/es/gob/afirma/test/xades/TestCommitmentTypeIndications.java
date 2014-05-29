package es.gob.afirma.test.xades;

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
import es.gob.afirma.signers.xades.AOXAdESSigner;

/** Pruebas de XAdES Enveloped con inserci&oacute;n de CommitmentTypeIndications.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestCommitmentTypeIndications {

    private static final String CERT_PATH = "ANF_con cadena_certificacion.jks"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

    private static final Properties p3 = new Properties();
    static {
	    p3.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED); //$NON-NLS-1$
	    p3.setProperty("commitmentTypeIndications", "1"); //$NON-NLS-1$ //$NON-NLS-2$
	    p3.setProperty("commitmentTypeIndication0Identifier", "1.2.840.113549.1.9.16.6.1"); //$NON-NLS-1$ //$NON-NLS-2$
	    p3.setProperty("commitmentTypeIndication0Description", "Prueba"); //$NON-NLS-1$ //$NON-NLS-2$
	    p3.setProperty("commitmentTypeIndication0DocumentationReferences", "http://es.atos.net/es-es/home.html|http://atos.net/en-us/home.html"); //$NON-NLS-1$ //$NON-NLS-2$
	    p3.setProperty("commitmentTypeIndication0CommitmentTypeQualifiers", "Q1|Q2|Q3"); //$NON-NLS-1$ //$NON-NLS-2$
	    p3.setProperty("signerClaimedRoles", "Papa de Roma|Cardenal emerito de Pamplona"); //$NON-NLS-1$ //$NON-NLS-2$
    }

	/** Prueba de XAdES Enveloped con inserci&oacute;n de CommitmentTypeIndication.
	 * @throws Exception */
	@SuppressWarnings("static-method")
	@Test
	public void testCommitmentTypeIndication() throws Exception {

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;
        final KeyStore ks = KeyStore.getInstance("JKS"); //$NON-NLS-1$

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
        final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
        fos.write(result);
        fos.flush(); fos.close();
        System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$

	}

}
