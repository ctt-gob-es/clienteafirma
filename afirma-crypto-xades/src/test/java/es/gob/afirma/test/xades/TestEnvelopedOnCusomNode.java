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

/** Pruebas de XAdES Enveloped con inserci&oacute;n de firma en nodo espec&iacute;fico.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestEnvelopedOnCusomNode {

    private static final String CERT_PATH = "ANF_con cadena_certificacion.jks"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

    private static final Properties p3 = new Properties();
    static {
	    p3.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED); //$NON-NLS-1$
	    p3.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
	    p3.setProperty("includeOnlySignningCertificate", "true"); //$NON-NLS-1$ //$NON-NLS-2$
	    p3.setProperty("insertEnvelopedSignatureOnNodeByXPath", "/bookstore/book[1]/title"); //$NON-NLS-1$ //$NON-NLS-2$
	    p3.setProperty("useManifest", "true"); //$NON-NLS-1$ //$NON-NLS-2$
    }

    private static final Properties p2 = new Properties();
    static {
	    p2.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED); //$NON-NLS-1$
	    p2.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
	    p2.setProperty("includeOnlySignningCertificate", "true"); //$NON-NLS-1$ //$NON-NLS-2$
	    p2.setProperty("nodeToSign", "2"); //$NON-NLS-1$ //$NON-NLS-2$
	    p2.setProperty("useManifest", "true"); //$NON-NLS-1$ //$NON-NLS-2$
    }

	/** Prueba de XAdES Enveloped con inserci&oacute;n de firma en el mismo nodo espec&iacute;fico
	 * que se firma.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void TestInsertSignatureOnSignedNode() throws Exception {
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
    		p2
		);

        final File f = File.createTempFile("xpathnodenveloped-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
        final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
        fos.write(result);
        fos.flush(); fos.close();
        System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
	}

	/** Prueba de XAdES Enveloped con inserci&oacute;n de firma en nodo espec&iacute;fico.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void TestInsertSignatureOnCustomNode() throws Exception {

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

        final File f = File.createTempFile("xpathnodenveloped-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
        final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
        fos.write(result);
        fos.flush(); fos.close();
        System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$

	}

}
