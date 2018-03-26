package es.gob.afirma.signers.xades;

import java.io.File;
import java.io.IOException;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableEntryException;
import java.security.cert.CertificateException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.CounterSignTarget;

/**
 * Test para la comprobaci&oacute;n de las contrafirmas sobre firmas generadas por
 * la IGAE.
 * @author Carlos Gamuci
 */
public class TestContrafirmaIgae {

	private static final String[] SIGNATURE_FILES = new String[] {
		"firmaIgae.xsig.xml", //$NON-NLS-1$
		"firmaAfirma.xsig" //$NON-NLS-1$
	};

    private static final String CERT_PATH = "CATCERT CIUTADANIA PF CPIXSA-2.p12"; //$NON-NLS-1$
    private static final String CERT_PASS = "1111"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "persona f\u00EDsica de la pe\u00E7a de proves"; //$NON-NLS-1$



    /**
     * Prueba a contrafirmar una firma generada por la IGAE.
     * @throws IOException Cuando ocurre un error en la carga o guardado de los datos.
     * @throws KeyStoreException Cuando ocurre un error en el acceso al almac&eacute;n de certificados.
     * @throws AOException Cuando ocurre un error en el proceso de contrafirma.
     * @throws CertificateException Cuando ocurre un error en
     * @throws NoSuchAlgorithmException Cuando no se encuentra el algoritmo de firma.
     * @throws UnrecoverableEntryException Cuando no se puede recuperar la clave de firma.
     */
    @SuppressWarnings("static-method")
	@Test
    public void testContrafirmaXAdESDeFirmasDeIgae() throws IOException, KeyStoreException, AOException, NoSuchAlgorithmException, CertificateException, UnrecoverableEntryException {

    	Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
    	final PrivateKeyEntry pke;

    	final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
    	ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
    	pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

    	final AOXAdESSigner signer = new AOXAdESSigner();

    	final Properties p = new Properties();
    	final String algorithm = "SHA1withRSA"; //$NON-NLS-1$

    	for (final String signFile : SIGNATURE_FILES) {

        	System.out.println("\nContrafirma XAdES Detached con el algoritmo '" + //$NON-NLS-1$
        			algorithm + "' sobre la firma de la IGAE '" + signFile + "'"); //$NON-NLS-1$ //$NON-NLS-2$

    		final byte[] sign = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(signFile));

    		final byte[] result = signer.countersign(
    				sign,
    				algorithm,
    				CounterSignTarget.TREE,
    				null,
    				pke.getPrivateKey(),
    				pke.getCertificateChain(),
    				p);

    		final File f = File.createTempFile("Contrafirma-" + signFile, ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
    		try (
				final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
			) {
				fos.write(result);
				fos.flush();
			}
    		System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$

    		Assert.assertTrue("El resultado no se reconoce como firma", signer.isSign(result)); //$NON-NLS-1$
    	}
    }
}
