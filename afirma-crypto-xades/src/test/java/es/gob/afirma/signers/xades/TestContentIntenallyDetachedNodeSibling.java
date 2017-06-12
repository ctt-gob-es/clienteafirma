/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.signers.xades;

import java.io.File;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;

/** Pruebas del firma <i>Internally Detached</i> de nodo.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestContentIntenallyDetachedNodeSibling {

    private static final String CERT_PATH = "PFActivoFirSHA256.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "fisico activo prueba"; //$NON-NLS-1$

    private static final Properties[] XADES_MODES;

    static {
        final Properties p1 = new Properties();
        p1.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_DETACHED); //$NON-NLS-1$
        p1.setProperty("nodeToSign", "IDN"); //$NON-NLS-1$ //$NON-NLS-2$

        final Properties p2 = new Properties();
        p2.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_DETACHED); //$NON-NLS-1$
        p2.setProperty("nodeToSign", "ROOT"); //$NON-NLS-1$ //$NON-NLS-2$

        final Properties p3 = new Properties();
        p3.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_DETACHED); //$NON-NLS-1$

        final Properties p4 = new Properties();
        p4.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED); //$NON-NLS-1$

        final Properties p5 = new Properties();

        XADES_MODES = new Properties[] { p1, p2, p3, p4, p5 };
    }

    /** Algoritmos de firma a probar. */
    private final static String[] ALGOS = new String[] { AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA };

    // IMPORTANTE: Poner extension ".xml" a los ficheros de prueba con contenido XML
    private static final String[] TEST_FILES_DATA = new String[] {
        "internally_detached_node_sibling.xml", //$NON-NLS-1$
    };

    /** Prueba de firma <i>Internally Detached</i> de nodo.
     * @throws Exception en cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testSignature() throws Exception {

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final AOSigner signer = new AOXAdESSigner();

        String prueba;

        for (final Properties extraParams : XADES_MODES) {
            for (final String algo : ALGOS) {
                for (final String filename : TEST_FILES_DATA) {

                    prueba = "Firma XAdES en modo '" +  //$NON-NLS-1$
                    extraParams.getProperty("mode") +  //$NON-NLS-1$
                    ", formato '" + //$NON-NLS-1$
                    extraParams.getProperty("format") +  //$NON-NLS-1$
                    "' con el algoritmo ': " + //$NON-NLS-1$
                    algo +
                    "' y el fichero '" + //$NON-NLS-1$
                    filename + "'"; //$NON-NLS-1$

                    final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(filename));

                    final byte[] result = signer.sign(
                		data,
                		algo,
                		pke.getPrivateKey(),
                		pke.getCertificateChain(),
                		extraParams
            		);

                    final File f = File.createTempFile(algo + "-" + extraParams.getProperty("mode") + "-" + filename.replace(".xml", "") + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
                    try (
            			final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
            		) {
            			fos.write(result);
            			fos.flush();
            		}
                    System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$

                    Assert.assertNotNull(prueba, result);
                    Assert.assertTrue(signer.isSign(result));
                }
            }
        }

    }
}
