package es.gob.afirma.test.pades;

import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.beans.AOSimpleSignInfo;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.pades.AOPDFSigner;

/**
 * Pruebas del m&oacute;dulo PAdES de Afirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 *
 */
public class TestPAdES {
    
    private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$
    
    private static final Properties[] PADES_MODES;
    
    private static final String[] TEST_FILES = { "TEST_PDF.pdf" }; //$NON-NLS-1$
    
    static {
        final Properties p1 = new Properties();
        p1.setProperty("format", AOSignConstants.SIGN_FORMAT_PDF); //$NON-NLS-1$
        p1.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
        p1.setProperty("signReason", "test"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("signatureProductionCity", "madrid"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("signerContact", "sink@usa.net"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyQualifier", "2.16.724.1.3.1.1.2"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyIdentifier", "http://google.com/"); //$NON-NLS-1$ //$NON-NLS-2$
        

        final Properties p2 = new Properties();
        p2.setProperty("format", AOSignConstants.SIGN_FORMAT_PDF); //$NON-NLS-1$
        p2.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$

        PADES_MODES = new Properties[] {
                p1, p2
        };
    }

    /** Algoritmos de firma a probar. */
    private final static String[] ALGOS = new String[] {
            AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA, 
            AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA,
    };
    
    /**
     * Prueba de firma convencional.
     * @throws Exception en cualquier error
     */
    @Test
    public void testSignature() throws Exception {
        
        Assert.assertEquals("file.signed.pdf", AOPDFSigner.getSignedName("file.pdf")); //$NON-NLS-1$ //$NON-NLS-2$
        
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;
        final X509Certificate cert;

        KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
        cert = (X509Certificate) ks.getCertificate(CERT_ALIAS);
        
        AOSigner signer = new AOPDFSigner();
        
        String prueba;
        
        for (final Properties extraParams : PADES_MODES) {
            for (final String algo : ALGOS) {
                for (final String file : TEST_FILES) {
                    
                    final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(file)); 
                    
                    Assert.assertTrue("No se ha reconocido como un PDF", signer.isValidDataFile(testPdf)); //$NON-NLS-1$
                
                    prueba = "Firma PAdES en modo '" +  //$NON-NLS-1$
                    extraParams.getProperty("mode") +  //$NON-NLS-1$
                    "' con el algoritmo ': " + //$NON-NLS-1$
                    algo +
                    "' y el fichero '" +  //$NON-NLS-1$
                    file +
                    "'"; //$NON-NLS-1$
                    
                    System.out.println(prueba);
                    
                    byte[] result = signer.sign(testPdf, algo, pke, extraParams);
                    
                    Assert.assertNotNull(prueba, result);
                    Assert.assertTrue(signer.isSign(result));
                    
                    AOTreeModel tree = signer.getSignersStructure(result, false);
                    Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject()); //$NON-NLS-1$
                    Assert.assertEquals("ANF Usuario Activo", ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject()); //$NON-NLS-1$
                    
                    tree = signer.getSignersStructure(result, true);
                    Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject()); //$NON-NLS-1$
                    AOSimpleSignInfo simpleSignInfo = (AOSimpleSignInfo) ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject();
                    
                    Assert.assertNotNull(simpleSignInfo.getSigningTime());
                    Assert.assertEquals(cert, simpleSignInfo.getCerts()[0]);    
                    
                    Assert.assertEquals("application/pdf", signer.getDataMimeType(result)); //$NON-NLS-1$
                    
                }
                
                
            }
        }
    }
}
