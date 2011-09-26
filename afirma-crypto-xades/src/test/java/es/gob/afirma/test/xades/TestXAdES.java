package es.gob.afirma.test.xades;

import java.io.File;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.crypto.dsig.DigestMethod;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.beans.AOSimpleSignInfo;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
//import es.gob.afirma.platform.ws.TestSignVerifier;
import es.gob.afirma.signers.xades.AOXAdESSigner;



/**
 * Pruebas del m&oacute;dulo CAdES de Afirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 *
 */
public final class TestXAdES {
    
    private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

//    private static final String CERT_PATH2 = "CATCERT GENCAT SAFP PF Identidad y Firma Reconocida de Clase 1 Caducado.pfx"; //$NON-NLS-1$
//    private static final String CERT_PASS2 = "1234"; //$NON-NLS-1$
//    private static final String CERT_ALIAS2 = "{71e526c4-0f27-4f32-8be0-90df52dcbc53}"; //$NON-NLS-1$
//    
//    private static final String CERT_PATH3 = "CAMERFIRMA_PF_SW_Clave_usuario_Activo.p12"; //$NON-NLS-1$
//    private static final String CERT_PASS3 = "1111"; //$NON-NLS-1$
//    private static final String CERT_ALIAS3 = "1"; //$NON-NLS-1$
    
    private static final Properties[] XADES_MODES;
    
    static {
        final Properties p1 = new Properties();
        p1.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_DETACHED); //$NON-NLS-1$
        p1.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
        p1.setProperty("policyIdentifier", "urn:oid:2.16.724.1.3.1.1.2.1.8"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyIdentifierHash", "ESTEESUNHASH=");  //$NON-NLS-1$//$NON-NLS-2$
        p1.setProperty("policyIdentifierHashAlgorithm", DigestMethod.SHA1);         //$NON-NLS-1$
        p1.setProperty("policyDescription", "Politica de firma electronica para las Administraciones Publicas en Espana"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyQualifier", "http://blogs.adobe.com/security/91014620_eusig_wp_ue.pdf"); //$NON-NLS-1$ //$NON-NLS-2$

        final Properties p2 = new Properties();
        p2.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_DETACHED); //$NON-NLS-1$
        p2.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$
//        p2.setProperty("policyIdentifier", "http://blogs.adobe.com/security/91014620_eusig_wp_ue.pdf");
//        p2.setProperty("policyDescription", "DESCRIPCION");
//        p2.setProperty("policyQualifier", "urn:oid:2.16.724.1.3.2.1.1.8");
        
        
        final Properties p3 = new Properties();
        p3.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED); //$NON-NLS-1$
        p3.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
//        p3.setProperty("policyIdentifier", "http://blogs.adobe.com/security/91014620_eusig_wp_ue.pdf");
//        p3.setProperty("policyDescription", "DESCRIPCION");
//        p3.setProperty("policyQualifier", "urn:oid:2.16.724.1.3.2.1.1.8");


        final Properties p4 = new Properties();
        p4.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING); //$NON-NLS-1$
        p4.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
//        p4.setProperty("policyIdentifier", "http://blogs.adobe.com/security/91014620_eusig_wp_ue.pdf");
//        p4.setProperty("policyDescription", "DESCRIPCION");
//        p4.setProperty("policyQualifier", "urn:oid:2.16.724.1.3.2.1.1.8");

        final Properties p5 = new Properties();
        p5.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING); //$NON-NLS-1$
        p5.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$
//        p5.setProperty("policyIdentifier", "http://blogs.adobe.com/security/91014620_eusig_wp_ue.pdf");
//        p5.setProperty("policyDescription", "DESCRIPCION");
//        p5.setProperty("policyQualifier", "urn:oid:2.16.724.1.3.2.1.1.8");
        
        XADES_MODES = new Properties[] {
                p1, p2, p3, p4, p5
        };
    }
    
    /** Algoritmos de firma a probar. */
    private final static String[] ALGOS = new String[] {
            AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA, 
            AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA
    };
    
    private static final String[] TEST_FILES_SIGN = new String[] {
            "ANF_PF_Activo.pfx", //$NON-NLS-1$
            "sample-class-attributes.xml", //$NON-NLS-1$
            "sample-embedded-style.xml", //$NON-NLS-1$
            "sample-encoding-UTF-8.xml", //$NON-NLS-1$
            "sample-facturae.xml", //$NON-NLS-1$
            "sample-internal-dtd.xml", //$NON-NLS-1$
            "sample-namespace-encoding-us-ascii.xml" //$NON-NLS-1$
    };
    
    /**
     * Prueba de firma convencional.
     * @throws Exception en cualquier error
     */
    @Test
    public void testSignature() throws Exception {
        
//        TestSignVerifier verifier = null;
//        try {
//            verifier = new TestSignVerifier();
//        } 
//        catch (Exception e) {
//            System.out.println("No se ha podido inicializar el validador de firmas, no se validaran como parte de las pruebas: " + e); //$NON-NLS-1$
//        }

        
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;
        final X509Certificate cert;

        KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
        cert = (X509Certificate) ks.getCertificate(CERT_ALIAS);

        AOSigner signer = new AOXAdESSigner();
        
        String prueba;
        
        for (final Properties extraParams : XADES_MODES) {
            for (final String algo : ALGOS) {
                for (final String filename : TEST_FILES_SIGN) {
                
                    prueba = "Firma XAdES en modo '" +  //$NON-NLS-1$
                    extraParams.getProperty("mode") +  //$NON-NLS-1$
                    "' con el algoritmo ': " + //$NON-NLS-1$
                    algo +
                    "' y el fichero '" + //$NON-NLS-1$
                    filename + "'"; //$NON-NLS-1$
                    
                    System.out.println(prueba);
                    
                    final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(filename));
                    
                    final byte[] result = signer.sign(data, algo, pke, extraParams);
                        
                    File f = File.createTempFile(algo+extraParams.getProperty("mode") + "-" + filename.replace(".xml", "") + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
                    java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
                    fos.write(result);
                    try { fos.flush(); fos.close(); } catch (Exception e) { 
                        // Ignoramos los errores
                    }
                    System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
    
//                    // Enviamos a validar a AFirma
//                    if (verifier != null) {
//                        Assert.assertTrue("Fallo al validar " + filename, verifier.verifyXML(result)); //$NON-NLS-1$
//                    }

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
                                        
                }
            }
        }

    }
    
        
}
