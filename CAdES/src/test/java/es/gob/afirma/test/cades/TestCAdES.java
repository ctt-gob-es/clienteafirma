package es.gob.afirma.test.cades;

import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.beans.AOSimpleSignInfo;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.cades.AOCAdESSigner;


/**
 * Pruebas del m&oacute;dulo CAdES de Afirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 *
 */
public final class TestCAdES {
    
    private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

    private static final String CERT_PATH2 = "CATCERT GENCAT SAFP PF Identidad y Firma Reconocida de Clase 1 Caducado.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS2 = "1234"; //$NON-NLS-1$
    private static final String CERT_ALIAS2 = "{71e526c4-0f27-4f32-8be0-90df52dcbc53}"; //$NON-NLS-1$
    
    private static final String CERT_PATH3 = "CAMERFIRMA_PF_SW_Clave_usuario_Activo.p12"; //$NON-NLS-1$
    private static final String CERT_PASS3 = "1111"; //$NON-NLS-1$
    private static final String CERT_ALIAS3 = "1"; //$NON-NLS-1$
    
    private static final byte[] DATA = "usfusgfusgduifgsifiusdgfigsdiufgsid".getBytes(); //$NON-NLS-1$
    private static final Properties[] CADES_MODES;
    
    static {
        final Properties p1 = new Properties();
        p1.setProperty("format", AOSignConstants.SIGN_FORMAT_CADES); //$NON-NLS-1$
        p1.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$

        final Properties p2 = new Properties();
        p2.setProperty("format", AOSignConstants.SIGN_FORMAT_CADES); //$NON-NLS-1$
        p2.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$

        CADES_MODES = new Properties[] {
                p1, p2
        };
    }
    
    /** Algoritmos de firma a probar. */
    private final static String[] ALGOS = new String[] {
            AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA, 
            AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_MD2WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_MD5WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA
    };
    
    /**
     * Prueba de firma convencional.
     */
    @Test
    public void testSignature() {
        
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;
        final X509Certificate cert;
        try {
            KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
            ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
            pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
            cert = (X509Certificate) ks.getCertificate(CERT_ALIAS);
        }
        catch(Exception e) {
            Assert.fail("No se ha podido obtener las claves de firma: " + e); //$NON-NLS-1$
            return;
        }
        AOSigner signer = new AOCAdESSigner();
        
        String prueba;
        
        for (final Properties extraParams : CADES_MODES) {
            for (final String algo : ALGOS) {
                
                prueba = "Firma CAdES en modo '" +  //$NON-NLS-1$
                extraParams.getProperty("mode") +  //$NON-NLS-1$
                "' con el algoritmo ': " + //$NON-NLS-1$
                algo +
                "'"; //$NON-NLS-1$
                
                byte[] result = null;
                try {
                    result = signer.sign(DATA, algo, pke, extraParams);
                }
                catch(final Exception e) {
                    Assert.fail("Error al generar " + prueba + //$NON-NLS-1$
                        ": " + //$NON-NLS-1$
                        e);
                }
                Assert.assertNotNull(prueba, result);
                Assert.assertTrue(signer.isSign(result));
                Assert.assertTrue(AOCAdESSigner.isCADESValid(result, AOSignConstants.CMS_CONTENTTYPE_SIGNEDDATA));
                
                AOTreeModel tree = signer.getSignersStructure(result, false);
                Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject());
                Assert.assertEquals("ANF Usuario Activo", ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject());
                
                tree = signer.getSignersStructure(result, true);
                Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject());
                AOSimpleSignInfo simpleSignInfo = (AOSimpleSignInfo) ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject();
                
//                Assert.assertEquals("CAdES", simpleSignInfo.getSignFormat());
//                Assert.assertEquals(algo, simpleSignInfo.getSignAlgorithm());
                Assert.assertNotNull(simpleSignInfo.getSigningTime());
                Assert.assertEquals(cert, simpleSignInfo.getCerts()[0]);    
                
                
                //System.out.println(prueba + ": OK"); //$NON-NLS-1$
            }
        }
        
        try {
            signer.sign(DATA, "SHA1withRSA", pke, null); //$NON-NLS-1$
        }
        catch(final Exception e) {
            Assert.fail("Error al generar la firma: " + e); //$NON-NLS-1$
        }
    }
    
    /**
     * Prueba de cofirma.
     */
    @Test
    public void testCoSignature() {
        
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke1 = loadKeyEntry(CERT_PATH, CERT_ALIAS, CERT_PASS);
        final PrivateKeyEntry pke2 = loadKeyEntry(CERT_PATH2, CERT_ALIAS2, CERT_PASS2);
        final PrivateKeyEntry pke3 = loadKeyEntry(CERT_PATH3, CERT_ALIAS3, CERT_PASS3);
        
        AOSigner signer = new AOCAdESSigner();
        
        String prueba;
        
        for (final Properties extraParams : CADES_MODES) {
            for (final String algo : ALGOS) {
                
                prueba = "Firma CAdES en modo '" +  //$NON-NLS-1$
                extraParams.getProperty("mode") +  //$NON-NLS-1$
                "' con el algoritmo ': " + //$NON-NLS-1$
                algo +
                "'"; //$NON-NLS-1$
                
                // Firma simple
                byte[] sign1 = sign(signer, DATA, algo, pke1, extraParams);
                
                // Cofirma sin indicar los datos
                byte[] sign2 = cosign(signer, sign1, algo, pke2, extraParams);
                
                Assert.assertNotNull(prueba, sign2);
                Assert.assertTrue(signer.isSign(sign2));
                Assert.assertTrue(AOCAdESSigner.isCADESValid(sign2, AOSignConstants.CMS_CONTENTTYPE_SIGNEDDATA));
                
                AOTreeModel tree = signer.getSignersStructure(sign2, false);
                Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject());
                Assert.assertEquals("ANF Usuario Activo", ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject());
                
                tree = signer.getSignersStructure(sign2, true);
                Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject());
                AOSimpleSignInfo simpleSignInfo = (AOSimpleSignInfo) ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject();
                
//                Assert.assertEquals("CAdES", simpleSignInfo.getSignFormat());
//                Assert.assertEquals(algo, simpleSignInfo.getSignAlgorithm());
                Assert.assertNotNull(simpleSignInfo.getSigningTime());
                
//                System.out.println("Certificado firmante:\n" + pke2.getCertificate());
//                System.out.println("-------------");
//                System.out.println("Certificado extraido:\n" + simpleSignInfo.getCerts()[0]);
                
                //Assert.assertEquals(pke2.getCertificate(), simpleSignInfo.getCerts()[0]);    
                
                
                // Cofirma indicando los datos
                byte[] sign3 = cosign(signer, DATA, sign2, algo, pke3, extraParams);
                
                
                Assert.assertNotNull(prueba, sign3);
                Assert.assertTrue(signer.isSign(sign3));
                Assert.assertTrue(AOCAdESSigner.isCADESValid(sign3, AOSignConstants.CMS_CONTENTTYPE_SIGNEDDATA));
                
                tree = signer.getSignersStructure(sign3, false);
                Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject());
                Assert.assertEquals("ANF Usuario Activo", ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject());
                
                tree = signer.getSignersStructure(sign3, true);
                Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject());
                simpleSignInfo = (AOSimpleSignInfo) ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject();
                
//                Assert.assertEquals("CAdES", simpleSignInfo.getSignFormat());
//                Assert.assertEquals(algo, simpleSignInfo.getSignAlgorithm());
                Assert.assertNotNull(simpleSignInfo.getSigningTime());
                //Assert.assertEquals(pke3.getCertificate(), simpleSignInfo.getCerts()[0]);    
                
                //System.out.println(prueba + ": OK"); //$NON-NLS-1$
            }
        }
        
        try {
            signer.sign(DATA, "SHA1withRSA", pke1, null); //$NON-NLS-1$
        }
        catch(final Exception e) {
            Assert.fail("Error al generar la firma: " + e); //$NON-NLS-1$
        }
    }

    private PrivateKeyEntry loadKeyEntry(String pkcs12File, String alias, String password) {
        final PrivateKeyEntry pke;
        try {
            KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
            ks.load(ClassLoader.getSystemResourceAsStream(pkcs12File), password.toCharArray());
            pke = (PrivateKeyEntry) ks.getEntry(alias, new KeyStore.PasswordProtection(password.toCharArray()));
        }
        catch(Exception e) {
            Assert.fail("No se ha podido obtener las claves de firma del certificado '" + pkcs12File + "': " + e); //$NON-NLS-1$
            return null;
        }
        
        return pke;
    }
    
    private byte[] sign(AOSigner signer, byte[] data, String algorithm, PrivateKeyEntry pke, Properties params) {
        try {
            return signer.sign(data, algorithm, pke, params);
        }
        catch(final Exception e) {
            Assert.fail("Error al firmar:" + e);
            return null;
        }
    }
    
    private byte[] cosign(AOSigner signer, byte[] sign, String algorithm, PrivateKeyEntry pke, Properties params) {
        try {
            return signer.cosign(sign, algorithm, pke, params);
        }
        catch(final Exception e) {
            Assert.fail("Error al cofirmar con datos:" + e);
            return null;
        }
    }
    
    private byte[] cosign(AOSigner signer, byte[] data, byte[] sign, String algorithm, PrivateKeyEntry pke, Properties params) {
        try {
            return signer.cosign(data, sign, algorithm, pke, params);
        }
        catch(final Exception e) {
            Assert.fail("Error al cofirmar con datos:" + e);
            return null;
        }
    }
}
