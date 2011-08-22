package es.gob.afirma.test.cades;

import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signers.cades.AOCAdESSigner;


/**
 * Pruebas del m&oacute;dulo CAdES de Afirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 *
 */
public final class TestCAdES {
    
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
        try {
            KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
            ks.load(ClassLoader.getSystemResourceAsStream("ANF_PF_Activo.pfx"), "12341234".toCharArray()); //$NON-NLS-1$ //$NON-NLS-2$
            pke = (PrivateKeyEntry) ks.getEntry("anf usuario activo", new KeyStore.PasswordProtection("12341234".toCharArray())); //$NON-NLS-1$ //$NON-NLS-2$
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
    

}
