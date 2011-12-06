package es.gob.afirma.test.keystores;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Test;

import junit.framework.Assert;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.main.common.AOKeyStore;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerFactory;

/** Pruebas espec&iacute;ficas para los almacenes de Mac OS X.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestMacKeyChain {
    
    /** Prueba de carga y uso de un <i>KayChain</i> en fichero suelto.
     * @throws Exception */
    @Test
    public void testStandaloneKeyChain() throws Exception {
        if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
            return;
        }
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        
        // Copiamos el KeyChain a un fichero temporal
        final File kc = File.createTempFile("test", ".keychain"); //$NON-NLS-1$ //$NON-NLS-2$
        kc.deleteOnExit();
        final OutputStream os = new FileOutputStream(kc);
        os.write(AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("test.keychain"))); //$NON-NLS-1$
        os.flush();
        os.close();
        
        final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(AOKeyStore.APPLE, kc.getAbsolutePath(), "Mac-Afirma", null, null); //$NON-NLS-1$
        Assert.assertNotNull(ksm);
        final String[] aliases = ksm.getAliases();
        Assert.assertNotNull(aliases);
        
        final PrivateKeyEntry pke = ksm.getKeyEntry("anf usuario activo", null); //$NON-NLS-1$
        Assert.assertNotNull(pke);
        
        X509Certificate cert = (X509Certificate) pke.getCertificate();
        Assert.assertNotNull(cert);
        
        Assert.assertEquals(
            "C=ES, OU=Clase 2 persona fisica, EMAILADDRESS=test@prueba.com, SERIALNUMBER=12345678Z, SURNAME=Usuario Activo, GIVENNAME=ANF, CN=ANF Usuario Activo",  //$NON-NLS-1$
            cert.getSubjectX500Principal().toString()
        );
        
        Assert.assertNotNull(pke.getPrivateKey());
        
    }
    
    /** Prueba de carga y uso del <i>KayChain</i> del sistema.
     * Requiere importada en el sistema una entrada con alias "anf usuario activo" que tenga clave privada
     * @throws Exception */
    @Test
    public void testSystemKeyChain() throws Exception {
        if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
            return;
        }
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        
        final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(AOKeyStore.APPLE, null, "Mac-Afirma", null, null); //$NON-NLS-1$
        Assert.assertNotNull(ksm);
        final String[] aliases = ksm.getAliases();
        Assert.assertNotNull(aliases);
        
        final PrivateKeyEntry pke = ksm.getKeyEntry("anf usuario activo", null); //$NON-NLS-1$
        Assert.assertNotNull(pke);
        
        X509Certificate cert = (X509Certificate) pke.getCertificate();
        Assert.assertNotNull(cert);
        
        Assert.assertNotNull(
            cert.getSubjectX500Principal().toString()
        );
        
        Assert.assertNotNull(pke.getPrivateKey());
        
    }

}
