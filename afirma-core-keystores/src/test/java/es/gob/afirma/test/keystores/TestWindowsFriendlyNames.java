package es.gob.afirma.test.keystores;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.InvalidKeyException;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.main.common.AOKeyStore;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.main.common.AOKeystoreAlternativeException;
import es.gob.afirma.keystores.main.common.KeyStoreUtilities;

/** Prueba la conversi&oacute;n de alias en nombres significativos en CAPI.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestWindowsFriendlyNames {
    
	private static final AOKeyStore KEYSTORE_TYPE = AOKeyStore.WINDOWS;
	private static final String KEYSTORE_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
	private static final String KEYSTORE_PASS = "12341234"; //$NON-NLS-1$
	
    /** Prueba la conversi&oacute;n de alias en nombres significativos en CAPI. 
     * @throws AOKeystoreAlternativeException 
     * @throws InvalidKeyException */
    @Test
    public void testWindowsFriendlyNames() throws InvalidKeyException, AOKeystoreAlternativeException, Exception {
        if (!Platform.OS.WINDOWS.equals(Platform.getOS())) {
            return;
        }
        
        byte[] p12file = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(KEYSTORE_PATH)); //$NON-NLS-1$
        Assert.assertTrue("No se ha podido leer el P12", p12file.length > 0); //$NON-NLS-1$
        File tmpFile = File.createTempFile("temp", "afirma"); //$NON-NLS-1$ //$NON-NLS-2$
        tmpFile.deleteOnExit();
        OutputStream os = new FileOutputStream(tmpFile);
        os.write(p12file);
        os.flush();
        os.close();

        PasswordCallback pc = new PasswordCallback(">", false); //$NON-NLS-1$
        pc.setPassword(KEYSTORE_PASS.toCharArray());
        
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
        		KEYSTORE_TYPE,
                tmpFile.getAbsolutePath(), 
               "TEST",  //$NON-NLS-1$
               pc, 
               null
        );
//        for(final String al : ksm.getAliases()) {
//            System.out.println(al);
//        }
        System.out.println();
        final Map<String, String> aliases = KeyStoreUtilities.getAliasesByFriendlyName(
               ksm.getAliases(), 
               ksm, 
               true, // Check private keys
               true, // Show expired
               null  // filters
       );
       for (final String key : aliases.keySet()) {
           System.out.println(key + "\n\t" + aliases.get(key)); //$NON-NLS-1$
       }
    }

}
