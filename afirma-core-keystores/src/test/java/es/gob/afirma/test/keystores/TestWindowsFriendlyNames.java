package es.gob.afirma.test.keystores;

import org.junit.Test;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.main.common.AOKeyStore;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;

/** Prueba la conversi&oacute;n de alias en nombres significativos en CAPI.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestWindowsFriendlyNames {
    
    /** Prueba la conversi&oacute;n de alias en nombres significativos en CAPI. */
    @Test
    public void testWindowsFriendlyNames() {
        if (!Platform.OS.WINDOWS.equals(Platform.getOS())) {
            return;
        }
        System.out.println(AOKeyStoreManager.getKeyStore(AOKeyStore.WINDOWS.getDescription()));
    }

}
