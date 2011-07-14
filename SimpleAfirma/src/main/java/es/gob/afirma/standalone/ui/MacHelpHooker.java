package es.gob.afirma.standalone.ui;

import java.util.logging.Logger;

import es.gob.afirma.misc.Platform;

final class MacHelpHooker {
    
    private static boolean loaded = false;
    
    static {
        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            try {
                System.loadLibrary("JavaHelpHook"); //$NON-NLS-1$
                loaded = true;
            }
            catch(final UnsatisfiedLinkError e) {
                Logger.getLogger("es.gob.afirma").warning("No se encuentra la biblioteca nativa de apertura de Apple Help: " + e);  //$NON-NLS-1$//$NON-NLS-2$
            }
            catch(final Exception e) {
                Logger.getLogger("es.gob.afirma").warning("No ha sido posible cargar la biblioteca nativa de apertura de Apple Help: " + e);  //$NON-NLS-1$//$NON-NLS-2$
            }
        }
    }
    
    private static native void showHelp();
    
    static boolean isMacHelpAvailable() {
        return loaded;
    }

}
