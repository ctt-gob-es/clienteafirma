package es.gob.afirma.standalone.ui;

import java.util.logging.Logger;

final class MacHelpHooker {
    
    static {
        try {
            System.loadLibrary("JavaHelpHook"); //$NON-NLS-1$
        }
        catch(final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("No ha sido posible cargar la biblioteca nativa de apertura de Apple Help: " + e);  //$NON-NLS-1$//$NON-NLS-2$
        }
    }
    
    static native void showHelp();

}
