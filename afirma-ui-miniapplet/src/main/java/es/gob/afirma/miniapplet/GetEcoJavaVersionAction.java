package es.gob.afirma.miniapplet;

import java.security.PrivilegedAction;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.Platform.JREVER;

/**
 * Acci&oacute;n privilegiada que permite obtener la versi&oacute;n de la m&aacute;quina
 * virtual de Java en un formato claramente identificable: JX, en donde X es un n&uacute;mero
 * entero que identifica la versi&oacute;n. X ser&aacute; 4 para Java 1.4 o invferior, 5 para Java 5,
 * 6 para Java 6 y 7 para cualquier otro.
 * @deprecated Se externaliza las comprobaciones de entorno.
 */
@Deprecated
class GetEcoJavaVersionAction implements PrivilegedAction<Platform.JREVER> {
    public Platform.JREVER run() { 
        final String jreVersion = System.getProperty("java.version");  //$NON-NLS-1$ 
        if (    jreVersion.startsWith("0.")  || //$NON-NLS-1$
                jreVersion.startsWith("1.0") ||  //$NON-NLS-1$
                jreVersion.startsWith("1.1") || //$NON-NLS-1$ 
                jreVersion.startsWith("1.2") ||  //$NON-NLS-1$
                jreVersion.startsWith("1.3") || //$NON-NLS-1$ 
                jreVersion.startsWith("1.4")) { //$NON-NLS-1$
            return JREVER.J4;
        }
        if (jreVersion.startsWith("1.5")) { //$NON-NLS-1$
            return JREVER.J5;
        }
        if (jreVersion.startsWith("1.6")) { //$NON-NLS-1$
            return JREVER.J6;
        }
        return JREVER.J7;
    } 
}
