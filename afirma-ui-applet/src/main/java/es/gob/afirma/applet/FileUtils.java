package es.gob.afirma.applet;

import java.net.URI;
import java.security.AccessController;
import java.util.logging.Logger;


/** Funciones de utilidad orientadas al uso de ficheros, teniendo en cuenta que
 * estas deben tratarse como funciones privilegiadas. */
public final class FileUtils {

    /** Carga un fichero de datos. Si ocurre un error durante la carga, se
     * devuelve {@code null}.
     * @param path
     *        Ruta del fichero.
     * @return Contenido del fichero. */
    public static byte[] loadFile(final String path) {
    	try {
    		return AccessController.doPrivileged(new LoadFileAction(path));
    	} catch (final Exception e) {
    		Logger.getLogger("es.gob.afirma").severe(e.toString()); //$NON-NLS-1$
    		return null;
    	}
    }

    /** Carga un fichero de datos. Si ocurre un error durante la carga, se
     * devuelve {@code null}.
     * @param uri
     *        Ruta del fichero.
     * @return Contenido del fichero. */
    public static byte[] loadFile(final URI uri) {
    	try {
    		return AccessController.doPrivileged(new LoadFileAction(uri));
    	} catch (final Exception e) {
    		Logger.getLogger("es.gob.afirma").severe(e.toString()); //$NON-NLS-1$
    		return null;
    	}
    }
}
