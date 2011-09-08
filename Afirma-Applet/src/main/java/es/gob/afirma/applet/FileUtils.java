package es.gob.afirma.applet;

import java.net.URI;
import java.security.AccessController;
import java.util.logging.Logger;

import es.gob.afirma.applet.actions.LoadFileAction;

/** Funciones de utilidad orientadas al uso de ficheros, teniendo en cuenta que
 * estas deben tratarse como funciones privilegiadas. */
public final class FileUtils {

    /** Carga un fichero de datos. Si ocurre un error durante la carga, se
     * devuelve {@code null}.
     * @param path
     *        Ruta del fichero.
     * @return Contenido del fichero. */
    public static byte[] loadFile(final String path) {
        final LoadFileAction loadFileAction = new LoadFileAction(path);
        AccessController.doPrivileged(loadFileAction);
        if (loadFileAction.isError()) {
            Logger.getLogger("es.gob.afirma").severe(loadFileAction.getErrorMessage()); //$NON-NLS-1$
            return null;
        }
        return loadFileAction.getResult();
    }

    /** Carga un fichero de datos. Si ocurre un error durante la carga, se
     * devuelve {@code null}.
     * @param uri
     *        Ruta del fichero.
     * @return Contenido del fichero. */
    public static byte[] loadFile(final URI uri) {
        final LoadFileAction loadFileAction = new LoadFileAction(uri);
        AccessController.doPrivileged(loadFileAction);
        if (loadFileAction.isError()) {
            Logger.getLogger("es.gob.afirma").severe(loadFileAction.getErrorMessage()); //$NON-NLS-1$
            return null;
        }
        return loadFileAction.getResult();
    }
}
