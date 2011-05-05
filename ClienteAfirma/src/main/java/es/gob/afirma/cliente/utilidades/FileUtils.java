package es.gob.afirma.cliente.utilidades;

import java.awt.Component;
import java.net.URI;
import java.security.AccessController;
import java.util.logging.Logger;

import es.gob.afirma.cliente.actions.LoadFileAction;

/**
 * Funciones de utilidad orientadas al uso de ficheros, teniendo en cuenta que estas deben
 * tratarse como funciones privilegiadas.
 */
public final class FileUtils {

	/**
	 * Carga un fichero de datos. Si ocurre un error durante la carga, se devuelve {@code null}.
	 * @param path Ruta del fichero.
	 * @param waitDialog Mostrar di&aacute;logo de espera.
	 * @param parent Componente padre sobre el que mostrar el di&aacute;logo de carga.
	 * @return Contenido del fichero.
	 */
	public static byte[] loadFile(final String path, final boolean waitDialog, final Component parent) {
		final LoadFileAction loadFileAction = new LoadFileAction(path, parent);
		loadFileAction.setWaitDialog(waitDialog);
		AccessController.doPrivileged(loadFileAction);
		if (loadFileAction.isError()) {
			Logger.getLogger("es.gob.afirma").severe(loadFileAction.getErrorMessage());
			return null;
		}
		return loadFileAction.getResult();
	}
	
	/**
	 * Carga un fichero de datos. Si ocurre un error durante la carga, se devuelve {@code null}.
	 * @param uri Ruta del fichero.
	 * @param waitDialog Mostrar di&aacute;logo de espera.
	 * @param parent Componente padre sobre el que mostrar el di&aacute;logo de carga.
	 * @return Contenido del fichero.
	 */
	public static byte[] loadFile(final URI uri, final boolean waitDialog, final Component parent) {
		final LoadFileAction loadFileAction = new LoadFileAction(uri, parent);
		loadFileAction.setWaitDialog(waitDialog);
		AccessController.doPrivileged(loadFileAction);
		if (loadFileAction.isError()) {
			Logger.getLogger("es.gob.afirma").severe(loadFileAction.getErrorMessage());
			return null;
		}
		return loadFileAction.getResult();
	}
}
