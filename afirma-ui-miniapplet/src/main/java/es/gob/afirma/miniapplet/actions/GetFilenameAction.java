package es.gob.afirma.miniapplet.actions;

import java.io.IOException;
import java.security.PrivilegedExceptionAction;

import javax.jnlp.FileOpenService;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.miniapplet.ui.FileSelectionDialog;

/**
 * Acci&oacute;n para la recuperaci&oacute;n del nombre de un fichero.
 * @author Carlos Gamuci Mill&aacute;n
 */
public class GetFilenameAction implements PrivilegedExceptionAction<String> {

    private FileOpenService jnlpFos;
    
    private String[] exts;
    
    /**
     * Crea la acci&oacute;n en base a un servicio JNLP para la carga de ficheros.
     * @param fos Servicio de carga de ficheros.
     */
    public GetFilenameAction(final FileOpenService fos, String[] exts ) {
        this.jnlpFos = fos;
        this.exts = exts;
    }
    
    /**
     * Muestra un di&aacute;logo modal para la carga de un fichero y recuperar el nombre
     * del mismo.
     * @return Nombre del fichero.
     * @throws AOCancelledOperationException Cuando se cancela la operaci&oacute;n de selecci&oacute;n.
     * @throws IOException Cuando se produce un error al leer el fichero.
     */
	@Override
	public String run() throws AOCancelledOperationException, IOException {
        FileSelectionDialog dialog = new FileSelectionDialog(this.jnlpFos, this.exts);
        return dialog.getFilename();
	}
}
