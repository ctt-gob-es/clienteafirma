package es.gob.afirma.miniapplet.actions;

import java.awt.Component;
import java.io.IOException;
import java.security.PrivilegedExceptionAction;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.miniapplet.ui.FileSelectionDialog;

/**
 * Acci&oacute;n para la recuperaci&oacute;n del nombre de un fichero.
 * @author Carlos Gamuci Mill&aacute;n
 */
public class GetFilenameAction implements PrivilegedExceptionAction<String> {
    
    private String title;
    
    private String[] exts;
    
    private String desc;
    
    private Component parent;
    
    /**
     * Crea la acci&oacute;n en base a un servicio JNLP para la carga de ficheros.
     * @param title T&iacute;tulo del di&aacute;logo.
     * @param exts Extensiones de fichero aceptadas por defecto.
     * @param description Descripci&opacute;n del tipo de fichero aceptado por defecto.
     * @param parent Componente padre sobre el que se mostrar&aacute; el di&aacute;logo.
     */
    public GetFilenameAction(final String title, final String[] exts, final String description,
    		 final Component parent) {
        this.title = title;
        this.exts = exts;
        this.desc = description;
        this.parent = parent;
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
        FileSelectionDialog dialog = new FileSelectionDialog(this.title, this.exts, this.desc, this.parent);
        return dialog.getPath();
	}
}
