package es.gob.afirma.miniapplet;

import java.awt.Component;
import java.io.IOException;
import java.security.PrivilegedExceptionAction;

import es.gob.afirma.miniapplet.ui.FileSelectionDialog;

/**
 * Acci&oacute;n para la recuperaci&oacute;n del nombre de un fichero.
 * @author Carlos Gamuci Mill&aacute;n
 */
public final class GetFilePathAction implements PrivilegedExceptionAction<String> {
    
    private final String title;
    private final String[] exts;
    private final String desc;
    private final Component parent;
    
    /**
     * Crea la acci&oacute;n para la recuperaci&oacute;n de la ruta de un fichero.
     * @param title T&iacute;tulo del di&aacute;logo.
     * @param exts Extensiones de fichero aceptadas por defecto.
     * @param description Descripci&opacute;n del tipo de fichero aceptado por defecto.
     * @param parent Componente padre sobre el que se mostrar&aacute; el di&aacute;logo.
     */
    public GetFilePathAction(final String title, final String[] exts, final String description,
    		 final Component parent) {
        this.title = title;
        this.exts = (exts != null ? exts.clone() : null);
        this.desc = description;
        this.parent = parent;
    }
    
    /**
     * Muestra un di&aacute;logo modal para la carga de un fichero y recuperar la ruta
     * del mismo.
     * @return Nombre del fichero.
     * @throws AOCancelledOperationException Cuando se cancela la operaci&oacute;n de selecci&oacute;n.
     * @throws IOException Cuando se produce un error al leer el fichero.
     */
	public String run() throws IOException {
        return new FileSelectionDialog(this.title, this.exts, this.desc, this.parent).getPath();
	}
}
