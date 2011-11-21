package es.gob.afirma.miniapplet;

import java.awt.Component;
import java.io.IOException;
import java.io.InputStream;
import java.security.PrivilegedExceptionAction;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.miniapplet.ui.FileSelectionDialog;

/**
 * Acci&oacute;n para la recuperaci&oacute;n del contenido de un fichero seleccionado
 * por el usuario.
 * @author Carlos Gamuci Mill&aacute;n
 */
public final class GetFileContentAction implements PrivilegedExceptionAction<byte[]>{

    private final String title;
    private final String[] exts;
    private final String desc;
    private final Component parent;
    
    /**
     * Crea la acci&oacute;n en base a un servicio JNLP para la carga de ficheros.
     * @param title T&iacute;tulo del di&aacute;logo.
     * @param exts Extensiones de fichero aceptadas por defecto.
     * @param description Descripci&opacute;n del tipo de fichero aceptado por defecto.
     * @param parent Componente padre sobre el que se mostrar&aacute; el di&aacute;logo.
     */
    public GetFileContentAction(final String title, final String[] exts, final String description,
    		 final Component parent) {
        this.title = title;
        this.exts = (exts != null ? exts.clone() : null);
        this.desc = description;
        this.parent = parent;
    }
    
    /**
     * Muestra al usuario un di&aacute;logo modal para la seleccion de un fichero y devuelve
     * su contenido.
     * @return El contenido del fichero.
     * @throws AOCancelledOperationException Cuando se cancela la operacion de selecci&oacute;n.
     * @throws IOException Cuando se produce un error al leer el fichero.
     */
	public byte[] run() throws IOException {
        final FileSelectionDialog dialog = new FileSelectionDialog(this.title, this.exts, this.desc, this.parent);
        final InputStream is = dialog.getFileContent();
        try {
        	return AOUtil.getDataFromInputStream(is);
        } 
        finally {
        	if (is != null) {
        		try {
        			is.close();
        		} 
        		catch (final Exception e) {
        			/* Ignoramos este error */
        		}
        	}
        }
	}
}
