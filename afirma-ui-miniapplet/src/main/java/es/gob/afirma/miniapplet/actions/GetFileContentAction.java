package es.gob.afirma.miniapplet.actions;

import java.io.IOException;
import java.io.InputStream;
import java.security.PrivilegedExceptionAction;

import javax.jnlp.FileOpenService;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.miniapplet.ui.FileSelectionDialog;

/**
 * Acci&oacute;n para la recuperaci&oacute;n del contenido de un fichero seleccionado
 * por el usuario.
 * @author Carlos Gamuci Mill&aacute;n
 */
public class GetFileContentAction implements PrivilegedExceptionAction<byte[]>{

    private FileOpenService jnlpFos;
    
    /**
     * Crea la acci&oacute;n en base a un servicio JNLP para la carga de ficheros.
     * @param fos Servicio de carga de ficheros.
     */
    public GetFileContentAction(final FileOpenService fos) {
        this.jnlpFos = fos;
    }
    
    /**
     * Muestra al usuario un di&aacute;logo modal para la seleccion de un fichero y devuelve
     * su contenido.
     * @return El contenido del fichero.
     * @throws AOCancelledOperationException Cuando se cancela la operacion de selecci&oacute;n.
     * @throws IOException Cuando se produce un error al leer el fichero.
     */
	@Override
	public byte[] run() throws AOCancelledOperationException, IOException {
        FileSelectionDialog dialog = new FileSelectionDialog(this.jnlpFos, null);
        InputStream is = dialog.getFileContent();
        byte[] content = AOUtil.getDataFromInputStream(is);
        try {
            is.close();
        } catch (Exception e) {
            /* Ignoramos este error */
        }
        
        return content;
	}
}
