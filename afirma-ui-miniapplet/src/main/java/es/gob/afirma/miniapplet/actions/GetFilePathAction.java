package es.gob.afirma.miniapplet.actions;

import java.io.IOException;
import java.io.InputStream;

import javax.jnlp.FileOpenService;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.miniapplet.ui.FileSelectionDialog;

/**
 * Acci&oacute;n para la recuperaci&oacute;n de la ruta de un fichero.
 * @author Carlos Gamuci Mill&aacute;n
 */
public class GetFilePathAction {

    private FileOpenService jnlpFos;
    
    /**
     * Crea la acci&oacute;n en base a un servicio JNLP para la carga de ficheros.
     * @param fos Servicio de carga de ficheros.
     */
    public GetFilePathAction(final FileOpenService fos) {
        this.jnlpFos = fos;
    }
    
    /**
     * Muestra un di&aacute;logo modal para la carga de un fichero y recupera la ruta del mismo.
     * @return La ruta absoluta del fichero.
     * @throws AOCancelledOperationException Cuando se cancela la operaci&oacute;n de selecci&oacute;n.
     * @throws IOException Cuando se produce un error al leer el fichero.
     */
    public String getResult() throws AOCancelledOperationException, IOException {
        FileSelectionDialog dialog = new FileSelectionDialog(this.jnlpFos);
        return dialog.getFilePath();
    }
}
