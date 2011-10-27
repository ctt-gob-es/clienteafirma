package es.gob.afirma.miniapplet.actions;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.PrivilegedExceptionAction;

import javax.jnlp.FileSaveService;

import es.gob.afirma.core.AOCancelledOperationException;

/**
 * Acci&oacute;n para almacenar un fichero en disco.
 * @author Carlos Gamuci Mill&aacute;n
 */
public class SaveFileAction implements PrivilegedExceptionAction<Boolean> {

    private FileSaveService jnlpFss;
    
    private byte[] data;
    
    private String[] exts;
    
    private String filenameHint;
    
    private String pathHint;
    
    /**
     * Crea la acci&oacute;n en base a un servicio JNLP para la carga de ficheros.
     * @param fss Servicio de guardado de ficheros.
     * @param data Datos que se desean guardar.
     * @param exts Extensiones permitidas para los datos.
     * @param filenameHint Nombre propuesto para el fichero.
     * @param pathHint Ruta propuesta para el guardado.
     */
    public SaveFileAction(final FileSaveService fss, byte[] data, String[] exts, String filenameHint, String pathHint) {
        this.jnlpFss = fss;
        this.data = data;
        this.exts = exts;
        this.filenameHint = filenameHint;
        this.pathHint = pathHint;
    }
    
    /**
     * Muestra un di&aacute;logo modal para el guardado de un fichero y lo salva en el directorio
     * y con el nombre indicado por el usuario.
     * @return {@code true} si el fichero se almacen&oacute; correctamente.
     * @throws AOCancelledOperationException Cuando se cancela la operaci&oacute;n.
     * @throws IOException Cuando se produce un error al almacenar el fichero.
     */
	@Override
	public Boolean run() throws AOCancelledOperationException, IOException {
    	this.jnlpFss.saveFileDialog(
    			this.pathHint, this.exts, new ByteArrayInputStream(this.data), this.filenameHint);
        return Boolean.TRUE;
	}
}
