package es.gob.afirma.miniapplet.ui;

import java.io.IOException;
import java.io.InputStream;

import javax.jnlp.FileContents;
import javax.jnlp.FileOpenService;

import es.gob.afirma.core.AOCancelledOperationException;


/**
 * Di&aacute;logo para la selecci&oacute;n de un fichero y la devoluci&oacute;n de
 * un flujo de datos con su contenido.
 */
public class FileSelectionDialog {

    private FileOpenService jnlpFos;
    
    /**
     * Crea el di&aacute;logo a partire de un servicio JNLP para la apertura de
     * ficheros.
     * @param jnlpFos Servicio para la apertura de ficheros.
     */
    public FileSelectionDialog(FileOpenService jnlpFos) {
        this.jnlpFos = jnlpFos;
    }
    
    /**
     * Muestra un di&aacute;logo modal para la selecci&oacute;n de un fichero y
     * devuelve el flujo de datos del contenido del fichero.
     * @return Flujo del datos del fichero seleccionado.
     * @throws AOCancelledOperationException Si el usuario cancela la operaci&oacute;n.
     * @throws IOException Si se produce alg&uacute;n error en la carga del fichero.
     */
    public InputStream getFileContent() throws AOCancelledOperationException, IOException {
        FileContents fileContents = this.jnlpFos.openFileDialog(null, null);
        if (fileContents == null) {
            throw new AOCancelledOperationException("El usuario cancelo la seleccion del fichero"); //$NON-NLS-1$
        }

        return fileContents.getInputStream();
        
//        JFileChooser fc = new JFileChooser();
//        if (fc.showOpenDialog(null) != JFileChooser.APPROVE_OPTION) {
//            throw new AOCancelledOperationException("El usuario cancelo la seleccion del fichero"); //$NON-NLS-1$
//        }
//        return new FileInputStream(fc.getSelectedFile());
    }
    
    /**
     * Muestra un di&aacute;logo modal para la selecci&oacute;n de un fichero y
     * devuelve el flujo de datos del contenido del fichero.
     * @return Flujo del datos del fichero seleccionado.
     * @throws AOCancelledOperationException Si el usuario cancela la operaci&oacute;n.
     * @throws IOException Si se produce alg&uacute;n error en la selecci&oacute;n del fichero.
     */
    public String getFilePath() throws AOCancelledOperationException, IOException {
        FileContents fileContents = this.jnlpFos.openFileDialog(null, null);
        if (fileContents == null) {
            throw new AOCancelledOperationException("El usuario cancelo la seleccion del fichero"); //$NON-NLS-1$
        }

        return fileContents.getName();
    }
}
