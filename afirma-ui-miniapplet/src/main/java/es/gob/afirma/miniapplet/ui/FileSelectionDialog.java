package es.gob.afirma.miniapplet.ui;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import javax.jnlp.FileContents;
import javax.jnlp.FileOpenService;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;

import es.gob.afirma.core.AOCancelledOperationException;


/**
 * Di&aacute;logo para la selecci&oacute;n de un fichero y la devoluci&oacute;n de
 * un flujo de datos con su contenido.
 */
public class FileSelectionDialog {

    private FileOpenService jnlpFos;
    
    private String[] exts;
    
    /**
     * Crea el di&aacute;logo a partire de un servicio JNLP para la apertura de
     * ficheros.
     * @param jnlpFos Servicio para la apertura de ficheros.
     */
    public FileSelectionDialog(FileOpenService jnlpFos, String[] exts) {
        this.jnlpFos = jnlpFos;
        this.exts = exts;
    }
    
    /**
     * Muestra un di&aacute;logo modal para la selecci&oacute;n de un fichero y
     * devuelve el flujo de datos del contenido del fichero.
     * @return Flujo del datos del fichero seleccionado.
     * @throws AOCancelledOperationException Si el usuario cancela la operaci&oacute;n.
     * @throws IOException Si se produce alg&uacute;n error en la carga del fichero.
     */
    public InputStream getFileContent() throws AOCancelledOperationException, IOException {
//        FileContents fileContents = this.jnlpFos.openFileDialog(null, this.exts);
//        if (fileContents == null) {
//            throw new AOCancelledOperationException("El usuario cancelo la seleccion del fichero"); //$NON-NLS-1$
//        }
//
//        return fileContents.getInputStream();
    	
    	JFileChooser fc = new JFileChooser();
    	fc.setDialogTitle("Carga de datos"); //$NON-NLS-1$
    	fc.setFileFilter(this.getExtensionFileFilter(this.exts, "Ficheros")); //$NON-NLS-1$
    	int result = fc.showOpenDialog(null);
    	if (result != JFileChooser.APPROVE_OPTION) {
    		throw new AOCancelledOperationException("El usuario cancelo la seleccion del fichero"); //$NON-NLS-1$
    	}
    	return new FileInputStream(fc.getSelectedFile());
    }
    
    /**
     * Muestra un di&aacute;logo modal para la selecci&oacute;n de un fichero y
     * devuelve el nombre del mismo.
     * @return Nombre del fichero seleccionado.
     * @throws AOCancelledOperationException Si el usuario cancela la operaci&oacute;n.
     * @throws IOException Si se produce alg&uacute;n error en la selecci&oacute;n del fichero.
     */
    public String getFilename() throws AOCancelledOperationException, IOException {
        FileContents fileContents = this.jnlpFos.openFileDialog(null, null);
        if (fileContents == null) {
            throw new AOCancelledOperationException("El usuario cancelo la seleccion del fichero"); //$NON-NLS-1$
        }

        return fileContents.getName();
    }
    
    /**
     * Crea un filtro de fichero por extensi&oacute;n.
     * @param extensions Extensiones de fichero permitidas.
     * @param description Descripci&oacute;n del tipo de fichero.
     * @return
     */
    private FileFilter getExtensionFileFilter(final String[] extensions, final String description) {
    	return new FileFilter() {
			@Override
			public String getDescription() {
				return description;
			}
			
			@Override
			public boolean accept(File f) {
				for (String ext : extensions) {
					if (f.getName().endsWith(ext)) {
						return true;
					}
				}
				return false;
			}
		};
    }
}
