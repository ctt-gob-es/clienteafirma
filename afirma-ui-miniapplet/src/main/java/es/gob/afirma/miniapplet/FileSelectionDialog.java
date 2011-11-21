package es.gob.afirma.miniapplet;

import java.awt.Component;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;

import es.gob.afirma.core.AOCancelledOperationException;


/**
 * Di&aacute;logo para la selecci&oacute;n de un fichero y la devoluci&oacute;n de
 * un flujo de datos con su contenido.
 */
public final class FileSelectionDialog {

    private final String title;
    
    private final String[] exts;
    
    private final String desc;
    
    private final Component parent;
    
    /**
     * Crea el di&aacute;logo a partire de un servicio JNLP para la apertura de
     * ficheros.
     * @param title T&iacute;tulo a utilizar en el di&aacute;logo de selecci&oacute;n.
     * @param exts Extensiones de fichero aceptadas.
     * @param description Descripci&oacute;n del tipo de fichero aceptado por defecto.
     * @param parent Componente padre sobre el que se mostrar&aacute; el di&aacute;logo.
     */
    public FileSelectionDialog(final String title, 
                               final String[] exts, 
                               final String description, 
                               final Component parent) {
        this.title = title;
        this.exts = (exts != null ? exts.clone() : null);
        this.desc = description;
        this.parent = parent;
    }
    
    /**
     * Muestra un di&aacute;logo modal para la selecci&oacute;n de un fichero y
     * devuelve el flujo de datos del contenido del fichero.
     * @return Flujo del datos del fichero seleccionado.
     * @throws AOCancelledOperationException Si el usuario cancela la operaci&oacute;n.
     * @throws IOException Si se produce alg&uacute;n error en la carga del fichero.
     */
    public InputStream getFileContent() throws IOException {
    	return new FileInputStream(this.selectFile().getSelectedFile());
    }
    
    /**
     * Muestra un di&aacute;logo modal para la selecci&oacute;n de un fichero y
     * devuelve la ruta del mismo.
     * @return Ruta absoluta del fichero seleccionado.
     * @throws AOCancelledOperationException Si el usuario cancela la operaci&oacute;n.
     */
    public String getPath() {
    	return this.selectFile().getSelectedFile().getAbsolutePath();
    }
    

    /**
     * Muestra un di&aacute;logo modal que permite la selecci&oacute;n de un fichero. 
     * @return Di&aacute;logo con el fichero seleccionado.
     * @throws AOCancelledOperationException Si el usuario cancela la operaci&oacute;n.
     */
    private JFileChooser selectFile() {
    	final JFileChooser fc = new JFileChooser();
    	if (this.title != null) {
    		fc.setDialogTitle(this.title);
    	}
    	if (this.exts != null) {
    		fc.setFileFilter(this.getExtensionFileFilter(this.exts, this.desc));
    	}
    	int result = fc.showOpenDialog(this.parent);
    	if (result != JFileChooser.APPROVE_OPTION) {
    		throw new AOCancelledOperationException("El usuario cancelo la seleccion del fichero"); //$NON-NLS-1$
    	}
    	return fc;
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
				if (f.isDirectory()) {
					return true;
				}
				for (final String ext : extensions) {
					if (f.getName().endsWith(ext)) {
						return true;
					}
				}
				return false;
			}
		};
    }
}
