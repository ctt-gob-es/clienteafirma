package es.gob.afirma.ui.visor.ui;

import java.awt.Component;
import java.awt.Desktop;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.SelectionDialog;

/**
 * Enlace para la apetura/guardado de un fichero.
 * @author Carlos Gamuci
 */
final class ShowFileLinkAction {

    private final String text;
    private final byte[] data;
    private final File dataFile;
    private final Component parent;

    ShowFileLinkAction(final String text, final byte[] data, final Component parent) {
        this.text = text;
        this.data = data.clone();
        this.dataFile = null;
        this.parent = parent;
    }

    ShowFileLinkAction(final String text, final File dataFile, final Component parent) {
        this.text = text;
        this.data = null;
        this.dataFile = dataFile;
        this.parent = parent;
    }

    void action() {
        if (this.dataFile != null) {
        	openFile(this.dataFile, this.parent);
        }
        else if (this.data != null) {
        	openData(this.data, this.parent);
        }
    }

    /** Abre un fichero de datos.
     * @param dataFile Fichero de datos. */
    private static void openFile(final File dataFile, final Component parent) {

    	if (!dataFile.exists()) {
    		CustomDialog.showMessageDialog(parent, true,
    				Messages.getString("ShowFileLinkAction.4") + " " + dataFile.getAbsolutePath(), //$NON-NLS-1$ //$NON-NLS-2$
    				Messages.getString("ShowFileLinkAction.0"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
    		return;
    	}

    	if (!dataFile.canRead()) {
    		CustomDialog.showMessageDialog(parent, true,
    				Messages.getString("ShowFileLinkAction.5") + " " + dataFile.getAbsolutePath(), //$NON-NLS-1$ //$NON-NLS-2$
    				Messages.getString("ShowFileLinkAction.0"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
    		return;
    	}

        try {
        	Desktop.getDesktop().open(dataFile);
        }
        catch(final Exception e) {
        	CustomDialog.showMessageDialog(parent, true, Messages.getString("ShowFileLinkAction.3"), Messages.getString("ShowFileLinkAction.0"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
        }
    }

    /** Abre unos datos en la aplicaci&oacute;n por defecto del sistema.
     * @param data Datos. */
    private static void openData(final byte[] data, final Component parent) {
    	final String ext = ShowFileLinkAction.getCommonDataExtension(data);

        // Si conocemos la extension, intentamos abrir el fichero. Si no, permitimos
        // guardarlo con la extension que se desee.
        boolean openned = true;
        if (ext != null) {
            try {
                final File tmp = File.createTempFile("afirma", "." + ext);   //$NON-NLS-1$//$NON-NLS-2$
                tmp.deleteOnExit();
                try (
	                final OutputStream fos = new FileOutputStream(tmp);
	                final OutputStream bos = new BufferedOutputStream(fos);
        		) {
                	bos.write(data);
                	bos.flush();
                }
                Desktop.getDesktop().open(tmp);
            }
            catch(final Exception e) {
            	CustomDialog.showMessageDialog(parent, true, Messages.getString("ShowFileLinkAction.2") + " '" + ext + "'", Messages.getString("ShowFileLinkAction.0"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
            	openned = false;
            }
        }

        if (ext == null || !openned) {
            SelectionDialog.saveDataToFile(
                    Messages.getString("ShowFileLinkAction.1"), //$NON-NLS-1$
                    data,
                    null,
                    null,
                    parent);
        }
    }

    private static String getCommonDataExtension(final byte[] dat) {
    	String ext = null;
    	try {
    		ext = new MimeHelper(dat).getExtension();
    	}
    	catch (final Exception e) {
    		Logger.getLogger("es.gob.afirma").log(Level.WARNING, "Error al identificar el tipo de dato", e); //$NON-NLS-1$ //$NON-NLS-2$
		}
        return ext == null || ext.length() == 0 ? null : ext;
    }

    @Override
    public String toString() {
        return this.text;
    }

}
