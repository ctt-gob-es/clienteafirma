package es.gob.afirma.ui.visor.ui;

import java.awt.Desktop;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;

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

    ShowFileLinkAction(final String text, final byte[] data) {
        this.text = text;
        this.data = data.clone();
    }

    void action() {

        if (this.data == null) {
            return;
        }

        final String ext = ShowFileLinkAction.getCommonDataExtension(this.data);

        // Si conocemos la extension, intentamos abrir el fichero. Si no, permitimos
        // guardarlo con la extension que se desee.
        if (ext != null) {
            try {
                final File tmp = File.createTempFile("afirma", "." + ext);   //$NON-NLS-1$//$NON-NLS-2$
                tmp.deleteOnExit();
                final OutputStream fos = new FileOutputStream(tmp);
                final OutputStream bos = new BufferedOutputStream(fos);
                bos.write(this.data);
                try { bos.flush(); } catch(final Exception e) { /* Ignoramos los errores */ }
                try { bos.close(); } catch(final Exception e) { /* Ignoramos los errores */ }
                try { fos.close(); } catch(final Exception e) { /* Ignoramos los errores */ }
                Desktop.getDesktop().open(tmp);
            }
            catch(final Exception e) {
            	CustomDialog.showMessageDialog(null, true, Messages.getString("ShowFileLinkAction.2") + " '" + ext + "'", Messages.getString("ShowFileLinkAction.0"), JOptionPane.ERROR_MESSAGE);
            }
        }
        else {
            SelectionDialog.saveDataToFile(
                    Messages.getString("ShowFileLinkAction.1"), //$NON-NLS-1$
                    this.data,
                    null,
                    null,
                    this);
        }
    }

    private static String getCommonDataExtension(final byte[] dat) {
        return new MimeHelper(dat).getExtension();
    }

    @Override
    public String toString() {
        return this.text;
    }

}
