package es.gob.afirma.standalone.ui;

import java.awt.Desktop;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;

import javax.swing.JOptionPane;

import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.standalone.Messages;

/**
 * Enlace para la apetura/guardado de un fichero.
 * @author Carlos Gamuci
 */
final class ShowFileLinkAction {

    private final String text;
    private final byte[] data;

    ShowFileLinkAction(final String text, final byte[] data) {
        this.text = text;
        this.data = data;
    }

    void action() {

        if (this.data == null) {
            return;
        }

        final String ext = this.getCommonDataExtension(this.data);

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
                UIUtils.showErrorMessage(
                        null,
                        Messages.getString("ShowFileLinkAction.2") + " '" + ext + "'",  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
                        Messages.getString("ShowFileLinkAction.0"), //$NON-NLS-1$
                        JOptionPane.ERROR_MESSAGE
                );
            }
        }
        else {
            FileUIManager.saveFile(
                    null,
                    this.data,
                    null,
                    null,
                    null,
                    Messages.getString("ShowFileLinkAction.1") //$NON-NLS-1$
            );
        }
    }

    private String getCommonDataExtension(final byte[] dat) {
        return new MimeHelper(dat).getExtension();
    }

    @Override
    public String toString() {
        return this.text;
    }

}
