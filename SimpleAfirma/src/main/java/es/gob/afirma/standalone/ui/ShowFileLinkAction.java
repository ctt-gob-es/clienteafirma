package es.gob.afirma.standalone.ui;

import java.awt.Desktop;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;

import javax.swing.JOptionPane;

import es.gob.afirma.misc.MimeHelper;

/**
 * Enlace para la apetura/guardado de un fichero.
 * @author Carlos Gamuci
 */
final class ShowFileLinkAction {

    private String text;
    private byte[] data;
    
    ShowFileLinkAction(final String text, final byte[] data) {
        this.text = text;
        this.data = data;
    }

    void action() {
        
        if (this.data == null) {
            return;
        }
        
        String ext = this.getCommonDataExtension(this.data);
        
        // Si conocemos la extension, intentamos abrir el fichero. Si no, permitimos
        // guardarlo con la extension que se desee.
        if (ext != null) {
            try {
                final File tmp = File.createTempFile("afirma", "." + ext);
                tmp.deleteOnExit();
                final OutputStream fos = new FileOutputStream(tmp);
                final OutputStream bos = new BufferedOutputStream(fos);
                bos.write(this.data);
                try { bos.flush(); } catch(final Exception e) {}
                try { bos.close(); } catch(final Exception e) {}
                try { fos.close(); } catch(final Exception e) {}
                Desktop.getDesktop().open(tmp);
            }
            catch(final Exception e) {
                UIUtils.showErrorMessage(
                        null,
                        "No se ha podido abrir el fichero,\ncompruebe que dispone de una aplicaci\u00F3n instalada para visualizar ficheros '." + ext + "'",
                        "Error",
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
                    "Guardar datos"
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
