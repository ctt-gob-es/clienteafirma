package es.gob.afirma.miniapplet.actions;

import java.io.IOException;
import java.io.InputStream;

import javax.jnlp.FileOpenService;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.miniapplet.ui.FileSelectionDialog;

public class GetFileContentAction {

    private FileOpenService jnlpFos;
    
    public GetFileContentAction(final FileOpenService fos) {
        this.jnlpFos = fos;
    }
    
    public String getResult() throws AOCancelledOperationException, IOException {
        FileSelectionDialog dialog = new FileSelectionDialog(this.jnlpFos);
        InputStream is = dialog.getFileContent();
        byte[] content = AOUtil.getDataFromInputStream(is);
        try {
            is.close();
        } catch (Exception e) {
            /* Ignoramos este error */
        }
        
        return Base64.encodeBytes(content);
    }
}
