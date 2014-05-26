package es.gob.afirma.ui.visor.ui;

import java.awt.Component;
import java.io.File;

import es.gob.afirma.ui.principal.Main;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.SelectionDialog;

/**
 * Enlace para cargar un fichero.
 * @author Carlos Gamuci
 */
final class LoadFileLinkAction {

    private final String text;
    private File selectedFile;
    private final Component parent;

    LoadFileLinkAction(final String text, final Component parent) {
        this.text = text;
        this.parent = parent;

        this.selectedFile = null;
    }

    void action() {
    	this.selectedFile = SelectionDialog.showFileOpenDialog(
    			this.parent, Messages.getString("LoadFileLinkAction.0"), //$NON-NLS-1$
    			Main.getPreferences().get("dialog.load.dir", null)); //$NON-NLS-1$
    }

    @Override
    public String toString() {
        return this.text;
    }

    /**
     * Recupera el fichero seleccionado. Si no se selecciono ningun, se
     * devolver&aacute; {@code null}.
     * @return Fichero selecionado.
     */
    public File getFile() {
    	return this.selectedFile;
    }
}
