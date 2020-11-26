package es.gob.afirma.plugin.hash.action;

import java.awt.Window;

import es.gob.afirma.plugin.hash.CheckHashFileDialog;
import es.gob.afirma.standalone.plugins.PluginAction;

/**
 * Clase del plugin de huella digital encargada de gestionar la acci&oacute; de comprobar la huella digital de un fichero.
 */
public class CheckHashFileAction extends PluginAction {

	@Override
	public void start(final Window parent) {
		CheckHashFileDialog.launch(parent);
	}
}
