package es.gob.afirma.plugin.hash.action;

import java.awt.Window;

import es.gob.afirma.plugin.hash.CheckHashDirDialog;
import es.gob.afirma.standalone.plugins.PluginAction;

/**
 * Clase del plugin de huella digital encargada de gestionar la acci&oacute; de comprobar la huella digital de un directorio.
 */
public class CheckHashDirAction extends PluginAction {

	@Override
	public void start(final Window parent) {
		CheckHashDirDialog.startHashCheck(parent);
	}

}
