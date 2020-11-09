package es.gob.afirma.plugin.hash.action;

import java.awt.Frame;

import es.gob.afirma.standalone.plugins.PluginMenu;
import es.gob.afirma.standalone.ui.hash.CheckHashFiles;

/**
 * Clase del plugin de huella digital encargada de gestionar la acci&oacute; de comprobar la huella digital de un directorio.
 */
public class CheckHashDirAction extends PluginMenu {
	
	@Override
	public void init(Frame parent) {
		CheckHashFiles.startHashCheck(parent);
		
	}

}
