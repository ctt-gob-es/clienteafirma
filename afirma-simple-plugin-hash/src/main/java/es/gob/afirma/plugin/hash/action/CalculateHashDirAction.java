package es.gob.afirma.plugin.hash.action;

import java.awt.Frame;

import es.gob.afirma.standalone.plugins.PluginMenu;
import es.gob.afirma.standalone.ui.hash.CreateHashFiles;

/**
 * Clase del plugin de huella digital encargada de gestionar la acci&oacute; de calcular la huella digital de un directorio.
 */
public class CalculateHashDirAction extends PluginMenu {

	@Override
	public void init(Frame parent) {
		CreateHashFiles.startHashCreation(parent);
		
	}
	
	

}
