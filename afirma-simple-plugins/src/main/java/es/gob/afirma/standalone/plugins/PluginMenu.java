package es.gob.afirma.standalone.plugins;

import java.awt.Frame;

/**
 * Clase abstracta que define los m&eacute;todos necesarios para los plugins que contienen menus.
 */
public abstract class PluginMenu extends PluginAction {
	
	/**
	 * M&eacute;todo encargado de realizar la acci&oacute;n del menu cuando se pulsa.
	 * @param parent Ventana padre.
	 */
	public abstract void init(Frame parent); 

}
