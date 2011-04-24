/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */

package es.gob.afirma.callbacks;

import javax.security.auth.callback.PasswordCallback;

/**
 * PasswordCallback que siempre devuelve <code>null</code> como contrase&ntilde;a.
 */
public final class NullPasswordCallback extends PasswordCallback {

	private static final long serialVersionUID = -5926953046433722802L;

	/**
	 * Contruye el la forma b&aacute;sica de la clase. 
	 */
	public NullPasswordCallback() {
		super(">", false);
	}
	
	@Override
	public char[] getPassword() {
		return null;
	}
}
