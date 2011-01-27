/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.callbacks;

import java.awt.Component;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.ui.AOUIManager;

/**
 * <i>PasswordCallbak</i> que muestra un di&aacute;logo Swing para solicitar una contrase&ntilde;a.
 */
public final class UIPasswordCallback extends PasswordCallback {

	private static final long serialVersionUID = 1719174318602363633L;

	/**
	 * Componente padre sobre el que se mostrar&aacute; el di&aacute;logo para la inserci&oacute;n
	 * de la contrase&ntilde;a. 
	 */
	private Component parent = null;
	
	/**
	 * Crea una <i>CallBack</i> para solicitar al usuario una contrase&ntilde;a mediante
	 * un di&aacute;logo gr&aacute;fico. La contrase&ntilde;a no se retiene ni almacena
	 * internamente en ning&uacute;n momento
	 * @param prompt Texto del di&aacute;logo para solicitar la contrase&ntilde;a
	 * @param parent Componente padre para la modalidad del di&aacute;logo
	 */
	public UIPasswordCallback(final String prompt, final Component parent) {
		super(prompt, false);
		this.parent = parent;
	}
	
	@Override
	public char[] getPassword() {
		return AOUIManager.getPassword(this.getPrompt(), parent);
	}
}
