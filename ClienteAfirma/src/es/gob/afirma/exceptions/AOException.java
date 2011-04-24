/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */

package es.gob.afirma.exceptions;

/**
 * Excepci&oacute;n gen&eacute;rica.
 * @version 1.0
 */
public class AOException extends Exception {

	private static final long serialVersionUID = -662191654860389176L;

	/**
	 * Contruye una excepci&oacute;n gen&eacute;rica con mensaje.
	 * @param msg Mensaje de la excepci&oacute;n
	 */
	public AOException(String msg) {
		super(msg);
	}
	
	/**
	 * Contruye una excepci&oacute;n gen&eacute;rica con mensaje y define su causa.
	 * @param msg Descripcion del error.
	 * @param e Causa del error.
	 */
	public AOException(String msg, Throwable e) {
		super(msg, e);
	}
}
