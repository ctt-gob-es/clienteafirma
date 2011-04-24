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
 * Excepci&oacute;n utilizada para indicar que se ha sobrepasado el n&uacute;mero m&aacute;ximo
 * de intentos de acceso a un recurso protegido.
 */
public class AOMaxAttemptsExceededException extends AOException {

	private static final long serialVersionUID = -1340950945486548407L;

	/**
	 * Crea la excepci&oacute;n con un mensaje determinado.
	 * @param msg Mensaje descriptivo de la excepci&oacute;n.
	 */
	public AOMaxAttemptsExceededException(String msg) {
		super(msg);
	}
}
