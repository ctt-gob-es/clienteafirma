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
 * Excepci&oacute;n que indica una operaci&oacute;n cancelada voluntariamente por el usuario.
 */
public final class AOCancelledOperationException extends RuntimeException {

	private static final long serialVersionUID = 4447842480432712246L;

	/**
	 * Crea una excepci&oacute;n sin informaci&oacute;n adicional.
	 */
	public AOCancelledOperationException() {
		super();
	}
	
	/**
	 * Crea la excepci&oacute;n con un mensaje determinado.
	 * @param msg Mensaje descriptivo de la excepci&oacute;n.
	 */
	public AOCancelledOperationException(String msg) {
		super(msg);
	}

}
