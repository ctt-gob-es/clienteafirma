/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo las licencias EUPL version 1.1 y GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.exceptions;

/**
 * Excepci&oacute;n para notificar que se ha obtenido o proporcionado una clave no v&aacute;lida. 
 */
public final class AOInvalidKeyException extends AOException {

	private static final long serialVersionUID = 4592634848579299321L;

	/**
	 * Crea la excepci&oacute;n con un mensaje determinado.
	 * @param msg Mensaje descriptivo de la excepci&oacute;n.
	 */
	public AOInvalidKeyException(String msg) {
		super(msg);
	}
	
	public AOInvalidKeyException(String msg, Throwable e) {
		super(msg, e);
	}

}
