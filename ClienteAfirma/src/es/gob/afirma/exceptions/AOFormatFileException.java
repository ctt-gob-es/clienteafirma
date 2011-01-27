/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.exceptions;

/**
 * Excepci&oacute;n para notificar que se ha proporcionado un fichero o dato con un formato
 * no v&aacute;lido para la acci&oacute;n en curso. 
 */
public final class AOFormatFileException extends AOException {

	private static final long serialVersionUID = 6785819338728771962L;

	/**
	 * Crea la excepci&oacute;n con un mensaje determinado.
	 * @param msg Mensaje descriptivo de la excepci&oacute;n.
	 */
	public AOFormatFileException(String msg) {
		super(msg);
	}

}