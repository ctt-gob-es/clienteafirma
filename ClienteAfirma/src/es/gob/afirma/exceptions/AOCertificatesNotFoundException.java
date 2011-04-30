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
 * Excepci&oacute;n para notificar que no se han encontrado certificado de usuario en un
 * almac&eacute;n de certificados. 
 */
public final class AOCertificatesNotFoundException extends AOException {

	private static final long serialVersionUID = -6996346324337434742L;

	/**
	 * Crea la excepci&oacute;n con un mensaje determinado.
	 * @param msg Mensaje descriptivo de la excepci&oacute;n.
	 */
	public AOCertificatesNotFoundException(String msg) {
		super(msg);
	}

}
