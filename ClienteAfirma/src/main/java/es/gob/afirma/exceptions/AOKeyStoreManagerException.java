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
 * Excepci&oacute;n para notificar un error en la inicializacion o en el acceso a un
 * almac&eacute;n de certificados. 
 */
public final class AOKeyStoreManagerException extends AOException {

	private static final long serialVersionUID = 2896862509190263027L;

  /**
	 * Crea la excepci&oacute;n con un mensaje determinado.
	 * @param msg Mensaje descriptivo de la excepci&oacute;n.
	 */
	public AOKeyStoreManagerException(String msg) {
	    super(msg);
	}

  /**
	 * Crea la excepci&oacute;n con un mensaje determinado.
	 * @param msg Mensaje descriptivo de la excepci&oacute;n.
	 * @param e Excepci&oacute;n que ha causado el lanzamiento de esta.
	 */
	public AOKeyStoreManagerException(String msg, Throwable e) {
	    super(msg, e);
	}
}
