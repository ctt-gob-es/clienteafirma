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
 * Excepci&oacute;n para indicar que se ha intentado abrir un sobre digital con un certificado
 * que no estaba entre sus destinatarios. 
 */
public class AOInvalidRecipientException extends AOException {

	private static final long serialVersionUID = 2582498859303039013L;

	/**
	 * Crea la excepci&oacute;n con un mensaje determinado.
	 * @param msg Mensaje descriptivo de la excepci&oacute;n.
	 */
  public AOInvalidRecipientException(String msg) {
      super(msg);
  }

  /**
	 * Crea la excepci&oacute;n con un mensaje determinado.
	 * @param msg Mensaje descriptivo de la excepci&oacute;n.
	 * @param e Excepci&oacute;n que ha causado el lanzamiento de esta.
	 */
  public AOInvalidRecipientException(String msg, Throwable e) {
      super(msg, e);
  }
	
}
