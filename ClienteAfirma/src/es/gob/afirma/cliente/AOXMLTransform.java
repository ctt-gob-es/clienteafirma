/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */

package es.gob.afirma.cliente;

/**
 * Transformada XML que se aplicar&aacute; a las firmas que las soporten.
 */
final class AOXMLTransform {

	/** Tipo de transformaci&oacute;n. */
	private String type = null;

	/** Subtipo de la transformaci&oacute;n. */
	private String subtype = null;
	
	/** Cuerpo de la transformaci&oacute;n. */
	private String body = null;
	
	/**
	 * Crea una transformaci&oacute;n XML.
	 * @param type Tipo de transformaci&oacute;n.
	 * @param subtype Subtipo de la transformaci&oacute;n.
	 * @param body Cuerpo de la transformaci&oacute;n.
	 */
	AOXMLTransform(String type, String subtype, String body) {
		
		if(type == null)
			throw new NullPointerException("El tipo de una transformacion XML no puede ser nulo");
		
		this.type = type;
		this.subtype = subtype;
		this.body = body;
	}

	/**
	 * Recupera el tipo de la transformaci&oacute;n.
	 * @return Tipo de transformaci&oacute;n.
	 */
	String getType() {
		return type;
	}

	/**
	 * Recupera el subtipo de la transformaci&oacute;n. Este elemento puede ser nulo.
	 * @return Subtipo de la transformaci&oacute;n.
	 */
	String getSubtype() {
		return subtype;
	}

	/**
	 * Recupera el cuerpo de la transformaci&oacute;n.
	 * @return Cuerpo de la transformaci&oacute;n.
	 */
	String getBody() {
		return body;
	}
}

