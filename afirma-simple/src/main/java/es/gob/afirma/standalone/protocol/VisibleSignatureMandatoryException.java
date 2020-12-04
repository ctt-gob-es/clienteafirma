/* Copyright (C) 2020 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */
package es.gob.afirma.standalone.protocol;

/**
 * Error producido cuando se cancela la introducci&oacute;n del area para mostrar la
 * firma visible cuando es obligatorio mostrarla.
 */
public class VisibleSignatureMandatoryException extends Exception {

	private static final long serialVersionUID = -1102815287789465969L;

	/**
	 * Construye una excepci&oacute;n de error debido a la obligatoriedad de mostrar
	 * la firma en el documento.
	 * 
	 * @param msg Mensaje de la excepci&oacute;n.
	 */
	public VisibleSignatureMandatoryException(final String msg) {
		super(msg);
	}

	/**
	 * Construye una excepci&oacute;n de error debido a la obligatoriedad de mostrar
	 * la firma en el documento.
	 * 
	 * @param msg Mensaje de la excepci&oacute;n.
	 * @param t   Causa inicial de la excepci&oacute;n.
	 */
	public VisibleSignatureMandatoryException(final String msg, final Throwable t) {
		super(msg, t);
	}
}
