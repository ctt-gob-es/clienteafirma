/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.configurator;

/** Cuando no se puede generar el certificado.
 * Com&uacute;nmente por un error de configuraci&oacute;n. */
public final class ConfigurationException extends Exception {

	/** Serial Id. */
	private static final long serialVersionUID = 2072955063471096921L;

	/** Error de configuraci&oacute;n para la generaci&oacute;n del certificado.
	 * @param msg Descripci&oacute;n del error. */
	public ConfigurationException(final String msg) {
		super(msg);
	}

	/** Error de configuraci&oacute;n para la generaci&oacute;n del certificado.
	 * @param msg Descripci&oacute;n del error.
	 * @param cause Motivo que origin&oacute; el error. */
	public ConfigurationException(final String msg, final Throwable cause) {
		super(msg, cause);
	}
}
