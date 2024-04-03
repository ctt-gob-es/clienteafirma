/* Copyright (C) 2024 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone;

/** Excepcion de linea de comandos que se&ntilde;ala un problema con los par&aacute;metros de entrada.
 * @author Carlos Gamuci */
public final class CommandLineParameterException extends CommandLineException {

	/** Serial Id. */
	private static final long serialVersionUID = 6635394222787127627L;

	CommandLineParameterException(final String message) {
		super(message);
	}

	CommandLineParameterException(final Throwable cause) {
		super(cause);
	}

	CommandLineParameterException(final String message, final Throwable cause) {
		super(message, cause);
	}

	CommandLineParameterException(final String message, final boolean usingGui) {
		super(message, usingGui);
	}

	CommandLineParameterException(final Throwable cause, final boolean usingGui) {
		super(cause, usingGui);
	}

	CommandLineParameterException(final String message, final Throwable cause, final boolean usingGui) {
		super(message, cause, usingGui);
	}
}
