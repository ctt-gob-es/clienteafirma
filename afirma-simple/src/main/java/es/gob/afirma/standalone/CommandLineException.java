/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone;

/** Excepcion que encapsula cualquier excepcion que se lance durante la operativa en modo consola.
 * @author Carlos Gamuci */
public class CommandLineException extends Exception {

	private static final long serialVersionUID = 3756189019018723691L;

	private boolean usingGui = false;

	CommandLineException(final String message) {
		super(message);
	}

	CommandLineException(final Throwable cause) {
		super(cause);
	}

	CommandLineException(final String message, final Throwable cause) {
		super(message, cause);
	}

	CommandLineException(final String message, final boolean usingGui) {
		super(message);
		this.usingGui = usingGui;
	}

	CommandLineException(final Throwable cause, final boolean usingGui) {
		super(cause);
		this.usingGui = usingGui;
	}

	CommandLineException(final String message, final Throwable cause, final boolean usingGui) {
		super(message, cause);
		this.usingGui = usingGui;
	}

	/**
	 * Indica si se ha configurado el uso de interfaz gr&aacute;fica o no.
	 * @return {@code true} si el comando requiere usar interfaz gr&aacute;fica,
	 * {@code false} en caso contrario.
	 */
	public boolean isUsingGui() {
		return this.usingGui;
	}
}
