/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone;

enum CommandLineCommand {
	LIST("listaliases"), //$NON-NLS-1$
	SIGN("sign"), //$NON-NLS-1$
	COSIGN("cosign"), //$NON-NLS-1$
	COUNTERSIGN("countersign"), //$NON-NLS-1$
	MASSIVE("massive"), //$NON-NLS-1$
	VERIFY("verify"), //$NON-NLS-1$
	CREATEHASH("createdigest"), //$NON-NLS-1$
	CHECKHASH("checkdigest"), //$NON-NLS-1$
	BATCHSIGN("batchsign"); //$NON-NLS-1$

	private String op;

	private CommandLineCommand(final String op) {
		this.op = op;
	}

	public String getOp() {
		return this.op;
	}

	/** Obtiene el comando que se responde a la cadena indicada.
	 * @param op Texto identificador del comando.
	 * @return Comando correspondiente o {@code null} si no hay ninguno. */
	public static CommandLineCommand parse(final String op) {
		for (final CommandLineCommand c : CommandLineCommand.values()) {
			if (c.getOp().equals(op)) {
				return c;
			}
		}
		return null;
	}

}