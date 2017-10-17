/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.massive;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

/** Gestor de registro por defecto para la firma masiva, en un fichero de texto. */
final class DefaultLogHandler extends LogHandler {

	private final OutputStream os;

	DefaultLogHandler(final OutputStream os) {
		this.os = os;
	}

	@Override
	public void addLog(final int level, final String msg, final String inputData, final String outputSign) {
		try {
			this.os.write(("\r\n" + getLevel(level) + ": " + msg + " - " + (outputSign != null ? outputSign : inputData != null ? inputData : "")).getBytes()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").warning("No se pudo insertar una entrada en el log de la operacion masiva: " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	@Override
	public void close(final Properties params) throws IOException {
		final String warningsCount = params != null ? params.getProperty("warningsCount", "0") : "0"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		final String errorsCount = params != null ? params.getProperty("errorsCount", "0") : "0"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		this.os.write(("\r\n\r\n" + MassiveSignMessages.getString("DirectorySignatureHelper.25") + ": " + warningsCount).getBytes()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		this.os.write(("\r\n" + MassiveSignMessages.getString("DirectorySignatureHelper.26") + ": " + errorsCount).getBytes()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}

	private static String getLevel(final int level) {
		if (level == Level.INFO.intValue()) {
			return Level.INFO.getName();
		}
		else if (level == Level.WARNING.intValue()) {
			return Level.WARNING.getName();
		}
		else {
			return Level.SEVERE.getName();
		}
	}
}
