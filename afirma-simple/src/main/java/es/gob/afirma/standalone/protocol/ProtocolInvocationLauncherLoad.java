/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.logging.Logger;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.protocol.UrlParametersToLoad;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.AutoFirmaUtil;

final class ProtocolInvocationLauncherLoad {

	/**
	 * Character separator between file name and file data: filename|filedata64
	 */
	private static final char LOAD_SEPARATOR = ':';

	private static final char MULTILOAD_SEPARATOR = '|';

	private static final String RESULT_CANCEL = "CANCEL"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$


	private ProtocolInvocationLauncherLoad() {
		// No instanciable
	}

	static String processLoad(final UrlParametersToLoad options, final boolean bySocket) throws SocketOperationException {


		if (!ProtocolInvocationLauncher.MAX_PROTOCOL_VERSION_SUPPORTED.support(options.getMinimumVersion())) {
			LOGGER.severe(String.format("Version de protocolo no soportada (%1s). Version actual: %s2. Hay que actualizar la aplicacion.", options.getMinimumVersion(), ProtocolInvocationLauncher.MAX_PROTOCOL_VERSION_SUPPORTED)); //$NON-NLS-1$
			ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.ERROR_UNSUPPORTED_PROCEDURE);
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_UNSUPPORTED_PROCEDURE);
		}

		final File[] selectedDataFiles;

		// Invocamos la factoria de elementos de interfaz para para cargar el
		// dialogo de seleccion de fichero
		try {
			if (Platform.OS.MACOSX.equals(Platform.getOS())) {
				ServiceInvocationManager.focusApplication();
			}

			selectedDataFiles = AOUIFactory.getLoadFiles(options.getTitle(),
					options.getFilepath(),
					null,
					options.getExtensions() != null ? options.getExtensions().split(",") : null, //$NON-NLS-1$
					options.getDescription(),
					false,
					options.getMultiload(),
					AutoFirmaUtil.getDefaultDialogsIcon(),
					null);

		} catch (final AOCancelledOperationException e) {
			LOGGER.info("carga de datos de firma cancelada por el usuario: " + e); //$NON-NLS-1$
			if (!bySocket) {
				throw new SocketOperationException(getResultCancel());
			}
			return getResultCancel();
		}

		// Preparamos el buffer para enviar el resultado
		final StringBuilder dataToSend = new StringBuilder();

		// Intentamos extraer la informacion de cada objecto File obtenidos en el
		// paso anterior
		try {

			for (int i = 0; i < selectedDataFiles.length; i++) {

				final byte[] data;
				try (final InputStream fis = new FileInputStream(selectedDataFiles[i]);
						final InputStream bis = new BufferedInputStream(fis);) {
					data = AOUtil.getDataFromInputStream(bis);
				}

				if (data == null) {
					throw new IOException("La lectura de datos para cargar ha devuelto un nulo"); //$NON-NLS-1$
				}

				dataToSend.append(selectedDataFiles[i].getName());
				dataToSend.append(LOAD_SEPARATOR);
				dataToSend.append(Base64.encode(data));

				if (i < selectedDataFiles.length - 1) {
					dataToSend.append(MULTILOAD_SEPARATOR);
				}

			}
		} catch (final Exception e) {
			LOGGER.severe("Error en la lectura de los datos a cargar: " + e); //$NON-NLS-1$
			ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.ERROR_CANNOT_LOAD_DATA);
			if (!bySocket) {
				throw new SocketOperationException(ProtocolInvocationLauncherErrorManager.ERROR_CANNOT_LOAD_DATA);
			}
			return ProtocolInvocationLauncherErrorManager
					.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_CANNOT_LOAD_DATA);
		}

		return dataToSend.toString();
	}

	public static String getResultCancel() {
		return RESULT_CANCEL;
	}

}
