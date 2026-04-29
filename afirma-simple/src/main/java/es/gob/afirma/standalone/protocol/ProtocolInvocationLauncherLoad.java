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
import es.gob.afirma.core.ErrorCode;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.protocol.ProtocolVersion;
import es.gob.afirma.core.misc.protocol.UrlParametersToLoad;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.DesktopUtil;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.SimpleErrorCode;
import es.gob.afirma.standalone.so.macos.MacUtils;
import es.gob.afirma.standalone.ui.ProgressInfoDialogManager;

final class ProtocolInvocationLauncherLoad {

	/**
	 * Character separator between file name and file data: filename|filedata64
	 */
	private static final char LOAD_SEPARATOR = ':';

	private static final char MULTILOAD_SEPARATOR = '|';

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$


	private ProtocolInvocationLauncherLoad() {
		// No instanciable
	}

	/** Procesa un lote de firma en invocaci&oacute;n por protocolo.
	 * @param options Par&aacute;metros de la operaci&oacute;n.
	 * @param protocolVersion Versi&oacute;n del protocolo de comunicaci&oacute;n.
	 * @return Datos cargados o mensaje de error.
	 * @throws SocketOperationException Si hay errores en la
	 *                                  comunicaci&oacute;n por <i>socket</i> local. */
	static String processLoad(final UrlParametersToLoad options,
			final ProtocolVersion protocolVersion) throws SocketOperationException {

        // Comprobamos si soportamos la version del protocolo indicada
		if (!ProtocolInvocationLauncher.isCompatibleWith(protocolVersion)) {
			LOGGER.severe(String.format("Version de protocolo no soportada (%1s). Hay que actualizar la aplicacion.", //$NON-NLS-1$
					protocolVersion.toString()));
			throw new SocketOperationException(SimpleErrorCode.Request.UNSUPPORTED_PROTOCOL_VERSION);
		}

        // Comprobamos si se exige una version minima del Cliente
        if (options.getMinimumClientVersion() != null) {
        	final String minimumRequestedVersion = options.getMinimumClientVersion();
        	final Version requestedVersion = new Version(minimumRequestedVersion);
        	if (requestedVersion.greaterThan(SimpleAfirma.getVersion())) {
   				throw new SocketOperationException(SimpleErrorCode.Functional.MINIMUM_VERSION_NON_SATISTIED);
        	}
        }

		final File[] selectedDataFiles;

		// Invocamos la factoria de elementos de interfaz para para cargar el
		// dialogo de seleccion de fichero
		try {
			if (Platform.OS.MACOSX.equals(Platform.getOS())) {
				MacUtils.focusApplication();
			}
			ProgressInfoDialogManager.hideProgressDialog();
			selectedDataFiles = AOUIFactory.getLoadFiles(options.getTitle(),
					options.getFilepath(),
					null,
					options.getExtensions() != null ? options.getExtensions().split(",") : null, //$NON-NLS-1$
					options.getDescription(),
					false,
					options.getMultiload(),
					DesktopUtil.getDefaultDialogsIcon(),
					null);

		} catch (final AOCancelledOperationException e) {
			throw e;
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
			final ErrorCode errorCode = SimpleErrorCode.Internal.CANT_LOAD_FILE;
			throw new SocketOperationException(errorCode);
		}

		return dataToSend.toString();
	}

}
