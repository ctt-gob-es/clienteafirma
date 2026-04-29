/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

import java.util.Collections;
import java.util.logging.Logger;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.protocol.ProtocolVersion;
import es.gob.afirma.core.misc.protocol.UrlParametersToSave;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.ui.GenericFileFilter;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.SimpleErrorCode;
import es.gob.afirma.standalone.so.macos.MacUtils;
import es.gob.afirma.standalone.ui.ProgressInfoDialogManager;

final class ProtocolInvocationLauncherSave {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String RESULT_OK = "OK"; //$NON-NLS-1$

	private ProtocolInvocationLauncherSave() {
		// No instanciable
	}

	/** Procesa una peticion de guardado de datos en disco en invocaci&oacute;n
	 * por protocolo.
	 * @param options Par&aacute;metros de la operaci&oacute;n.
	 * @param protocolVersion Versi&oacute;n del protocolo de comunicaci&oacute;n.
	 * @return La cadena OK o una cadena descriptiva con el mensaje de error.
	 * @throws SocketOperationException Si hay errores en la
	 *                                  comunicaci&oacute;n por <i>socket</i> local. */
	static String processSave(final UrlParametersToSave  options,
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

		try {
			if (Platform.OS.MACOSX.equals(Platform.getOS())) {
				MacUtils.focusApplication();
			}
			ProgressInfoDialogManager.hideProgressDialog();
			AOUIFactory.getSaveDataToFile(
				options.getData(),
				options.getTitle(),
				null,
				options.getFileName(),
				Collections.singletonList(
					new GenericFileFilter(
						options.getExtensions() != null ? new String[] { options.getExtensions() } : null,
						options.getFileTypeDescription()
					)
				),
				null
			);
		}
		catch(final AOCancelledOperationException e) {
			throw e;
		}
		catch (final Exception e) {
			throw new SocketOperationException(e, SimpleErrorCode.Internal.CANT_SAVE_FILE);
		}

		return RESULT_OK;
	}

	public static String getResultOk() {
		return RESULT_OK;
	}

}
