/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

import java.util.logging.Logger;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.protocol.UrlParametersToSave;
import es.gob.afirma.core.ui.AOUIFactory;

final class ProtocolInvocationLauncherSave {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String RESULT_OK = "OK"; //$NON-NLS-1$
	private static final String RESULT_CANCEL = "CANCEL"; //$NON-NLS-1$

	private ProtocolInvocationLauncherSave() {
		// No instanciable
	}

	static String processSave(final UrlParametersToSave  options, final boolean bySocket) throws SocketOperationException {

		if (!ProtocolInvocationLauncher.MAX_PROTOCOL_VERSION_SUPPORTED.support(options.getMinimumVersion())) {
			LOGGER.severe(String.format("Version de protocolo no soportada (%1s). Version actual: %s2. Hay que actualizar la aplicacion.", options.getMinimumVersion(), ProtocolInvocationLauncher.MAX_PROTOCOL_VERSION_SUPPORTED)); //$NON-NLS-1$
			ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_21);
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_21);
		}

		try {
			if (Platform.OS.MACOSX.equals(Platform.getOS())) {
				ServiceInvocationManager.focusApplication();
			}
			AOUIFactory.getSaveDataToFile(
				options.getData(),
				options.getTitle(),
				null,
				options.getFileName(),
				options.getExtensions() != null ? new String[] { options.getExtensions() } : null,
				options.getFileTypeDescription(),
				null
			);
		}
		catch(final AOCancelledOperationException e) {
			LOGGER.severe("Operacion cancelada por el usuario" + e); //$NON-NLS-1$
			if (!bySocket){
				throw new SocketOperationException(RESULT_CANCEL);
			}
			return RESULT_CANCEL;
		}
		catch (final Exception e) {
			LOGGER.severe("Error en el guardado de datos: " + e); //$NON-NLS-1$
			ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_05);
			if (!bySocket){
				throw new SocketOperationException(
					ProtocolInvocationLauncherErrorManager.SAF_09
				);
			}
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_05);
		}

		if (options.getStorageServletUrl() != null) {
			// Enviamos la firma cifrada al servicio remoto de intercambio
			try {
				IntermediateServerUtil.sendData(RESULT_OK, options.getStorageServletUrl().toString(), options.getId());
			}
			catch (final Exception e) {
				LOGGER.severe("Error al enviar los datos al servidor: " + e); //$NON-NLS-1$
				ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_11);
				return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_11);
			}
		}
		else {
			LOGGER.info(
				"Se omite el envio por red del resultado por no haberse proporcionado una URL de destino" //$NON-NLS-1$
			);
		}

		return RESULT_OK;
	}

	public static String getResultOk() {
		return RESULT_OK;
	}

	public static String getResultCancel() {
		return RESULT_CANCEL;
	}

}
