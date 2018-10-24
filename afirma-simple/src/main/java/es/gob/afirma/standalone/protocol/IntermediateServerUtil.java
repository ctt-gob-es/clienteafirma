/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

import java.io.IOException;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.misc.http.UrlHttpMethod;

/**
 * Clases para la interaccion con el servidor intermedio.
 */
public class IntermediateServerUtil {

	private static final String METHOD_OP_PUT = "put"; //$NON-NLS-1$
	private static final String SYNTAX_VERSION = "1_0"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static Object semaphore = null;

	/** Env&iacute;a datos al servidor intermedio.
	 * @param data Buffer con los datos a enviar.
	 * @param storageServiceUrl URL del servicio de guardado.
	 * @param id Identificador a asignar a los datos a subir al servidor.
	 * @throws IOException Si hay problemas enviando los datos. */
	public static void sendData(final StringBuilder data, final String storageServiceUrl, final String id) throws IOException {

		final StringBuilder url = new StringBuilder(storageServiceUrl)
		.append("?op=").append(METHOD_OP_PUT) //$NON-NLS-1$
		.append("&v=").append(SYNTAX_VERSION) //$NON-NLS-1$
		.append("&id=").append(id) //$NON-NLS-1$
		.append("&dat=").append(data.toString()); //$NON-NLS-1$

		send(url);
	}

	/** Env&iacute;a datos al servidor intermedio.
	 * @param data Buffer con los datos a enviar.
	 * @param storageServiceUrl URL del servicio de guardado.
	 * @param id Identificador a asignar a los datos a subir al servidor.
	 * @throws IOException Si hay problemas enviando los datos. */
	public static void sendData(final String data, final String storageServiceUrl, final String id) throws IOException {

		final StringBuilder url = new StringBuilder(storageServiceUrl)
		.append("?op=").append(METHOD_OP_PUT) //$NON-NLS-1$
		.append("&v=").append(SYNTAX_VERSION) //$NON-NLS-1$
		.append("&id=").append(id) //$NON-NLS-1$
		.append("&dat=").append(data); //$NON-NLS-1$

		send(url);
	}

	/** Env&iacute;a datos al servidor intermedio.
	 * @param url URL del servicio de guardado.
	 * @throws IOException Si hay problemas durante el env&iacute;o. */
	private static void send(final StringBuilder url) throws IOException {

		// Llamamos al servicio para guardar los datos
		final byte[] result = UrlHttpManagerFactory.getInstalledManager().readUrl(url.toString(), UrlHttpMethod.POST);

		LOGGER.info("Resultado: " + new String(result)); //$NON-NLS-1$
	}

	/**
	 * Obtiene el sem&aacute;foro que gestiona el acceso para el envio de datos
	 * al servidor intermedio. El uso de este sem&aacute;foro antes de las llamadas
	 * para el env&iacute;o de datos, garantiza que no se env&iacute;e nada al
	 * servidor cuando ya deb&iacute;o dejar de enviar.
	 * @return Instancia del sem&aacute;foro.
	 */
	public static Object getUniqueSemaphoreInstance() {
		if (semaphore == null) {
			semaphore = new Object();
		}
		return semaphore;
	}
}
