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

import es.gob.afirma.core.misc.http.UrlHttpMethod;
import es.gob.afirma.standalone.HttpManager;

/**
 * Clases para la interaccion con el servidor intermedio.
 */
public class IntermediateServerUtil {

	private static final String METHOD_OP_PUT = "put"; //$NON-NLS-1$
	private static final String METHOD_OP_GET = "get"; //$NON-NLS-1$

	private static final String SYNTAX_VERSION = "1_0"; //$NON-NLS-1$

	private static Object semaphore = null;

	/** Env&iacute;a datos al servidor intermedio.
	 * @param data Buffer con los datos a enviar.
	 * @param storageServiceUrl URL del servicio de guardado.
	 * @param id Identificador a asignar a los datos a subir al servidor.
	 * @throws IOException Si hay problemas enviando los datos. */
	public static void sendData(final CharSequence data, final String storageServiceUrl, final String id) throws IOException {

		final StringBuilder url = new StringBuilder(storageServiceUrl)
		.append("?op=").append(METHOD_OP_PUT) //$NON-NLS-1$
		.append("&v=").append(SYNTAX_VERSION) //$NON-NLS-1$
		.append("&id=").append(id) //$NON-NLS-1$
		.append("&dat=").append(data); //$NON-NLS-1$

		send(url);
	}

	/** Recupera datos del servidor intermedio.
	 * @param retrieveServiceUrl URL del servicio de recuperaci&oacute;n.
	 * @param id Identificador a asignar a los datos a subir al servidor.
	 * @return Datos recuperados.
	 * @throws IOException Si hay problemas recuperando los datos. */
	public static byte[] retrieveData(final String retrieveServiceUrl, final String id) throws IOException {
		final StringBuilder url = new StringBuilder(retrieveServiceUrl)
				.append("?op=").append(METHOD_OP_GET) //$NON-NLS-1$
				.append("&v=").append(SYNTAX_VERSION) //$NON-NLS-1$
				.append("&id=").append(id); //$NON-NLS-1$

		return send(url);
	}

	/**
	 * Hace la llamada al servidor intermedio.
	 * @param url URL del servicio de guardado.
	 * @return Respuesta del env&iacute;o.
	 * @throws IOException Si hay problemas durante el env&iacute;o. */
	private static byte[] send(final StringBuilder url) throws IOException {
		return new HttpManager().readUrl(url.toString(), UrlHttpMethod.POST);
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
