/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.misc.http;

import java.io.IOException;
import java.util.Properties;

/** Clase abstracta para la lectura y env&iacute;o de datos a URL remotas.
 * @author Carlos Gamuci */
public interface UrlHttpManager {

	/** Lee una URL HTTP o HTTPS estableciendo un tiempo m&aacute;ximo para la comunicaci&oacute;n.
	 * Los par&aacute;metros se indican en la URL.
	 * @param url URL a leer
	 * @param timeout Timeout.
	 * @param contentType Content-Type a insertar en la cabecera de la petici&oacute;n HTTP.
	 * @param accept Tipo de contenido que se acepta como respuesta.
	 * @param method M&eacute;todo HTTP.
	 * @return Contenido de la URL
	 * @throws IOException Si no se puede leer la URL */
	byte[] readUrl(final String url,
			       final int timeout,
			       final String contentType,
			       final String accept,
			       final UrlHttpMethod method) throws IOException;

	/** Lee una URL HTTP o HTTPS.
	 * @param url URL a leer.
	 * @param method M&eacute;todo HTTP.
	 * @return Contenido de la URL.
	 * @throws IOException Si no se puede leer la URL */
	byte[] readUrl(final String url, final UrlHttpMethod method) throws IOException;

	/** Lee una URL HTTP o HTTPS.
	 * @param url URL a leer.
	 * @param timeout Timeout.
	 * @param method M&eacute;todo HTTP.
	 * @param requestProperties Propiedades a usar en la cabecera de la petici&oacute;n HTTP.
	 * @return Contenido de la URL.
	 * @throws IOException Si no se puede leer la URL */
	byte[] readUrl(final String url,
		           final int timeout,
			       final UrlHttpMethod method,
			       final Properties requestProperties) throws IOException;
}
