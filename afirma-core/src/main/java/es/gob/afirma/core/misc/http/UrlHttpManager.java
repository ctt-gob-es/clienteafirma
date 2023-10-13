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

/**
 * Clase abstracta para la lectura y env&iacute;o de datos a URL remotas.
 * @author Carlos Gamuci
 */
public interface UrlHttpManager {

	/**
	 * Lee una URL HTTP o HTTPS.
	 * @param url URL a leer.
	 * @param method M&eacute;todo HTTP.
	 * @return Contenido de la URL.
	 * @throws IOException Si no se puede leer la URL.
	 */
	byte[] readUrl(String url, UrlHttpMethod method) throws IOException;

	/**
	 * Lee una URL HTTP o HTTPS.
	 * @param url URL a leer.
	 * @param method M&eacute;todo HTTP.
	 * @param processor Procesador de errores en la conexi&oacute;n.
	 * @return Contenido de la URL.
	 * @throws IOException Si no se puede leer la URL.
	 */
	byte[] readUrl(String url, UrlHttpMethod method, HttpErrorProcessor processor) throws IOException;

	/**
	 * Lee una URL HTTP o HTTPS.
	 * @param url URL a leer.
	 * @param method M&eacute;todo HTTP.
	 * @param processor Procesador de errores en la conexi&oacute;n.
	 * @param sslConfig Configuraci&oacute;n para las conexiones SSL.
	 * @return Contenido de la URL.
	 * @throws IOException Si no se puede leer la URL.
	 */
	byte[] readUrl(String url, UrlHttpMethod method, HttpErrorProcessor processor, SSLConfig sslConfig)
			throws IOException;

	/**
	 * Lee una URL HTTP o HTTPS.
	 * @param url URL a leer.
	 * @param timeout Tiempo m&aacute;ximo en milisegundos para la conexi&oacute;n. El valor 0
	 * indica tiempo infinito y -1 el por defecto de Java.
	 * @param method M&eacute;todo HTTP.
	 * @param requestProperties Propiedades a usar en la cabecera de la petici&oacute;n HTTP.
	 * @return Contenido de la URL.
	 * @throws IOException Si no se puede leer la URL.
	 */
	byte[] readUrl(String url,
			int timeout,
			UrlHttpMethod method,
			Properties requestProperties) throws IOException;

	/**
	 * Lee una URL HTTP o HTTPS estableciendo un tiempo m&aacute;ximo para la comunicaci&oacute;n.
	 * Los par&aacute;metros se indican en la URL.
	 * @param url URL a leer
	 * @param timeout Tiempo m&aacute;ximo en milisegundos para la conexi&oacute;n. El valor 0
	 * indica tiempo infinito y -1 el por defecto de Java.
	 * @param contentType Content-Type a insertar en la cabecera de la petici&oacute;n HTTP.
	 * @param accept Tipo de contenido que se acepta como respuesta.
	 * @param method M&eacute;todo HTTP.
	 * @return Contenido de la URL
	 * @throws IOException Si no se puede leer la URL.
	 */
	byte[] readUrl(String url,
			int timeout,
			String contentType,
			String accept,
			UrlHttpMethod method) throws IOException;

	/**
	 * Lee una URL HTTP o HTTPS.
	 * @param url URL a leer
	 * @param timeout Tiempo m&aacute;ximo en milisegundos para la conexi&oacute;n. El valor 0
	 * indica tiempo infinito y -1 el por defecto de Java.
	 * @param contentType Content-Type a insertar en la cabecera de la petici&oacute;n HTTP.
	 * @param accept Tipo de contenido que se acepta como respuesta.
	 * @param method M&eacute;todo HTTP.
	 * @param processor Procesador de errores en la conexi&oacute;n.
	 * @return Contenido de la URL.
	 * @throws IOException Si no se puede leer la URL.
	 */
	byte[] readUrl(String url,
			int timeout,
			String contentType,
			String accept,
			UrlHttpMethod method,
			HttpErrorProcessor processor) throws IOException;

	/**
	 * Lee una URL HTTP o HTTPS.
	 * @param url URL a leer
	 * @param timeout Tiempo m&aacute;ximo en milisegundos para la conexi&oacute;n. El valor 0
	 * indica tiempo infinito y -1 el por defecto de Java.
	 * @param contentType Content-Type a insertar en la cabecera de la petici&oacute;n HTTP.
	 * @param accept Tipo de contenido que se acepta como respuesta.
	 * @param method M&eacute;todo HTTP.
	 * @param processor Procesador de errores en la conexi&oacute;n.
	 * @param sslConfig Configuraci&oacute;n para las conexiones SSL.
	 * @return Contenido de la URL.
	 * @throws IOException Si no se puede leer la URL.
	 */
	byte[] readUrl(String url,
			int timeout,
			String contentType,
			String accept,
			UrlHttpMethod method,
			HttpErrorProcessor processor,
			SSLConfig sslConfig) throws IOException;

	/**
	 * Lee una URL HTTP o HTTPS.
	 * @param url URL a leer
	 * @param timeout Tiempo m&aacute;ximo en milisegundos para la conexi&oacute;n. El valor 0
	 * indica tiempo infinito y -1 el por defecto de Java.
	 * @param method M&eacute;todo HTTP.
	 * @param requestProperties Propiedades a usar en la cabecera de la petici&oacute;n HTTP.
	 * @param httpProcessor Procesador de errores en la conexi&oacute;n.
	 * @return Contenido de la URL.
	 * @throws IOException Si no se puede leer la URL.
	 */
	byte[] readUrl(String url,
			int timeout,
			UrlHttpMethod method,
			Properties requestProperties,
			HttpErrorProcessor httpProcessor) throws IOException;


	/**
	 * Lee una URL HTTP o HTTPS.
	 * @param url URL a leer
	 * @param timeout Tiempo m&aacute;ximo en milisegundos para la conexi&oacute;n. El valor 0
	 * indica tiempo infinito y -1 el por defecto de Java.
	 * @param method M&eacute;todo HTTP.
	 * @param requestProperties Propiedades a usar en la cabecera de la petici&oacute;n HTTP.
	 * @param httpProcessor Procesador de errores en la conexi&oacute;n.
	 * @param sslConfig Configuraci&oacute;n para las conexiones SSL.
	 * @return Contenido de la URL.
	 * @throws IOException Si no se puede leer la URL.
	 */
	byte[] readUrl(String url,
			int timeout,
			UrlHttpMethod method,
			Properties requestProperties,
			HttpErrorProcessor httpProcessor,
			SSLConfig sslConfig) throws IOException;
}
