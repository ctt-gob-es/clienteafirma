/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.core.misc.http;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.Proxy;
import java.net.URL;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.StringTokenizer;
import java.util.logging.Logger;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSession;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import es.gob.afirma.core.misc.AOUtil;

/** Clase para la lectura y env&iacute;o de datos a URL remotas.
 * @author Carlos Gamuci */
public class UrlHttpManagerImpl implements UrlHttpManager {

	/** Tiempo de espera por defecto para descartar una conexi&oacute;n HTTP. */
	public static final int DEFAULT_TIMEOUT = -1;

	private static final String HTTPS = "https"; //$NON-NLS-1$

	private static final HostnameVerifier DEFAULT_HOSTNAME_VERIFIER = HttpsURLConnection.getDefaultHostnameVerifier();
	private static final SSLSocketFactory DEFAULT_SSL_SOCKET_FACTORY = HttpsURLConnection.getDefaultSSLSocketFactory();

	protected UrlHttpManagerImpl() {
		// Instanciacion "default"
	}

	private static final TrustManager[] DUMMY_TRUST_MANAGER = new TrustManager[] {
		new X509TrustManager() {
			@Override
			public java.security.cert.X509Certificate[] getAcceptedIssuers() {
				return null;
			}
			@Override
			public void checkClientTrusted(final X509Certificate[] certs, final String authType) { /* No hacemos nada */ }
			@Override
			public void checkServerTrusted(final X509Certificate[] certs, final String authType) {  /* No hacemos nada */  }

		}
	};

	/** Lee una URL HTTP o HTTPS por POST si se indican par&aacute;metros en la URL y por GET en caso contrario.
	 * En HTTPS no se hacen comprobaciones del certificado servidor.
	 * @param url URL a leer
	 * @return Contenido de la URL
	 * @throws IOException Si no se puede leer la URL */
	@Override
	public byte[] readUrlByPost(final String url) throws IOException {
		return readUrlByPost(url, DEFAULT_TIMEOUT, "application/x-www-form-urlencoded"); //$NON-NLS-1$
	}

	/** Lee una URL HTTP o HTTPS por POST si se indican par&aacute;metros en la URL y por GET en caso contrario.
	 * En HTTPS no se hacen comprobaciones del certificado servidor.
	 * @param url URL a leer
	 * @param timeout Tiempo m&aacute;ximo en milisegundos que se debe esperar por la respuesta. Un timeout de 0
	 * se interpreta como un timeout infinito. Si se indica -1, se usar&aacute; el por defecto de Java.
	 * @return Contenido de la URL
	 * @throws IOException Si no se puede leer la URL */
	@Override
	public byte[] readUrlByPost(final String url, final int timeout, final String contentType) throws IOException {
		if (url == null) {
			throw new IllegalArgumentException("La URL a leer no puede ser nula"); //$NON-NLS-1$
		}

		// Si la URL no tiene parametros la leemos por GET
		if (!url.contains("?")) { //$NON-NLS-1$
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
				"Se ha pedido una peticion POST sin parametros, pero se realizara por GET" //$NON-NLS-1$
			);
			return readUrlByGet(url);
		}

		final StringTokenizer st = new StringTokenizer(url, "?"); //$NON-NLS-1$
		final String request = st.nextToken();
		final String urlParameters = st.nextToken();

		final URL uri = new URL(request);

		if (uri.getProtocol().equals(HTTPS)) {
			try {
				disableSslChecks();
			}
			catch(final Exception e) {
				Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
					"No se ha podido ajustar la confianza SSL, es posible que no se pueda completar la conexion: " + e //$NON-NLS-1$
				);
			}
		}

		final HttpURLConnection conn = (HttpURLConnection) uri.openConnection(Proxy.NO_PROXY);
		conn.setRequestMethod("POST"); //$NON-NLS-1$

		conn.addRequestProperty("Accept", "*/*"); //$NON-NLS-1$ //$NON-NLS-2$
		conn.addRequestProperty("Connection", "keep-alive"); //$NON-NLS-1$ //$NON-NLS-2$
		if (contentType != null) {
			conn.addRequestProperty("Content-type", contentType); //$NON-NLS-1$
		}
		conn.addRequestProperty("Host", uri.getHost()); //$NON-NLS-1$
		conn.addRequestProperty("Origin", uri.getProtocol() +  "://" + uri.getHost()); //$NON-NLS-1$ //$NON-NLS-2$

		if (timeout != DEFAULT_TIMEOUT) {
			conn.setConnectTimeout(timeout);
			conn.setReadTimeout(timeout);
		}

		conn.setDoOutput(true);

		final OutputStreamWriter writer = new OutputStreamWriter(conn.getOutputStream());

		writer.write(urlParameters);
		writer.flush();
		writer.close();

		conn.connect();
		final int resCode = conn.getResponseCode();
		final String statusCode = Integer.toString(resCode);
		if (statusCode.startsWith("4") || statusCode.startsWith("5")) { //$NON-NLS-1$ //$NON-NLS-2$
			if (uri.getProtocol().equals(HTTPS)) {
				enableSslChecks();
			}
			throw new HttpError(resCode);
		}

		final InputStream is = conn.getInputStream();
		final byte[] data = AOUtil.getDataFromInputStream(is);
		is.close();

		if (uri.getProtocol().equals(HTTPS)) {
			enableSslChecks();
		}

		return data;
	}

	/** Lee una URL HTTP o HTTPS por GET. En HTTPS no se hacen comprobaciones del certificado servidor.
	 * @param url URL a leer
	 * @return Contenido de la URL
	 * @throws IOException Si no se puede leer la URL */
	@Override
	public byte[] readUrlByGet(final String url) throws IOException {
		final URL uri = new URL(url);
		if (uri.getProtocol().equals("https")) { //$NON-NLS-1$
			try {
				disableSslChecks();
			}
			catch(final Exception e) {
				Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
					"No se ha podido ajustar la confianza SSL, es posible que no se pueda completar la conexion: " + e //$NON-NLS-1$
				);
			}
		}
		final InputStream is = uri.openStream();
		final byte[] data = AOUtil.getDataFromInputStream(is);
		is.close();
		if (uri.getProtocol().equals("https")) { //$NON-NLS-1$
			enableSslChecks();
		}
		return data;
	}

	/** Habilita las comprobaciones de certificados en conexiones SSL dej&aacute;ndolas con su
	 * comportamiento por defecto. */
	public static void enableSslChecks() {
		HttpsURLConnection.setDefaultSSLSocketFactory(DEFAULT_SSL_SOCKET_FACTORY);
		HttpsURLConnection.setDefaultHostnameVerifier(DEFAULT_HOSTNAME_VERIFIER);
	}

	/** Deshabilita las comprobaciones de certificados en conexiones SSL, acept&aacute;dose entonces
	 * cualquier certificado.
	 * @throws KeyManagementException Si hay problemas en la gesti&oacute;n de claves SSL.
	 * @throws NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario. */
	public static void disableSslChecks() throws KeyManagementException, NoSuchAlgorithmException {
		final SSLContext sc = SSLContext.getInstance("SSL"); //$NON-NLS-1$
		sc.init(null, DUMMY_TRUST_MANAGER, new java.security.SecureRandom());
		HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory());
		HttpsURLConnection.setDefaultHostnameVerifier(
			new HostnameVerifier() {
				@Override
				public boolean verify(final String hostname, final SSLSession session) {
					return true;
				}
			}
		);
	}

}
