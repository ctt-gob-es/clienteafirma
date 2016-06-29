/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.android.network;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.net.Proxy;
import java.net.URL;
import java.net.URLConnection;
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
import es.gob.afirma.core.misc.http.UrlHttpManager;
import es.gob.afirma.core.misc.http.UrlHttpMethod;

/** Implementacion de ua clase para la lectura del contenido de una URL.
 * @author Carlos Gamuci */
public final class AndroidUrlHttpManager implements UrlHttpManager {

	/** Tiempo de espera por defecto antes de descartar una conexi&oacute;n. */
	public static final int DEFAULT_TIMEOUT = -1;

	/** Esquema del protocolo HTTPS. */
	public static final String HTTPS = "https"; //$NON-NLS-1$

	private static final HostnameVerifier DEFAULT_HOSTNAME_VERIFIER = HttpsURLConnection.getDefaultHostnameVerifier();
	private static final SSLSocketFactory DEFAULT_SSL_SOCKET_FACTORY = HttpsURLConnection.getDefaultSSLSocketFactory();

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

	private static byte[] readUrlByPost(final String url) throws IOException {
		return readUrlByPost(url, DEFAULT_TIMEOUT, "application/x-www-form-urlencoded"); //$NON-NLS-1$
	}

	private static byte[] readUrlByPost(final String url, final int timeout, final String contentType) throws IOException {
		if (url == null) {
			throw new IllegalArgumentException("La URL a leer no puede ser nula"); //$NON-NLS-1$
		}

		// Si la URL no tiene parametros la leemos por GET
		if (!url.contains("?")) { //$NON-NLS-1$
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

		final URLConnection conn = uri.openConnection(Proxy.NO_PROXY);

		conn.setDoOutput(true);
		if (timeout != DEFAULT_TIMEOUT) {
			conn.setConnectTimeout(timeout);
			conn.setReadTimeout(timeout);
		}

		final OutputStreamWriter writer = new OutputStreamWriter(conn.getOutputStream());

		writer.write(urlParameters);
		writer.flush();
		writer.close();

		final InputStream is = conn.getInputStream();
		final byte[] data = AOUtil.getDataFromInputStream(is);
		is.close();

		if (uri.getProtocol().equals(HTTPS)) {
			enableSslChecks();
		}

		return data;
	}

	private static byte[] readUrlByGet(final String url) throws IOException {
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
		if (uri.getProtocol().equals(HTTPS)) {
			enableSslChecks();
		}
		return data;
	}

	/** Hablita las comprobaciones de seguridad SSL con sus valores por defecto. */
	public static void enableSslChecks() {
		HttpsURLConnection.setDefaultSSLSocketFactory(DEFAULT_SSL_SOCKET_FACTORY);
		HttpsURLConnection.setDefaultHostnameVerifier(DEFAULT_HOSTNAME_VERIFIER);
	}

	/** Deshablita las comprobaciones de seguridad SSL.
	 * @throws KeyManagementException Si hay problemas en la gesti&oacute;n de claves.
	 * @throws NoSuchAlgorithmException Si no se soporta alg&uacute;n algoritmo necesario. */
	public static void disableSslChecks() throws KeyManagementException, NoSuchAlgorithmException {
		final SSLContext sc = SSLContext.getInstance("SSL"); //$NON-NLS-1$
		sc.init(null, DUMMY_TRUST_MANAGER, new java.security.SecureRandom());
		HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory());
		HttpsURLConnection.setDefaultHostnameVerifier(new HostnameVerifier() {
			@Override
			public boolean verify(final String hostname, final SSLSession session) {
				return true;
			}
		});
	}

	@Override
	public byte[] readUrl(String url, int timeout, String contentType, String accept, UrlHttpMethod method)
			throws IOException {
		if(method.compareTo(UrlHttpMethod.GET) == 1) {
			return readUrlByGet(url);
		}
		return readUrlByPost(url, timeout, contentType);
	}

	@Override
	public byte[] readUrl(String url, UrlHttpMethod method) throws IOException {
		if(method.compareTo(UrlHttpMethod.GET) == 1) {
			return readUrlByGet(url);
		}
		return readUrlByPost(url);
	}
}
