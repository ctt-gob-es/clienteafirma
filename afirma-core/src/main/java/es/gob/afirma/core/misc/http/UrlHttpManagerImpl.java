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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.Proxy;
import java.net.URL;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.UnrecoverableKeyException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.StringTokenizer;
import java.util.logging.Logger;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.KeyManager;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSession;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import es.gob.afirma.core.misc.AOUtil;

/** Clase para la lectura y env&iacute;o de datos a URL remotas.
 * @author Carlos Gamuci */
public class UrlHttpManagerImpl implements UrlHttpManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Tiempo de espera por defecto para descartar una conexi&oacute;n HTTP. */
	public static final int DEFAULT_TIMEOUT = -1;

	private static final String HTTPS = "https"; //$NON-NLS-1$

	private static final HostnameVerifier DEFAULT_HOSTNAME_VERIFIER = HttpsURLConnection.getDefaultHostnameVerifier();
	private static final SSLSocketFactory DEFAULT_SSL_SOCKET_FACTORY = HttpsURLConnection.getDefaultSSLSocketFactory();
	private static final String KEYSTORE = "javax.net.ssl.keyStore"; //$NON-NLS-1$
	private static final String KEYSTORE_PASS = "javax.net.ssl.keyStorePassword"; //$NON-NLS-1$
	private static final String KEYSTORE_TYPE = "javax.net.ssl.keyStoreType"; //$NON-NLS-1$
	private static final String KEYSTORE_INSTANCE = "SunJSSE"; //$NON-NLS-1$
	private static final String KEYSTORE_DEFAULT_TYPE = "JKS"; //$NON-NLS-1$
	private static final String KEYMANAGER_INSTANCE = "SunX509";//$NON-NLS-1$
	private static final String SSL_CONTEXT = "SSL";//$NON-NLS-1$

	private enum Method {
		GET,
		POST
	}

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

	private byte[] readUrl(final String url,
			               final int timeout,
			               final String contentType,
			               final Method method) throws IOException {
		if (url == null) {
			throw new IllegalArgumentException("La URL a leer no puede ser nula"); //$NON-NLS-1$
		}

		// Si la URL no tiene parametros la leemos por GET
		if (!url.contains("?") && Method.POST.equals(method)) { //$NON-NLS-1$
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
				"Se ha pedido una peticion POST sin parametros, pero se realizara por GET" //$NON-NLS-1$
			);
			return readUrl(url, timeout, contentType, Method.GET);
		}

		String urlParameters = null;
		String request = null;
		if (Method.POST.equals(method)) {
			final StringTokenizer st = new StringTokenizer(url, "?"); //$NON-NLS-1$
			request = st.nextToken();
			urlParameters = st.nextToken();
		}

		final URL uri = new URL(request != null ? request : url);

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

		conn.setRequestMethod(method.toString());

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

		if (urlParameters != null) {
			conn.setDoOutput(true);
			final OutputStreamWriter writer = new OutputStreamWriter(
				conn.getOutputStream()
			);
			writer.write(urlParameters);
			writer.flush();
			writer.close();
		}

		conn.connect();
		final int resCode = conn.getResponseCode();
		final String statusCode = Integer.toString(resCode);
		if (statusCode.startsWith("4") || statusCode.startsWith("5")) { //$NON-NLS-1$ //$NON-NLS-2$
			if (uri.getProtocol().equals(HTTPS)) {
				enableSslChecks();
			}
			throw new HttpError(resCode, conn.getResponseMessage(), url);
		}

		final InputStream is = conn.getInputStream();
		final byte[] data = AOUtil.getDataFromInputStream(is);
		is.close();

		if (uri.getProtocol().equals(HTTPS)) {
			enableSslChecks();
		}

		return data;

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
		return readUrl(url, timeout, contentType, Method.POST);
	}

	/** Lee una URL HTTP o HTTPS por GET. En HTTPS no se hacen comprobaciones del certificado servidor.
	 * @param url URL a leer.
	 * @return Contenido de la URL (de la lectura del contenido referenciado).
	 * @throws IOException Si no se puede leer la URL. */
	@Override
	public byte[] readUrlByGet(final String url) throws IOException {
		return readUrl(url, DEFAULT_TIMEOUT, null, Method.GET);
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
	 * @throws NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario.
	 * @throws KeyStoreException Si no se puede cargar el KeyStore SSL.
	 * @throws IOException Si hay errores en la carga del fichero KeyStore SSL.
	 * @throws CertificateException Si los certificados del KeyStore SSL son inv&aacute;lidos.
	 * @throws UnrecoverableKeyException Si una clave del KeyStore SSL es inv&aacute;lida.
	 * @throws NoSuchProviderException Si ocurre un error al recuperar la instancia del Keystore.*/
	public static void disableSslChecks() throws KeyManagementException,
	                                             NoSuchAlgorithmException,
	                                             KeyStoreException, UnrecoverableKeyException, CertificateException, IOException, NoSuchProviderException {
		final SSLContext sc = SSLContext.getInstance(SSL_CONTEXT);
		sc.init(getKeyManager(), DUMMY_TRUST_MANAGER, new java.security.SecureRandom());
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

	/**
	 * Devuelve un KeyManager a utilizar cuando se desea deshabilitar las comprobaciones de certificados en las conexiones SSL.
	 * @return KeyManager[] Se genera un KeyManager[] utilizando el keystore almacenado en las propiedades del sistema.
	 * @throws KeyStoreException Si no se puede cargar el KeyStore SSL.
	 * @throws NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario.
	 * @throws CertificateException Si los certificados del KeyStore SSL son inv&aacute;lidos.
	 * @throws IOException Si hay errores en la carga del fichero KeyStore SSL.
	 * @throws UnrecoverableKeyException Si una clave del KeyStore SSL es inv&aacute;lida.
	 * @throws NoSuchProviderException Si ocurre un error al recuperar la instancia del Keystore.
	 */
	private static KeyManager[] getKeyManager() throws KeyStoreException,
	                                                   NoSuchAlgorithmException,
	                                                   CertificateException,
	                                                   IOException,
	                                                   UnrecoverableKeyException, NoSuchProviderException {
		final String keyStore = System.getProperty(KEYSTORE);
		final String keyStorePassword = System.getProperty(KEYSTORE_PASS);
		final String keyStoreType = System.getProperty(KEYSTORE_TYPE);
		if (keyStore == null || keyStore.isEmpty()) {
			return null;
		}
		final File f = new File(keyStore);
		if (!f.isFile() || !f.canRead()) {
			LOGGER.warning("El KeyStore SSL no existe o no es legible: " + f.getAbsolutePath()); //$NON-NLS-1$
			return null;
		}
		final KeyStore keystore = KeyStore.getInstance(
			keyStoreType != null && !keyStoreType.isEmpty() ? keyStoreType : KEYSTORE_DEFAULT_TYPE, KEYSTORE_INSTANCE
		);
		final InputStream fis = new FileInputStream(f);
		keystore.load(
			fis,
			keyStorePassword != null ? keyStorePassword.toCharArray() : null
		);
		fis.close();
		final KeyManagerFactory keyFac = KeyManagerFactory.getInstance(KEYMANAGER_INSTANCE);
		keyFac.init(
			keystore,
			keyStorePassword != null ? keyStorePassword.toCharArray() : null
		);
		return keyFac.getKeyManagers();
	}

}
