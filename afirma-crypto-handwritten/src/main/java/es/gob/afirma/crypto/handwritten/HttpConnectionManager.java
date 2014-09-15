package es.gob.afirma.crypto.handwritten;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.Proxy;
import java.net.URL;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Logger;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSession;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import es.gob.afirma.core.misc.AOUtil;

public class HttpConnectionManager {

	/** Protocolo de transporte seguro. */
	private static final String HTTPS = "https"; //$NON-NLS-1$

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

	/**
	 * EnvIacute;a datos como par&aacute;metros por POST a una URL.
	 * @param storeUrl URL a la que enviar los datos.
	 * @param params Par&aacute;metros que hay que enviar.
	 * @throws Cuando ocurre alg&uacute;n error durante el env&iacute;o.
	 */
	public static void sendDataByPost(final URL storeUrl, final Map<String, String> params) throws IOException {

		if (storeUrl.getProtocol().equals(HTTPS)) {
			try {
				disableSslChecks();
			}
			catch(final Exception e) {
				Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
						"No se ha podido ajustar la confianza SSL, es posible que no se pueda completar la conexion: " + e //$NON-NLS-1$
						);
			}
		}

		final HttpURLConnection conn = (HttpURLConnection) storeUrl.openConnection(Proxy.NO_PROXY);
		conn.setRequestMethod("POST"); //$NON-NLS-1$

		conn.addRequestProperty("Accept", "*/*"); //$NON-NLS-1$ //$NON-NLS-2$
		conn.addRequestProperty("Connection", "keep-alive"); //$NON-NLS-1$ //$NON-NLS-2$
		conn.addRequestProperty("Content-type", "application/x-www-form-urlencoded"); //$NON-NLS-1$ //$NON-NLS-2$
		conn.addRequestProperty("Host", storeUrl.getHost()); //$NON-NLS-1$
		conn.addRequestProperty("Origin", storeUrl.getProtocol() +  "://" + storeUrl.getHost()); //$NON-NLS-1$ //$NON-NLS-2$

		conn.setDoOutput(true);

		final OutputStreamWriter writer = new OutputStreamWriter(conn.getOutputStream());

		Iterator<String> it = params.keySet().iterator();
		while (it.hasNext()) {
			final String param = it.next();
			writer.write(param);
			writer.write('=');
			writer.write(params.get(param));
		}

		writer.flush();
		writer.close();

		try {
			final InputStream is = conn.getInputStream();
			final byte[] data = AOUtil.getDataFromInputStream(is);
			is.close();

			Logger.getLogger("es.gob.afirma").info("Resultado del almacenamiento: " + new String(data)); //$NON-NLS-1$ //$NON-NLS-2$
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("No se ha podido recoger la respuesta de la peticion de guardado"); //$NON-NLS-1$ //$NON-NLS-2$
		}

		if (storeUrl.getProtocol().equals(HTTPS)) {
			enableSslChecks();
		}
	}

	private static void enableSslChecks() {
		HttpsURLConnection.setDefaultSSLSocketFactory(DEFAULT_SSL_SOCKET_FACTORY);
		HttpsURLConnection.setDefaultHostnameVerifier(DEFAULT_HOSTNAME_VERIFIER);
	}

	private static void disableSslChecks() throws KeyManagementException, NoSuchAlgorithmException {
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
}
