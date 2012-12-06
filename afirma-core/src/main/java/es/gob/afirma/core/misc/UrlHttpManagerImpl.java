package es.gob.afirma.core.misc;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
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

/** Implementacion de ua clase para la lectura del contenido de una URL.
 * @author Carlos Gamuci */
public class UrlHttpManagerImpl {

	private UrlHttpManagerImpl() {
		// No permitimos la instanciacion
	}

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

	/** Lee una URL HTTP o HTTPS por POST si se indican par&aacute;metros en la URL y por GET en caso contrario.
	 * En HTTPS no se hacen comprobaciones del certificado servidor.
	 * @param url URL a leer
	 * @return Contenido de la URL
	 * @throws IOException Si no se puede leer la URL */
	public static byte[] readUrlByPost(final String url) throws IOException {
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

		final java.net.URLConnection conn = uri.openConnection();

		conn.setDoOutput(true);
		final OutputStreamWriter writer = new OutputStreamWriter(conn.getOutputStream());

		writer.write(urlParameters);
		writer.flush();

		final InputStream is = conn.getInputStream();
		final byte[] data = AOUtil.getDataFromInputStream(is);
		is.close();

		if (uri.getProtocol().equals("https")) { //$NON-NLS-1$
			enableSslChecks();
		}

		return data;
	}

	/** Lee una URL HTTP o HTTPS por GET. En HTTPS no se hacen comprobaciones del certificado servidor.
	 * @param url URL a leer
	 * @return Contenido de la URL
	 * @throws IOException Si no se puede leer la URL */
	public static byte[] readUrlByGet(final String url) throws IOException {
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
