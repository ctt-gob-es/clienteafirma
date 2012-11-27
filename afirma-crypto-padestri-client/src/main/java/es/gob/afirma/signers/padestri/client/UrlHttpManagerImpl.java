package es.gob.afirma.signers.padestri.client;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.net.URLConnection;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.Map;
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
public class UrlHttpManagerImpl implements UrlHttpManager {

	private static final int BUFFER_SIZE = 1024;

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

	@Override
	public byte[] readUrl(final String url) throws IOException {
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
		final byte[] data = getDataFromInputStream(is);
		is.close();
		if (uri.getProtocol().equals("https")) { //$NON-NLS-1$
			enableSslChecks();
		}
		return data;
	}

    /** Lee un flujo de datos de entrada y los recupera en forma de array de
     * bytes. Este m&eacute;todo consume pero no cierra el flujo de datos de
     * entrada.
     * @param input
     *        Flujo de donde se toman los datos.
     * @return Los datos obtenidos del flujo.
     * @throws IOException
     *         Cuando ocurre un problema durante la lectura */
    private static byte[] getDataFromInputStream(final InputStream input) throws IOException {
        if (input == null) {
            return new byte[0];
        }
        int nBytes = 0;
        final byte[] buffer = new byte[BUFFER_SIZE];
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        while ((nBytes = input.read(buffer)) > 0) {
            baos.write(buffer, 0, nBytes);
        }
        return baos.toByteArray();
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

	@Override
	public byte[] readUrl(final String url, final Map<String, String> params) throws IOException {
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
		final URLConnection conn = uri.openConnection();
        conn.setDoOutput(true);
        final OutputStreamWriter writer = new OutputStreamWriter(conn.getOutputStream());

        //write parameters
        if (params.size() > 0) {
        	final String[] keys = params.keySet().toArray(new String[params.size()]);
        	writer.write(keys[0] + "=" + params.get(keys[0])); //$NON-NLS-1$
        	for (final String key : keys) {
        		writer.write("&" + key + "=" + params.get(key)); //$NON-NLS-1$ //$NON-NLS-2$
        	}
        }
    	writer.flush();
    	//writer.close();

		if (uri.getProtocol().equals("https")) { //$NON-NLS-1$
			enableSslChecks();
		}
		return getDataFromInputStream(conn.getInputStream());
	}


}
