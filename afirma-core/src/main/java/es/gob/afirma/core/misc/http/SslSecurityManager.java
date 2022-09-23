package es.gob.afirma.core.misc.http;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.logging.Level;
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
import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.misc.LoggerUtil;

/** Gestor de la seguridad SSL para las conexiones de red.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class SslSecurityManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

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

	private static final String KEYSTORE = "javax.net.ssl.keyStore"; //$NON-NLS-1$
	private static final String KEYSTORE_PASS = "javax.net.ssl.keyStorePassword"; //$NON-NLS-1$
	private static final String KEYSTORE_TYPE = "javax.net.ssl.keyStoreType"; //$NON-NLS-1$
	private static final String KEYSTORE_DEFAULT_TYPE = "JKS"; //$NON-NLS-1$

	private static final String SSL_CONTEXT = "SSL";//$NON-NLS-1$

	private static final HostnameVerifier DEFAULT_HOSTNAME_VERIFIER = HttpsURLConnection.getDefaultHostnameVerifier();
	private static final SSLSocketFactory DEFAULT_SSL_SOCKET_FACTORY = HttpsURLConnection.getDefaultSSLSocketFactory();


	private SslSecurityManager() {
		// No instanciable
	}

	/** Deshabilita las comprobaciones de certificados en conexiones SSL, acept&aacute;dose entonces
	 * cualquier certificado.
	 * @throws KeyManagementException Si hay problemas en la gesti&oacute;n de claves SSL.
	 * @throws NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario. */
	public static void disableSslChecks() throws KeyManagementException, NoSuchAlgorithmException {
		setTrustManagerAndKeyManager(
			DUMMY_TRUST_MANAGER,
			new HostnameVerifier() {
				@Override
				public boolean verify(final String hostname, final SSLSession session) {
					return true;
				}
			},
			null,
			null //TODO: Poner keystore
		);
	}

	/** Habilita las comprobaciones de certificados en conexiones SSL dej&aacute;ndolas con su
	 * comportamiento por defecto. */
	public static void enableSslChecks() {
		HttpsURLConnection.setDefaultSSLSocketFactory(DEFAULT_SSL_SOCKET_FACTORY);
		HttpsURLConnection.setDefaultHostnameVerifier(DEFAULT_HOSTNAME_VERIFIER);
		LOGGER.info(
			"Habilitadas comprobaciones SSL" //$NON-NLS-1$
		);
	}

	/** Establece los <code>TrustManager</code> de las conexiones SSL.
	 * @param tms <code>TrustManager</code> a establecer.
	 * @param hv Verificador de nombres de <i>host</i> a usar en las conexiones SSL.
	 * @param keyManagerKeyStore <code>KeyStore</code> para el <code>KeyManager</code> SSL.
	 * @param sslKeyStorePasswordCallback <code>PasswordCallback</code> con las contrase&ntilde;as para el
	 *                                    <code>KeyStore</code> para el <code>KeyManager</code> SSL.
	 * @throws KeyManagementException Si hay problemas en la gesti&oacute;n de claves SSL.
	 * @throws NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario. */
	public static void setTrustManagerAndKeyManager(final TrustManager[] tms,
			                                        final HostnameVerifier hv,
			                                        final KeyStore keyManagerKeyStore,
			                                        final PasswordCallback sslKeyStorePasswordCallback) throws KeyManagementException,
	                                                                                                           NoSuchAlgorithmException {
		if (tms == null || tms.length < 1) {
			throw new IllegalArgumentException("Es necesario proporcionar al menos un TrustManager"); //$NON-NLS-1$
		}
		if (hv == null) {
			throw new IllegalArgumentException("Es necesario proporcionar un HostnameVerifier"); //$NON-NLS-1$
		}
		final SSLContext sc = SSLContext.getInstance(SSL_CONTEXT);
		KeyManager[] km;
		try {
			km = getKeyManager(keyManagerKeyStore, sslKeyStorePasswordCallback);
		}
		catch(final Exception e) {
			// En ocasiones, los servidores de aplicaciones establecen configuraciones de KeyStore
			// que no se pueden cargar aqui, y no es algo controlable por las aplicaciones
			LOGGER.log(
				Level.SEVERE,
				"No ha sido posible obtener el KeyManager con el KeyStore '" + System.getProperty(KEYSTORE) + //$NON-NLS-1$
					"', se usara null: " + e, //$NON-NLS-1$
				e
			);
			km = null;
		}
		sc.init(
			km,
			tms,
			new java.security.SecureRandom()
		);
		HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory());
		HttpsURLConnection.setDefaultHostnameVerifier(hv);
	}

	/** Devuelve un KeyManager a utilizar cuando se desea deshabilitar las comprobaciones de certificados en las conexiones SSL.
	 * @param keyManagerKeyStore <code>KeyStore</code> para el <code>KeyManager</code> SSL.
	 * @param sslKeyStorePasswordCallback <code>PasswordCallback</code> con las contrase&ntilde;as para el
	 *                                    <code>KeyStore</code> para el <code>KeyManager</code> SSL.
	 * @return KeyManager[] Se genera un KeyManager[] utilizando el keystore almacenado en las propiedades del sistema.
	 * @throws KeyStoreException Si no se puede cargar el KeyStore SSL.
	 * @throws NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario.
	 * @throws CertificateException Si los certificados del KeyStore SSL son inv&aacute;lidos.
	 * @throws IOException Si hay errores en la carga del fichero KeyStore SSL.
	 * @throws UnrecoverableKeyException Si una clave del KeyStore SSL es inv&aacute;lida. */
	private static KeyManager[] getKeyManager(final KeyStore sslKeyStore,
			                                  final PasswordCallback sslKeyStorePasswordCallback) throws KeyStoreException,
	                                                                                                     NoSuchAlgorithmException,
	                                                                                                     CertificateException,
	                                                                                                     IOException,
	                                                                                                     UnrecoverableKeyException {
		final KeyStore kstore;
		final char[] kstorePassword;
		if (sslKeyStore != null) {
			LOGGER.info("Se usara el almacen de claves SSL proporcionado de forma directa: " + sslKeyStore.getType()); //$NON-NLS-1$
			kstore = sslKeyStore;
			kstorePassword = sslKeyStorePasswordCallback != null ? sslKeyStorePasswordCallback.getPassword() : new char[0];
		}
		else {
			final String keyStore = System.getProperty(KEYSTORE);
			final String keyStorePassword = System.getProperty(KEYSTORE_PASS);
			final String keyStoreType = System.getProperty(KEYSTORE_TYPE);
			if (keyStore == null || keyStore.isEmpty()) {
				return null;
			}
			final File f = new File(keyStore);
			if (!f.isFile() || !f.canRead()) {
				LOGGER.warning("El KeyStore SSL no existe o no es legible: " + LoggerUtil.getCleanUserHomePath(f.getAbsolutePath())); //$NON-NLS-1$
				return null;
			}
			kstore = KeyStore.getInstance(
				keyStoreType != null && !keyStoreType.isEmpty() ? keyStoreType : KEYSTORE_DEFAULT_TYPE
			);
			try (
				final InputStream fis = new FileInputStream(f)
			) {
				kstore.load(
					fis,
					keyStorePassword != null ? keyStorePassword.toCharArray() : null
				);
			}
			kstorePassword = keyStorePassword != null ? keyStorePassword.toCharArray() : new char[0];
		}
		final KeyManagerFactory keyFac = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
		keyFac.init(
			kstore,
			kstorePassword
		);
		return keyFac.getKeyManagers();
	}

}
