package es.gob.afirma.core.misc.http;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.GeneralSecurityException;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
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
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;
import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.LoggerUtil;

/** Gestor de la seguridad SSL para las conexiones de red.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class SslSecurityManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	static final TrustManager[] DUMMY_TRUST_MANAGER = new TrustManager[] {
		new X509TrustManager() {
			@Override
			public java.security.cert.X509Certificate[] getAcceptedIssuers() {
				return null;
			}
			@Override
			public void checkClientTrusted(final X509Certificate[] certs, final String authType) { /* No hacemos nada */ }
			@Override
			public void checkServerTrusted(final X509Certificate[] certs, final String authType) { /* No hacemos nada */  }

		}
	};

		static final HostnameVerifier DUMMY_HOSTNAME_VERIFIER = new HostnameVerifier() {
		@Override
		public boolean verify(final String hostname, final SSLSession session) {
			return true;
		}
	};

	private static final String KEYSTORE = "javax.net.ssl.keyStore"; //$NON-NLS-1$
	private static final String KEYSTORE_PASS = "javax.net.ssl.keyStorePassword"; //$NON-NLS-1$
	private static final String KEYSTORE_TYPE = "javax.net.ssl.keyStoreType"; //$NON-NLS-1$
	private static final String KEYSTORE_DEFAULT_TYPE = "JKS"; //$NON-NLS-1$

	private static final String SSL_CONTEXT = "SSL";//$NON-NLS-1$

	private static final HostnameVerifier DEFAULT_HOSTNAME_VERIFIER = HttpsURLConnection.getDefaultHostnameVerifier();
	private static final SSLSocketFactory DEFAULT_SSL_SOCKET_FACTORY = HttpsURLConnection.getDefaultSSLSocketFactory();

	private static final SecureRandom secureRandom = new SecureRandom();

	private static boolean afirmaTrustStoreConfigured = false;

	private SslSecurityManager() {
		// No instanciable
	}

	/** Deshabilita las comprobaciones de certificados en conexiones SSL, acept&aacute;dose entonces
	 * cualquier certificado.
	 * @param conn Conexi&oacute;n de la que desactivar las comprobaciones SSL.
	 * @throws GeneralSecurityException Si hay problemas al desactivar el uso de almacen de claves. */
	public static void disableSslChecks(final HttpsURLConnection conn) throws GeneralSecurityException {

		final SSLContext sc = SSLContext.getInstance(SSL_CONTEXT);
		sc.init(null, DUMMY_TRUST_MANAGER, secureRandom);

		conn.setSSLSocketFactory(sc.getSocketFactory());
		conn.setHostnameVerifier(DUMMY_HOSTNAME_VERIFIER);
	}

	/** Deshabilita las comprobaciones de certificados en conexiones SSL, acept&aacute;dose entonces
	 * cualquier certificado.
	 * @throws GeneralSecurityException Si hay problemas al desactivar el uso de almacen de claves. */
	public static void disableSslChecks() throws GeneralSecurityException {
		setTrustManagerAndKeyManager(
			DUMMY_TRUST_MANAGER,
			DUMMY_HOSTNAME_VERIFIER,
			null,
			null
		);
	}

	/**
	 * Habilita las comprobaciones de certificados en conexiones SSL dej&aacute;ndolas con su
	 * comportamiento por defecto.
	 * @throws IOException Cuando no se pueda encontrar o leer el almac&eacute;n de confianza.
	 * @throws GeneralSecurityException Cuando falla la carga del almac&eacute;n.
	 */
	public static void enableSslChecks() throws IOException, GeneralSecurityException {

		if (afirmaTrustStoreConfigured) {
			configureAfirmaTrustManagers();
		}
		else {
			HttpsURLConnection.setDefaultSSLSocketFactory(DEFAULT_SSL_SOCKET_FACTORY);
			HttpsURLConnection.setDefaultHostnameVerifier(DEFAULT_HOSTNAME_VERIFIER);
		}
		LOGGER.fine(
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
	private static void setTrustManagerAndKeyManager(final TrustManager[] tms,
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
		sc.init(km, tms, secureRandom);

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

	/**
	 * Configura los almacenes de confianza de Java y Autofirma para la validaci&oacute;n de las
	 * conexiones SSL.
	 * @throws IOException Cuando falla la lectura del almac&eacute;n del cliente.
	 * @throws GeneralSecurityException Cuando falla la carga del almac&eacute;n.
	 */
	public static void configureAfirmaTrustManagers() throws IOException, GeneralSecurityException {

		final File trustStoreFile = TrustStoreManager.getJKSFile();

		// Si no encontramos el almacen de confianza del Cliente @firma, no modificamos
		// la configuracion SSL
		if (!trustStoreFile.isFile()) {
			return;
		}

		// Cargamos el almacen en memoria para no requerir ya el fichero
		byte[] trustStoreContent;
		synchronized(TrustStoreManager.getInstance()) {
			try (InputStream is = new FileInputStream(trustStoreFile)) {
				trustStoreContent = AOUtil.getDataFromInputStream(is);
			}
		}

		// Cargamos el almacen y, si no tiene entradas, no modificamos la configuracion SSL
		final KeyStore trustStore = KeyStore.getInstance("JKS"); //$NON-NLS-1$
		try (InputStream cacertIs = new ByteArrayInputStream(trustStoreContent)) {
			trustStore.load(cacertIs, "changeit".toCharArray()); //$NON-NLS-1$
		}

		// Agregamos los trustmanagers del Cliente @firma
		if (trustStore.aliases() == null || !trustStore.aliases().hasMoreElements()) {
			return;
		}

		final X509TrustManager[] trustManagers = new X509TrustManager[2];

		// Agregamos el trustmanager por defecto de Java (solo se toma el primero, que es el unico que aplica)
		TrustManagerFactory factory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
		factory.init((KeyStore) null);
		trustManagers[0] = (X509TrustManager) factory.getTrustManagers()[0];

		// Agregamos los trustmanagers del Cliente @firma
		factory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
		factory.init(trustStore);
		trustManagers[1] = (X509TrustManager) factory.getTrustManagers()[0];

		final MultiX509TrustManager trustManager = new MultiX509TrustManager(trustManagers);

		LOGGER.info("Se configura el almacen de confianza de Autofirma"); //$NON-NLS-1$

		final SSLContext sslContext = SSLContext.getInstance("SSL"); //$NON-NLS-1$
		sslContext.init(null, new TrustManager[] { trustManager }, secureRandom);

		HttpsURLConnection.setDefaultSSLSocketFactory(sslContext.getSocketFactory());
		HttpsURLConnection.setDefaultHostnameVerifier(DEFAULT_HOSTNAME_VERIFIER);

		// Declaramos haber configurado el almacen de confianza del cliente @firma
		afirmaTrustStoreConfigured = true;
	}

	/**
	 * Establece la configuraci&oacute;n SSL certificados de confianza y validacion de nombres de
	 * dominio) para una conexi&oacute;n.
	 * @param conn Conexi&oacute;n a la que aplicar la configuraci&oacute;n.
	 * @param sslConfig Configuraci&oacute;n SSL.
	 */
	public static void setSslSecurity(final HttpsURLConnection conn, final SSLConfig sslConfig) {
		if (sslConfig.getSSLSocketFactory() != null) {
			conn.setSSLSocketFactory(sslConfig.getSSLSocketFactory());
		}
		if (sslConfig.getHostnameVerifier() != null) {
			conn.setHostnameVerifier(sslConfig.getHostnameVerifier());
		}
	}
}
