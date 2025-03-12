package es.gob.afirma.standalone.protocol;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.util.logging.Logger;

import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;

import es.gob.afirma.standalone.DesktopUtil;

class SecureSocketUtils {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	// parametros para carga del certificado SSL
	private static final String KSPASS = "654321"; //$NON-NLS-1$
	private static final String CTPASS = "654321"; //$NON-NLS-1$
	private static final String KEYSTORE_NAME = "autofirma.pfx"; //$NON-NLS-1$
	private static final String PKCS12 = "PKCS12"; //$NON-NLS-1$
	private static final String SSLCONTEXT = "TLSv1"; //$NON-NLS-1$

	/**
	 * Obtiene un contexto SSL para la apertura de un socket/websocket seguro.
	 * @return Contexto SSL.
	 * @throws GeneralSecurityException Cuando no se ha podido cargar el almac&eacute;n de claves SSL.
	 * @throws IOException Cuando no se ha encontrado el almacen de claves SSL o este no es v&aacute;lido.
	 */
	static SSLContext getSecureSSLContext() throws GeneralSecurityException, IOException {

		// Ruta del almacen para el cifrado SSL
		final File sslKeyStoreFile = getKeyStoreFile();
		if (sslKeyStoreFile == null) {
			throw new KeyStoreException("No se encuentra el almacen para el cifrado de la comunicacion SSL"); //$NON-NLS-1$
		}

		LOGGER.info("Se utilizara el siguiente almacen para establecer el socket SSL: " + sslKeyStoreFile.getAbsolutePath()); //$NON-NLS-1$

		// pass del fichero
		final char ksPass[] = KSPASS.toCharArray();
		final char ctPass[] = CTPASS.toCharArray();
		// generamos el key store desde el fichero del certificado, de tipo PKCS12
		final KeyStore ks = KeyStore.getInstance(PKCS12);
		try (InputStream is = new FileInputStream(sslKeyStoreFile)) {
			ks.load(is, ksPass);
		}
		// key manager factory por defecto (de tipo SunX509 en OpenJDK, o de tipo ibmX509 en IBM JVM)
		final KeyManagerFactory kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
		kmf.init(ks, ctPass);

		final SSLContext sc = SSLContext.getInstance(SSLCONTEXT);
		sc.init(kmf.getKeyManagers(), null, null);

		return sc;
	}

	/** Obtiene el fichero del almac&eacute;n con la clave SSL de alguno de los directorios
	 * del sistema en los que puede estar.
	 * @return Almac&eacute;n de claves o {@code null} si no se encontr&oacute;. */
	private static File getKeyStoreFile() {

		File appDir = DesktopUtil.getApplicationDirectory();

		if (appDir != null && new File(appDir, KEYSTORE_NAME).exists()) {
			return new File(appDir, KEYSTORE_NAME);
		}

		appDir = DesktopUtil.getAlternativeDirectory();
		if (appDir != null && new File(appDir, KEYSTORE_NAME).exists()) {
			return new File(appDir, KEYSTORE_NAME);
		}

		return null;
	}
}
