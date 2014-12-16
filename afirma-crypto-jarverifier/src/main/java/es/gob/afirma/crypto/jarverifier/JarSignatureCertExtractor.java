package es.gob.afirma.crypto.jarverifier;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.security.CodeSource;
import java.security.InvalidAlgorithmParameterException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.security.cert.CertPathValidator;
import java.security.cert.CertPathValidatorException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.PKIXParameters;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import sun.security.pkcs.PKCS7;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.ui.AOUIFactory;

/** Clase de utilidad para obtener los certificados de la firma del JAR
 * que contiene a esta propia clase.
 * Basado en la implementaci&oacute;n de la Universidad de Murcia
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class JarSignatureCertExtractor {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final int BUFFER_SIZE = 1024;

	private static final String SIGNATURE_DIR_PATH = "META-INF/"; //$NON-NLS-1$
	private static final String SIGNATURE_EXT_RSA = ".RSA"; //$NON-NLS-1$
	private static final String SIGNATURE_EXT_DSA = ".DSA"; //$NON-NLS-1$

	private static final String USER_HOME = "$USER_HOME"; //$NON-NLS-1$

	private static final String DEFAULT_PASSWORD = ""; //$NON-NLS-1$
	private static final String[] CACERTS_DEFAULT_PASSWORDS = {
		DEFAULT_PASSWORD,
		"changeit", //$NON-NLS-1$
		"changeme", //$NON-NLS-1$
	};

	private static String keystorePassword = null;

	private JarSignatureCertExtractor() {
		// No permitimos la instanciacion
	}

	private static X509Certificate[] getJarSignatureCertChain() throws IOException {
		final byte[] signature = getJarSignature();
		if (signature == null) {
			return null;
		}
		return new PKCS7(signature).getCertificates();
	}

	private static byte[] getJarSignature() throws IOException {
		final CodeSource src = JarSignatureCertExtractor.class.getProtectionDomain().getCodeSource();
		if (src == null) {
			throw new IOException("No se ha podido acceder a los recursos del JAR"); //$NON-NLS-1$
		}

		int n = 0;
		ZipEntry e;
		ByteArrayOutputStream baos = null;
		final byte[] buffer = new byte[BUFFER_SIZE];
		final ZipInputStream zip = new ZipInputStream(src.getLocation().openStream());
		while((e = zip.getNextEntry()) != null) {
			final String name = e.getName();
			if (name.startsWith(SIGNATURE_DIR_PATH) && (name.endsWith(SIGNATURE_EXT_RSA) || name.endsWith(SIGNATURE_EXT_DSA))) {
				baos = new ByteArrayOutputStream();
				while ((n = zip.read(buffer)) > 0) {
					baos.write(buffer, 0, n);
				}
				break;
			}
		}

		return baos == null ? null : baos.toByteArray();
	}

	/**
	 * Recupera el fichero con el almac&eacute;n de CAs de confianza del usuario.
	 * Si no lo localiza ni lo puede crear, devuelve {@code null}.
	 * @return Fichero con el almac&eacute;n de CAs de confianza del usuario.
	 */
	private static File getUsersJavaCaKeyStoreFile() {

		String keystoreFilename = System.getProperty(
			"deployment.user.security.trusted.cacerts" //$NON-NLS-1$
		);

		// Comprobacion por el error http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=7140869
		if (keystoreFilename != null && keystoreFilename.contains(USER_HOME)) {
			keystoreFilename = keystoreFilename.replace(
				USER_HOME,
				System.getProperty("user.home") //$NON-NLS-1$
			);
		}

		final File ret = keystoreFilename != null ? new File(keystoreFilename) : null;

		// Devolvemos el fichero con el truststore si existe su directorio padre, ya que entonces
		// o existe o lo podemos crear en ese directorio
		return ret != null && ret.getParentFile().exists() ? ret : null;
	}

	/**
	 * Recupera el fichero con el almac&eacute;n de CAs de confianza del sistema.
	 * Si no lo localiza, devuelve {@code null}.
	 * @return Fichero con el almac&eacute;n de CAs de confianza del sistema.
	 */
	private static File getSystemsJavaCaKeyStoreFile() {

		final String keystoreFilename = System.getProperty(
			"deployment.system.security.cacerts" //$NON-NLS-1$
		);

		if (keystoreFilename == null) {
			return null;
		}

		final File ret = new File(keystoreFilename);
		return ret.exists() ? ret : null;
	}

	private static KeyStore getJavaCaKeyStore(final File storeFile) throws KeyStoreException,
	                                                                        NoSuchAlgorithmException,
	                                                                        CertificateException,
	                                                                        IOException {
		final FileInputStream fis = new FileInputStream(storeFile);
		final KeyStore ks = KeyStore.getInstance("JKS"); //$NON-NLS-1$
		for (final String password : CACERTS_DEFAULT_PASSWORDS) {
			try {
				ks.load(fis, password.toCharArray());
				keystorePassword = password;
				break;
			} catch (final IOException e) {
				// Si el error no se debe a una clave erronea, la subimos
				if (!(e.getCause() instanceof UnrecoverableKeyException)) {
					fis.close();
					throw e;
				}
			}
		}
		fis.close();



		return ks;
	}

	private static void checkCertChain(final X509Certificate[] chain,
			                           final KeyStore trustStore) throws CertPathValidatorException,
			                                                             KeyStoreException,
			                                                             InvalidAlgorithmParameterException,
			                                                             CertificateException,
			                                                             NoSuchAlgorithmException {

		// Si no hay certificados en el almacen, no estara entre los certificados de confianza
		if (trustStore.size() == 0) {
			throw new CertPathValidatorException("No hay certificados en el almacen de confianza"); //$NON-NLS-1$
		}

		// Miramos si el certificado mas elevado de la cadena esta en el trustrore,
		// en cuyo caso no hacemos nada
		final X509Certificate chainEdge = chain[chain.length - 1];
		final Enumeration<String> aliases = trustStore.aliases();
		while (aliases.hasMoreElements()) {
			if (chainEdge.getSerialNumber().equals(
					((X509Certificate) trustStore.getCertificate(aliases.nextElement())).getSerialNumber())) {
				LOGGER.info("El extremo de la cadena de certificados esta en el truststore de Java"); //$NON-NLS-1$
				return;
			}
		}

		// Comprobamos ahora la cadena normalmente
		final PKIXParameters params = new PKIXParameters(trustStore);
		params.setRevocationEnabled(false);
		CertPathValidator.getInstance(CertPathValidator.getDefaultType()).validate(
				CertificateFactory.getInstance("X.509").generateCertPath(Arrays.asList(chain)), //$NON-NLS-1$
				params
				);
	}

	/** Inserta los certificados con los que se ha firmado el JAR que contiene esta clase en
	 * el almac&eacute;n de certificados ra&iacute;z del JRE, con el permiso del usuario.
	 * @param dialogParent Padre para el di&aacute;logo de solicitud de permiso
	 * @throws KeyStoreException Si no se puede tratar el almac&eacute;n de certificados ra&iacute;z del JRE
	 * @throws NoSuchAlgorithmException Si no se soporta alg&uacute;n algoritmo necesario
	 * @throws CertificateException Cuando ocurren errores relacionados con los certificados X.509
	 * @throws IOException Cuando ocurren errores de entrada / salida
	 * @throws InvalidAlgorithmParameterException Si no se soporta alg&uacute;n par&aacute;metro necesario
	 *                                            para alg&uacute;n algoritmo  */
	public static void insertJarSignerOnCACerts(final Object dialogParent) throws KeyStoreException,
	                                                                              NoSuchAlgorithmException,
	                                                                              CertificateException,
	                                                                              IOException,
	                                                                              InvalidAlgorithmParameterException {

		// Primero, obtenemos los certificados con los que se ha firmado la aplicacion
		final X509Certificate[] certs = getJarSignatureCertChain();
		if (certs == null || certs.length < 1) {
			LOGGER.warning("La aplicacion no esta firmada"); //$NON-NLS-1$
			return;
		}

		// A continuacion, comprobamos si esos certificados son de confianza para el sistema
		final File systemsCaCertFile = getSystemsJavaCaKeyStoreFile();
		if (systemsCaCertFile != null) {
			try {
				checkCertChain(certs, getJavaCaKeyStore(systemsCaCertFile));
				// Si no salta excepcion, salimos, porque ha validado y no hay que hacer nada
				LOGGER.warning("Los certificados de firma del JAR son de confianza en Java"); //$NON-NLS-1$
				return;
			}
			catch (final Exception e) {
				LOGGER.warning(
					"Error en la validacion de los certificados contra el almacen de Java: " + e //$NON-NLS-1$
				);
				// Si falla continuamos con el almacen de confianza del usuario
			}
		}

		// Si no son de confianza para el sistema, comprobamos si lo son para el usuario
		// Cargamos el fichero del almacen
		final File usersCaCertFile = getUsersJavaCaKeyStoreFile();
		if (usersCaCertFile == null) {
			LOGGER.warning("No se puede localizar el almacen de confianza del usuario, se suspende la validacion"); //$NON-NLS-1$
			return;
		}

		// Si existe el fichero, lo cargamos, si no existe pero se puede crear, lo creamos
		final KeyStore usersTruststore;
		if (!usersCaCertFile.exists()) {
			keystorePassword = DEFAULT_PASSWORD;
			usersTruststore = KeyStore.getInstance(KeyStore.getDefaultType());
			usersTruststore.load(null, keystorePassword.toCharArray());
			LOGGER.info("Creamos el truststore ya que no existia previamente"); //$NON-NLS-1$
		}
		else {
			try {
				usersTruststore = getJavaCaKeyStore(usersCaCertFile);
			}
			catch (final Exception e) {
				LOGGER.warning("No se ha podido cargar el almacen de certificados de CA de confianza del usuario, no se agregara el certificado: " + e); //$NON-NLS-1$
				return;
			}
		}

		// Comprobamos si el extremo de la cadena es de confianza o no
		try {
			checkCertChain(certs, usersTruststore);
			// Si no salta excepcion, salimos, porque ha validado y no hay que hacer
			// nada
			LOGGER.info("Los certificados de firma del JAR ya son de confianza para el usuario"); //$NON-NLS-1$
			return;
		}
		catch (final CertPathValidatorException e) {
			LOGGER.warning("Debemos agregar el certificado al truststore del usuario para que sea de confianza: " + e); //$NON-NLS-1$
			// Se ignora, porque si falla la validacion es que debemos continuar
			// normalmente con el proceso, ya que significa que no se valida la
			// cadena y hay que insertar la raiz
		}

		// Creamos la lista de certificados que se van a insertar para preguntarle al usuario
		final StringBuilder sb = new StringBuilder("<br>"); //$NON-NLS-1$
		for (final X509Certificate cert : certs) {
			sb.append("&nbsp;- "); //$NON-NLS-1$
			sb.append(AOUtil.getCN(cert));
			// El dialogo tendra formato HTML
			sb.append("<br>"); //$NON-NLS-1$
		}

		if (AOUIFactory.showConfirmDialog(
			dialogParent,
			"<html><p>" + //$NON-NLS-1$
				JarSignatureCertExtractorMessages.getString("JarSignatureCertExtractor.0") + //$NON-NLS-1$
				"</p><p>" + //$NON-NLS-1$
				JarSignatureCertExtractorMessages.getString("JarSignatureCertExtractor.1") + //$NON-NLS-1$
				"</p><p>&nbsp;<br>" + //$NON-NLS-1$
				JarSignatureCertExtractorMessages.getString("JarSignatureCertExtractor.2") + //$NON-NLS-1$
				sb.toString() +
				"&nbsp;</p></html>", //$NON-NLS-1$
			JarSignatureCertExtractorMessages.getString("JarSignatureCertExtractor.3"), //$NON-NLS-1$
			AOUIFactory.YES_NO_OPTION,
			AOUIFactory.WARNING_MESSAGE
		) == AOUIFactory.NO_OPTION) {
			return;
		}

		for (final X509Certificate cert : certs) {
			usersTruststore.setCertificateEntry(
				AOUtil.getCN(cert) + cert.getSerialNumber(),
				cert
			);
		}

		final FileOutputStream fos = new FileOutputStream(usersCaCertFile);
		usersTruststore.store(fos, keystorePassword.toCharArray());
		fos.close();

		LOGGER.info("Se han insertado correctamente certificados en el cacerts del usuario"); //$NON-NLS-1$
	}
}
