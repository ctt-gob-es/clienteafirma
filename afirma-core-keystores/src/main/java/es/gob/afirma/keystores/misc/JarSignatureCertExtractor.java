package es.gob.afirma.keystores.misc;

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
import java.security.cert.CertPathValidator;
import java.security.cert.CertPathValidatorException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.PKIXParameters;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import sun.security.pkcs.PKCS7;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;

/** Clase de utilidad para obtener los certificados de la firma del JAR
 * que contiene a esta propia clase.
 * Basado en la implementaci&oacute;n de la Universidad de Murcia
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
@SuppressWarnings("restriction")
public final class JarSignatureCertExtractor {

	
	private static final String SIGNATURE_DIR_PATH = "META-INF/"; //$NON-NLS-1$
	private static final String SIGNATURE_EXT = ".RSA"; //$NON-NLS-1$
	
	private static final String USER_HOME = "$USER_HOME"; //$NON-NLS-1$
	private static final String CACERTS_DEFAULT_PASSWORD = "changeit"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

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
		final byte[] buffer = new byte[1024];
		final ZipInputStream zip = new ZipInputStream(src.getLocation().openStream());
		while((e = zip.getNextEntry()) != null) {
			String name = e.getName();
			if (name.startsWith(SIGNATURE_DIR_PATH) && name.endsWith(SIGNATURE_EXT)) {
				LOGGER.info("Entrada de la firma: " + name);
				baos = new ByteArrayOutputStream();
				while ((n = zip.read(buffer)) > 0) {
					baos.write(buffer, 0, n);
				}
				break;
			}
		}
		return baos == null ? null : baos.toByteArray();
	}
	
	private static File getJavaCaKeyStoreFile() {

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

		File ret = keystoreFilename != null ? new File(keystoreFilename) : null;

		// Si no existe el alamcen indicado por la variable de entorno o esta variable
		// no estaba establecida buscamos en la ruta por defecto
		if (ret == null || !ret.exists()) {
			final String defaultPath = Platform.getJavaHome() +
				File.separator +
				"lib" + //$NON-NLS-1$
				File.separator +
				"security" + //$NON-NLS-1$
				File.separator +
				"cacerts"; //$NON-NLS-1$

			ret = new File(defaultPath);
			if (ret.exists()) {
				return ret;
			}
		}

		throw new IllegalStateException(
			"No se ha encontrado el almacen de certificados raiz de Java" //$NON-NLS-1$
		);

	}

	private static KeyStore getJavaCaKeyStore(final File storeFile) throws KeyStoreException,
	                                                                        NoSuchAlgorithmException,
	                                                                        CertificateException,
	                                                                        IOException {
		final FileInputStream fis = new FileInputStream(storeFile);
		final KeyStore ks = KeyStore.getInstance("JKS"); //$NON-NLS-1$
		ks.load(fis, CACERTS_DEFAULT_PASSWORD.toCharArray());
		fis.close();

		return ks;
	}

	private static void chechCertChain(final X509Certificate[] chain,
			                           final KeyStore trustStore) throws CertPathValidatorException,
			                                                             KeyStoreException,
			                                                             InvalidAlgorithmParameterException,
			                                                             CertificateException,
			                                                             NoSuchAlgorithmException {
		
//		// Miramos primero si es un certificado autofirmado anadido directamente como raiz,
//		// porque en ese caso no forma cadena de confianza
//		final X509Certificate chainEdge = chain[chain.length - 1];
//		final Enumeration<String> aliases = trustStore.aliases();
//		while (aliases.hasMoreElements()) {
//			if (chainEdge.getPublicKey().equals(
//				trustStore.getCertificate(
//					aliases.nextElement()
//				).getPublicKey()
//
//			)) {
//				LOGGER.info("El extremo de la cadena de certificados esta directamente como raiz"); //$NON-NLS-1$
//				return;
//			}
//		}
		
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
	 * @throws KeyStoreException
	 * @throws NoSuchAlgorithmException
	 * @throws CertificateException
	 * @throws IOException
	 * @throws InvalidAlgorithmParameterException */
	public static void insertJarSignerOnCACerts(final Object dialogParent) throws KeyStoreException,
	                                                                              NoSuchAlgorithmException,
	                                                                              CertificateException,
	                                                                              IOException,
	                                                                              InvalidAlgorithmParameterException {
		// Primero obtenemos el almacen CA de Java, asi si no se encuentra o la
		// contrasena no es la por defecto o se continua
		final File cacertFile = getJavaCaKeyStoreFile();
		final KeyStore cacerts = getJavaCaKeyStore(cacertFile);

		// Obtenemos los certificados con los que se ha firmado este JAR
		final X509Certificate[] certs = getJarSignatureCertChain();

		if (certs == null || certs.length < 1) {
			return;
		}

		// Comprobamos si el extremo de la cadena es de confianza o no
		try {
			chechCertChain(certs, cacerts);
			// Si no salta excepcion, salimos, porque ha validado y no hay que hacer
			// nada
			LOGGER.info("Los certificados de firma del JAR ya son de confianza en Java"); //$NON-NLS-1$
			return;
		}
		catch (final CertPathValidatorException e) {
			LOGGER.warning("Debemos agregar el certificado al truststore para que sea de confianza: " + e); //$NON-NLS-1$
			// Se ignora, porque si falla la validacion es que debemos continuar
			// normalmente con el proceso, ya que significa que no se valida la
			// cadena y hay que hacer insertar la raiz
		}

		// Creamos una lista de que se va a insertar para preguntarle al usuario
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
			cacerts.setCertificateEntry(
				AOUtil.getCN(cert) + cert.getSerialNumber(),
				cert
			);
		}

		final FileOutputStream fos = new FileOutputStream(cacertFile);
		cacerts.store(fos, CACERTS_DEFAULT_PASSWORD.toCharArray());
		fos.close();

		LOGGER.info("Se han insertado correctamente certificados en el cacerts del usuario"); //$NON-NLS-1$

	}

	/** Main
	 * @param args
	 * @throws Exception */
	public static void main(final String args[]) throws Exception {
		insertJarSignerOnCACerts(null);
	}

}
