package es.gob.afirma.standalone.plugins.manager;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.security.CodeSigner;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.Manifest;
import java.util.zip.ZipFile;
/**
 * Comprueba la validez de un archivo JAR.
 */
public class JarVerifier {


	private static final long THRESHOLD_FILE_SIZE = 100000000; // 100Mb

	private JarVerifier() {
		// Impedimos instanciar la clase
	}

	/**
	 * Comprueba que un fichero se corresponda con un archivo JAR, que est&eacute; firmado
	 * y que las firmas sean integras y que todas las entradas est&eacute;n firmadas por los
	 * mismos firmantes.
	 * @param jarFile Fichero que hay que validar
	 * @return Listado con las cadenas de certificados de firma del JAR o {@code null}
	 * si no se encontraron certificados.
	 * @throws IOException Cuando ocurre un error durante la lectura del archivo.
	 * @throws SecurityException Cuando el JAR no esta correctamente firmado.
	 * @throws JarNoSignedException Cuando el JAR no esta firmado o tiene entradas sin firmar.
	 */
	public static List<X509Certificate[]> verify(final File jarFile)
			throws IOException, SecurityException, JarNoSignedException {

    	if (jarFile.length() >= THRESHOLD_FILE_SIZE) {
    		throw new IOException("El archivo tiene un tamano superior al permitido."); //$NON-NLS-1$
    	}

    	final Set<CodeSigner> signers = new HashSet<>();
		final List<X509Certificate[]> signingCerts = new ArrayList<>();

		// Cargamos el archivo pidiendo que se valide
		try (final JarFile jar = new JarFile(jarFile, true, ZipFile.OPEN_READ)) {

			// Obtenemos el manifest, lo que desencadenara el proceso de verificacion,
			// ademas de que este debe existir si el JAR esta firmado.
			final Manifest manifest = jar.getManifest();

			if (manifest == null) {
				throw new SecurityException("No se ha encontrado el manifest en el archivo"); //$NON-NLS-1$
			}

			// Cargamos todas las entradas para comprobar que estan correctamente firmadas
			final Enumeration<JarEntry> entries = jar.entries();
			while (entries.hasMoreElements()) {
				final JarEntry entry = entries.nextElement();

				// Los directorios y las entradas dentro del directorio META-INF son las unicas
				// entradas que no se firman
				if (entry.isDirectory() || entry.getName().contains("META-INF")) { //$NON-NLS-1$
					continue;
				}

				// Leemos la entrada para forzar a cargar su informacion
				final byte[] buffer = new byte[8096];
				try (InputStream is = jar.getInputStream(entry)) {
					while (is.read(buffer, 0, buffer.length) != -1) {
						// No hacemos nada
					}
				}

				// Comprobamos que la entrada este firmada
				final CodeSigner[] entrySigners = entry.getCodeSigners();
				if (entrySigners == null) {
					throw new JarNoSignedException("Se han encontrado entradas sin firmar: " + entry.getName()); //$NON-NLS-1$
				}

				// Si no hemos registrado antes firmantes, los registramos
				if (signers.isEmpty()) {
					for (final CodeSigner signer : entrySigners) {
						signers.add(signer);
						// Agregamos sus certificados como certificados de firma
						final List<? extends Certificate> certs = signer.getSignerCertPath().getCertificates();
						signingCerts.add(certs.toArray(new X509Certificate[0]));
					}
				}
				// Si ya estaban registrados, comprobamos que la entrada este firmada exactamente
				// por los mismos firmantes que tenemos registrados
				else {
					if (entrySigners.length != signers.size()) {
						throw new JarNoSignedException("Se han encontrado entradas no firmadas por todos los firmantes: " + entry.getName()); //$NON-NLS-1$
					}
					for (final CodeSigner signer : entrySigners) {
						if (!signers.contains(signer)) {
							throw new JarNoSignedException("Se han encontrado entradas firmadas por firmantes distintos al resto: " + entry.getName()); //$NON-NLS-1$
						}
					}
				}
			}
		}
		return signingCerts;
	}
}
