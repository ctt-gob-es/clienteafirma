package es.gob.afirma.standalone.configurator;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URISyntaxException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.logging.Logger;

final class ConfiguratorUtil {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	static final String CERT_ALIAS = "SocketAutoFirma"; //$NON-NLS-1$

	private ConfiguratorUtil() {
		// No instanciable
	}

	/** Guarda datos en disco.
	 * @param data Datos a guardar.
	 * @param outDir Directorio local.
	 * @throws IOException Cuando ocurre un error durante el guardado. */
	static void installFile(final byte[] data, final File outDir) throws IOException {
		try (
			final OutputStream configScriptOs = new FileOutputStream(outDir);
			final BufferedOutputStream bos = new BufferedOutputStream(configScriptOs);
		) {
			bos.write(data);
		}
	}

	/** Elimina un directorio con todo su contenido.
	 * @param targetDir Directorio a eliminar. */
	static void deleteDir(final File targetDir) {
		try {
			Files.walkFileTree(targetDir.toPath(), new SimpleFileVisitor<Path>() {
		         @Override
		         public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
		             Files.delete(file);
		             return FileVisitResult.CONTINUE;
		         }
		         @Override
		         public FileVisitResult postVisitDirectory(final Path dir, final IOException e) throws IOException {
		             if (e != null) {
		            	 throw e;
		             }
		             Files.delete(dir);
	                 return FileVisitResult.CONTINUE;
		         }
		     });

		}
		catch (final IOException e) {
			LOGGER.warning("No se pudo borrar el directorio " + targetDir.getAbsolutePath() + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	/** Recupera el directorio en el que se encuentra la aplicaci&oacute;n.
	 * @return Directorio de ejecuci&oacute;n.
	 */
	static File getApplicationDirectory() {
		File appDir;
		try {
			appDir = new File(ConfiguratorUtil.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath()).getParentFile();
		} catch (final URISyntaxException e) {
			LOGGER.warning("No se pudo localizar el directorio del fichero en ejecucion"); //$NON-NLS-1$
			appDir = null;
		}
		return appDir;
	}
}
