package es.gob.afirma.standalone.ui.restoreconfig;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

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

	/** Descomprime un fichero ZIP de recurso al disco.
	 * @param resource Ruta del recurso ZIP.
	 * @param outDir Directorio local en el que descomprimir.
	 * @throws IOException Cuando ocurre un error al descomprimir.
	 **/
	static void uncompressResource(final String resource, final File outDir) throws IOException {
		int n;

		final byte[] buffer = new byte[1024];
		try (final ZipInputStream zipIs = new ZipInputStream(ConfiguratorUtil.class.getResourceAsStream(resource));) {
			ZipEntry entry;
			while ((entry = zipIs.getNextEntry()) != null) {
				final File outFile = new File(outDir, entry.getName());
				if (entry.isDirectory()) {
					outFile.mkdirs();
				}
				else {
					try (final FileOutputStream outFis = new FileOutputStream(outFile);) {
						while ((n = zipIs.read(buffer)) > 0) {
							outFis.write(buffer, 0, n);
						}
						outFis.flush();
					}
				}
				zipIs.closeEntry();
			}

		}
	}

	/** Elimina un directorio con todo su contenido.
	 * @param targetDir Directorio a eliminar. */
	static void deleteDir(final File targetDir) {
		try {
			Files.walkFileTree(
				targetDir.toPath(),
				new SimpleFileVisitor<Path>() {
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
			   }
			);
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo borrar el directorio '" + targetDir.getAbsolutePath() + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}


	/**
	 * Guarda los comandos especificados en un buffer.
	 * @param comands Listado de comandos que almacenar.
	 * @param buffer Buffer de datos en el que se almacen el script.
	 */
	static void printScript(String[] comands, StringBuilder buffer) {
		for (final String s : comands) {
			buffer.append(s);
			buffer.append(' ');
		}
		buffer.append("\n"); //$NON-NLS-1$
	}

	/**
	 * Script un script en un fichero en disco. Si el fichero ya existiese, se agregarian
	 * las nuevas sentencias.
	 * @param buffer Buffer con el texto del script.
	 * @param outFile Fichero en el que guardar el script.
	 * @throws IOException Cuando el fichero no se puede crear o escribir.
	 */
	static void writeScript(StringBuilder buffer, File outFile) throws IOException {
		try (final FileOutputStream fout = new FileOutputStream(outFile, true);) {
			fout.write(buffer.toString().getBytes());
		}
	}

	/** Recupera el directorio en el que se encuentra la aplicaci&oacute;n actual.
	 * @return Directorio de ejecuci&oacute;n. */
	static File getApplicationDirectory() {
		try {
			return new File(ConfiguratorUtil.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath()).getParentFile();
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo localizar el directorio del fichero en ejecucion: " + e); //$NON-NLS-1$
		}
		return null;
	}
}
