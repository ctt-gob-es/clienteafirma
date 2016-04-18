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
import javax.jnlp.*; 

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
		ExtendedService es; 
		try { 
		    es = 
		(ExtendedService)ServiceManager.lookup("javax.jnlp.ExtendedService");
		} catch (UnavailableServiceException e) { es=null; }
		
		try {
			OutputStream configScriptOs;
			if(es!=null){
				configScriptOs = es.openFile(outDir).getOutputStream(true);
			}else{
				configScriptOs = new FileOutputStream(outDir);
			}
			final BufferedOutputStream bos = new BufferedOutputStream(configScriptOs);
			bos.write(data);
		}catch(Exception e){
			throw e;
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

	/** Recupera el directorio en el que se encuentra la aplicaci&oacute;n actual.
	 * @return Directorio de ejecuci&oacute;n. */
	static File getApplicationDirectory() {
		File appDir;
		
		try{
			ServiceManager.lookup("javax.jnlp.ExtendedService");
			appDir = new File(System.getProperty("java.io.tmpdir"));
			return appDir;
		} catch (UnavailableServiceException e) {

		}
		
		try {
			return new File(ConfiguratorUtil.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath()).getParentFile();
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo localizar el directorio del fichero en ejecucion: " + e); //$NON-NLS-1$
		}
		return null;
	}
}
