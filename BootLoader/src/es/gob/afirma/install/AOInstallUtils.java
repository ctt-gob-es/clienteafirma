package es.gob.afirma.install;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.net.URL;
import java.util.Enumeration;
import java.util.jar.JarOutputStream;
import java.util.jar.Pack200.Unpacker;
import java.util.logging.Logger;
import java.util.zip.GZIPInputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.misc.AOBootUtil;

/**
 * Funciones de utilidad para la instalaci&oacute;n del Cliente Afirma.
 */
final class AOInstallUtils {
	
	private AOInstallUtils() {}
	
    static final String PACK200_SUFIX = ".pack.gz";

    /**
     * Desempaqueta un fichero Pack200 para obtener el fichero JAR equivalente. El fichero
     * JAR resultante se almacenar&aacute; en el mismo directorio que el original y tendr&aacute;
     * por nombre el mismo sin la extension ".pack.gz" o ".pack" y terminado en ".jar".
     * @param pack200Filename Nombre del del fichero Pack200 origen, incluyendo ruta
     * @throws AOException Cuando ocurre un error durante el desempaquetado
     */
    static void unpack(final String pack200Filename) throws AOException {
        
        // Obtenemos el nombre del fichero de salida
        String jarFilename = pack200Filename + ".jar"; 
        if(pack200Filename.endsWith(".pack") || pack200Filename.endsWith(".pack.gz")) {
            jarFilename = pack200Filename.substring(0, pack200Filename.lastIndexOf(".pack")); 
            if(!jarFilename.endsWith(".jar")) jarFilename += ".jar";
        }
        
        // Desempaquetamos
        unpack(pack200Filename, jarFilename);
    }

    /**
     * Desempaqueta un fichero Pack200 para obtener el fichero JAR equivalente.
     * @param pack200Filename Nombre del del fichero Pack200 origen, incluyendo ruta
     * @param targetJarFilename Nombre del fichero de salida, incluyendo ruta
     * @throws AOException Cuando ocurre un error durante el desempaquetado.
     */
    static void unpack(final String pack200Filename, String targetJarFilename) throws AOException {
    	if (pack200Filename == null) throw new NullPointerException("El Pack200 origen no puede ser nulo");
    	if (targetJarFilename == null) throw new NullPointerException("El JAR de destino no puede ser nulo");
    	createDirectory(new File(targetJarFilename).getParentFile());
        try {
            unpack200gunzip(
                AOBootUtil.loadFile(
                    AOBootUtil.createURI(pack200Filename),
                    null,
                    false
                ),
                new FileOutputStream(new File(targetJarFilename))
            );
        } 
        catch(final AOException e) {
            throw e;
        } 
        catch(final Throwable e) {
            throw new AOException("Error al desempaquetar el fichero '" + pack200Filename + "'", e);
        }
        
        // Eliminamos la version empaquetada
        new File(pack200Filename).delete();
        
    }
    
    /**
     * Descomprime un JAR comprimido con Pack200 y GZip.
     * @param packgz <i>jar.pack.gz</i> original
     * @param jar JAR resultante del desempaquetado
     * @throws Throwable Si ocurre cualquier problema durante la descompresi&oacute;n
     */
    private static void unpack200gunzip(final InputStream packgz, final OutputStream jar) throws Throwable {
        if (packgz == null) throw new NullPointerException("El pack.gz es nulo");
        InputStream is = new GZIPInputStream(packgz);
        Unpacker u = java.util.jar.Pack200.newUnpacker();
        JarOutputStream jos = new JarOutputStream(jar);
        u.unpack(is, jos);
        jos.flush();
        jos.close();
    }
    
    /**
     * Descomprime un fichero Zip en un directorio. Si el directorio de destino no existe,
     * se crear&aacute;. En caso de ocurrir un error durante la descompresi&oacute;n de un
     * fichero concreto, la operaci&oacute;n no se detendr&aacute; y se tratar&aacute; de
     * descomprimir el resto.<br>
     * Si se introduce un directorio nulo o vac&iacute;o se descomprimir&aacute; en el directorio
     * actual.
     * @param zipFile Fichero zip
     * @param destDirectory Ruta del directorio en donde deseamos descomprimir
     * @throws AOException Cuando ocurre cualquier error durante la descompresi&oacute;n de ficheros
     * @throws IOException El nombre de directorio indicado coincide con el de un fichero
     */
    private static void unzip(final ZipFile zipFile, File destDirectory) throws AOException, IOException {

        if (zipFile == null) throw new NullPointerException("El fichero Zip no puede ser nulo"); //$NON-NLS-1$
        if (destDirectory == null) destDirectory = new File("."); //$NON-NLS-1$

        // Si no existe el directorio de destino, lo creamos
        if (!destDirectory.exists()) destDirectory.mkdirs();
        else if (destDirectory.isFile()) throw new IOException("Ya existe un fichero con el nombre indicado para el directorio de salida"); //$NON-NLS-1$
        else if (!destDirectory.canRead()) throw new IOException("No se dispone de permisos suficientes para almacenar ficheros en el directorio destino: " + destDirectory); //$NON-NLS-1$

        // Descomprimimos los ficheros
        final byte[] buffer = new byte[1024];
        for (Enumeration<? extends ZipEntry> zipEntries = zipFile.entries(); zipEntries.hasMoreElements();) {
            final ZipEntry entry = zipEntries.nextElement();
            try {
                // Creamos el arbol de directorios para el fichero
                final File outputFile = new File(destDirectory, entry.getName());
                if(!outputFile.getParentFile().exists()) outputFile.getParentFile().mkdirs();

                // Descomprimimos el fichero
                final InputStream zeis = zipFile.getInputStream(entry);
                final FileOutputStream fos = new FileOutputStream(outputFile);
                int nBytes;
                while ((nBytes = zeis.read(buffer)) != -1) fos.write(buffer, 0, nBytes);
                try { fos.flush(); } catch(final Throwable e) {}
                try { fos.close(); } catch(final Throwable e) {}
            }
            catch (final Throwable e) {
                throw new AOException(
                    "Error durante la instalacion, no se pudo instalar la biblioteca '" //$NON-NLS-1$
                    + entry.getName() + "'", e //$NON-NLS-1$
                );
            }
        }
    }
    
    /**
     * Copia un fichero indicado por una URL en un directorio local del sistema.
     * @param file Fichero que se desea copiar.
     * @param dirDest Directorio local.
     * @throws Exception Ocurri&oacute; un error durante la copia del fichero.
     */
    static void copyFileFromURL(URL file, File fileDest) throws Exception {
        copyFileFromURL(file, fileDest.getParentFile(), fileDest.getName());
    }
    
    /**
     * Copia un fichero referenciado por una URL en un directorio local del sistema.
     * @param urlFile Fichero que se desea copiar.
     * @param dirDest Directorio local.
     * @param newFilename Nombre con el que se almacenar&aacute; el fichero. Si se indica <code>null</code>
     * se almacena con el mismo nombre que tuviese el fichero remoto.
     * @throws Exception Ocurri&oacute; un error durante la copia del fichero.
     */
    private static void copyFileFromURL(URL urlFile, File dirDest, String newFilename) throws Exception {

        if (urlFile == null) {
            throw new NullPointerException("La URL al fichero remoto no puede ser nula"); //$NON-NLS-1$
        }
        if (dirDest == null) {
            throw new NullPointerException("El directorio destino no puede ser nulo"); //$NON-NLS-1$
        }

        // Obtenemos el nombre del fichero destino a partir del directorio de destino y el nombre del nuevo
        // fichero o el del fichero remoto si no se indico uno nuevo
        String filename = (newFilename != null ? newFilename : urlFile.getPath().substring(urlFile.getPath().lastIndexOf("/"))); //$NON-NLS-1$
        File outFile = new File(dirDest, filename);
        if (!outFile.getParentFile().exists()) {
            outFile.getParentFile().mkdirs();
        } 
        else if (outFile.getParentFile().isFile()) {
        	throw new IOException("No se ha podido crear el arbol de directorios necesario");  //$NON-NLS-1$
        }

        InputStream is = AOBootUtil.loadFile(new URI(urlFile.toString()), null, false);
        final URI uri = AOBootUtil.createURI(urlFile.toString());
        if (uri.getScheme().toLowerCase().equals("file")) { //$NON-NLS-1$
            try {
                is = new FileInputStream(urlFile.toString().substring(7));
            }
            catch (final Throwable e) {
                is = uri.toURL().openStream();
            }
        }
        else {
            is = uri.toURL().openStream();
        }
        
        int nBytes = 0;
        final byte[] buffer = new byte[1024];
        FileOutputStream fos = new FileOutputStream(outFile);
        while ((nBytes = is.read(buffer)) != -1) {
            fos.write(buffer, 0, nBytes);
        }

        try { is.close(); } catch (final Throwable e) {
            Logger.getLogger("es.gob.afirma").warning("No se pudo cerrar el fichero remoto: " + e); //$NON-NLS-1$ //$NON-NLS-2$
        }
        try { fos.close(); } catch (final Throwable e) {
            Logger.getLogger("es.gob.afirma").warning("No se pudo cerrar el fichero local: " + e); //$NON-NLS-1$ //$NON-NLS-2$
        }
    }
    
    /**
     * Crea un fichero vac&iacute;o temporal.
     * @return Fichero vac&iacute;o creado.
     */
    static File createTempFile() {
    	return createTempFile(false);
    }
    
    /**
     * Crea un fichero vac&iacute;o o un directorio temporal.
     * @param isDir Indica si debe crearse un directorio ({@code true}) o un fichero ({@code false}).
     * @return Directorio/Fichero creado.
     */
    static File createTempFile(final boolean isDir) {
    	int n = 0;
    	File file = null;
    	do {
    		try {
				file = File.createTempFile("afirma", null);
			} 
    		catch (final Throwable e) { 
    			continue;
    		}
    		if (file != null && file.exists() && file.isFile()) {
    			file.delete();
    		}
    		n++;
    	} while (file != null && file.exists() && n <= 5);
    	
    	if (n > 5 || file == null) {
    		Logger.getLogger("es.gob.afirma").warning("No se pudo crear un fichero temporal");
    		return null;
    	}
    	
    	if (isDir) {
    		file.mkdir();
    	}
    	
    	return file;
    }
    
    /**
     * Borra un directorio y todos sus subdirectorios y ficheros.
     * @param dir Directorio a borrar.
     * @return Devuelve <code>true</code> si el directorio se borr&oacute; completamente,
     * <code>false</code> si no se pudo borrar el directorio o alguno de los ficheros que contiene. 
     */
    static boolean deleteDir(final File dir) {
        if(dir == null || !dir.exists()) {
            return true;
        }

        boolean success = true;
        
        // Si es un directorio, borramos su contenido
        if (dir.isDirectory()) {
            for (File child : dir.listFiles()) {
                if (!deleteDir(child)) {
                    success = false;
                }
            }
        }
    
        // Borramos el propio fichero o directorio
        if (!dir.delete()) {
            Logger.getLogger("es.gob.afirma").severe("No se ha podido eliminar el archivo: " + dir); //$NON-NLS-1$ //$NON-NLS-2$
            success = false;
        }
        return success;
    }
        
	/**
	 * Realiza la instalaci&oacute;n de un fichero en el sistema local. Si se indica una
	 * CA de firma se comprueba que el fichero ZIP/JAR este firmado con un certificado
	 * expedido por esa CA.
	 * @param remoteFile Ruta del fichero a instalar.
	 * @param installationFile Ruta en donde se instalar&aacute;a el fichero.
	 * @param signingCa CA que se debe verificar. Nulo cuando no proceda.
	 * @throws SecurityException Cuando ocurre un error al validar la firma del fichero.
	 * @throws Exception
	 */
	static void installFile(final URL remoteFile, 
			                final File installationFile, 
			                final SigningCA signingCa) throws SecurityException, Exception {

		if (signingCa == null) {
			copyFileFromURL(remoteFile, installationFile);
		} 
		else {
			// Descargamos el fichero a un temporal, comprobamos la firma y
			// lo enviamos a la ruta de destino
			final File tempFile = createTempFile();
			copyFileFromURL(remoteFile, tempFile);
			checkSign(tempFile, signingCa);
			AOBootUtil.copyFile(tempFile, installationFile);
			try { tempFile.delete(); } catch (final Throwable e) { }
		}
	}

	/**
	 * Realiza la instalaci&oacute;n del contenido de un fichero Zip en el sistema local.
	 * Si se indica una CA de firma se comprueba que el Zip est&eacute; firmado (firma JAR)
	 * con un certificado expedido por esa CA.
	 * @param remoteFile Nombre del fichero a instalar.
	 * @param installationDir Directorio local en donde descomprimir el Zip.
	 * @param signingCa CA que se debe verificar.
	 * @throws SecurityException Cuando ocurre un error al validar la firma del fichero.
	 * @throws Exception
	 */
	static void installZip(final URL remoteFile, 
			               final File installationDir, 
			               final SigningCA signingCa) throws SecurityException, Exception {

		File tempFile = createTempFile();
		copyFileFromURL(remoteFile, tempFile);
		
		if (signingCa != null) {
			checkSign(tempFile, signingCa);
		}

		AOInstallUtils.unzip(new ZipFile(tempFile), installationDir);
	}
	
	/**
	 * Comprueba que la firma de un JAR haya sido realizada con un certificado de la entidad
	 * indicada.
	 * @param jarFile Fichero JAR/ZIP.
	 * @param ca Certificado de la CA.
	 * @throws SecurityException Cuando se produce cualquier error durante el proceso. 
	 */
	private static void checkSign(final File jarFile, final SigningCA ca) throws SecurityException {
		if (AfirmaBootLoader.DEBUG) {
			Logger.getLogger("es.gob.afirma").severe(
				"IMPORTANTE: Modo de depuracion, comprobaciones de firma desacivadas"
			);
			return;
		}
		new AOJarVerifier().verifyJar(
			jarFile.getAbsolutePath(), 
			ca.getCACertificate(), 
			ca.getSigningCertificate()
		);
	}
    
	private static void createDirectory(File dir) {
		if (dir == null) {
			Logger.getLogger("es.gob.afirma").warning(
				"Se ha pedido crear un directorio nulo, se ignorara la peticion"
			);
			return;
		}
		if (dir.exists()) {
			if (!dir.canWrite()) {
				Logger.getLogger("es.gob.afirma").severe(
					"El fichero/directorio '" + dir.getAbsolutePath() + "' ya existe, pero no se tienen derechos de escritura sobre el"
				);
				return;
			}
			if (dir.isFile()) {
				if (dir.delete()) {
					if (dir.mkdir()) {
						Logger.getLogger("es.gob.afirma").warning(
							"'" + dir.getAbsolutePath() + "' ya existia como fichero, se ha borrado y se ha creado un directorio con el mismo nombre"
						);
						return;
					}
				
					Logger.getLogger("es.gob.afirma").severe(
						"'" + dir.getAbsolutePath() + "' ya existia como fichero y se ha borrado, pero no se ha podido crear un directorio con el mismo nombre"
					);
					return;
				}
				Logger.getLogger("es.gob.afirma").severe(
					"'" + dir.getAbsolutePath() + "' ya existe como fichero y no se ha podido borrar"
				);
				return;
			}
		}
		// Si no existe
		else {
			if (!dir.mkdirs()) {
				Logger.getLogger("es.gob.afirma").severe(
						"No se ha podido crear el directorio '" + dir.getAbsolutePath() + "'"
				);
			}
		}
	}
	
}
