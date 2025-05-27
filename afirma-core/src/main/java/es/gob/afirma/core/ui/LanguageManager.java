package es.gob.afirma.core.ui;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;

import es.gob.afirma.core.LanguageException;
import es.gob.afirma.core.misc.AOFileUtils;
import es.gob.afirma.core.misc.LoggerUtil;

/**
 * Gestiona los idiomas
 */
public class LanguageManager {
	
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
	
	private static final String METADATA_FILENAME = "metadata.info"; //$NON-NLS-1$
	
	private static final String LOCALE_PROP = "locale"; //$NON-NLS-1$
	
	private static final String LANGUAGE_NAME_PROP = "language.name"; //$NON-NLS-1$
	
	private static final String FALLBACK_LOCALE = "fallback.locale"; //$NON-NLS-1$
	
	private static File languagesDir;
	
	public static final Locale [] AFIRMA_DEFAULT_LOCALES = {
	    	new Locale("es", "ES"), //$NON-NLS-1$ //$NON-NLS-2$
	        new Locale("en", "EN"), //$NON-NLS-1$ //$NON-NLS-2$
	        new Locale("ca", "ES"), //$NON-NLS-1$ //$NON-NLS-2$
	        new Locale("gl", "ES"), //$NON-NLS-1$ //$NON-NLS-2$
	        new Locale("eu", "ES"), //$NON-NLS-1$ //$NON-NLS-2$
	        new Locale("va", "ES")  //$NON-NLS-1$ //$NON-NLS-2$
	};
	
	public static void init(final File langDir) {
		languagesDir = langDir;
	}
	
	/** 
	 * Importa y agrega un nuevo idioma. 
	 * @throws IOException 
	 * @throws LanguageException 
	 * @throws IOException Error al leer archivo con informaci&oacute;n sobre el idioma
	 * @throws LanguageException Error al importar idioma
	 * */
	public static void addLanguage(final File langFile) throws IOException, LanguageException {
		
		Map<String, String> langProps = null;
		
		langProps = readMetadataInfo(langFile);
		
		final String localeName = langProps.get(LOCALE_PROP);

		copyLanguageToDirectory(langFile, new File(languagesDir, localeName));
	}
	
	/**
	 * Copia un fichero de idioma al directorio de idiomas de manera corriente, presuponiendo que se dispone
	 * de permisos en el directorio de idiomas y su directorio padre. Si no existiese el directorio de idiomas,
	 * se crear&oacute;a.
	 * @param langFile Fichero de idioma.
	 * @param langDir Directorio de idiomas.
	 * @return Fichero del idioma ya instalado en el directorio.
	 * @throws IOException Cuando ocurre un error de permisos o durante la copia.
	 * @throws PluginInstalledException Cuando el idioma ya existe.
	 */
	private static File copyLanguageToDirectory(final File langFile, final File langDir) throws IOException, LanguageException {

		// Creamos el directorio de idiomas si es preciso
		if (!langDir.isDirectory()) {
			try {
				if (!langDir.getParentFile().exists()) {
					createDirWithPermissions(langDir.getParentFile());
				}
				createDirWithPermissions(langDir);
			}
			catch (final Exception e) {
				throw new IOException("No se ha podido crear el directorio interno de idiomas: " + LoggerUtil.getCleanUserHomePath(langDir.getAbsolutePath()), e); //$NON-NLS-1$
			}
		}
		
		final File outLangFile = new File(langDir, langFile.getName());
		try (OutputStream fos = new FileOutputStream(outLangFile)) {
			Files.copy(langFile.toPath(), fos);
		}
		AOFileUtils.setAllPermissions(outLangFile);
		
		unzipFile(outLangFile, langDir.getAbsolutePath());
		
		return outLangFile;
		
	}
	
	/**
	 * Crea un directorio y sus subdirectorios y le concede permisos de lectura, escritura y ejecucion a todos los usuarios.
	 * @param dir Directorio.
	 * @throws IOException Cuando falla la creaci&oacute;n del directorio.
	 */
	private static void createDirWithPermissions(final File dir) throws IOException {

		if (!dir.mkdirs()) {
			throw new IOException("No se pudo crear el directorio: " + LoggerUtil.getCleanUserHomePath(dir.getAbsolutePath())); //$NON-NLS-1$
		}
		AOFileUtils.setAllPermissions(dir);
	}
	
	private static Map<String, String> readMetadataInfo(final File langFile) throws IOException {
	        final String metadataFile = "metadata.info";   //$NON-NLS-1$
	        final Map<String, String> langProps = new HashMap<String, String>();

	        try (ZipFile zipFile = new ZipFile(langFile.getAbsolutePath())) {
	            final ZipEntry entry = zipFile.getEntry(metadataFile);
	            if (entry != null) {
	                try (InputStream is = zipFile.getInputStream(entry);
	                    BufferedReader reader = new BufferedReader(new InputStreamReader(is))) {
	                    String line;
	                    while ((line = reader.readLine()) != null) {
	                    	final String[] parts = line.split("=", 2); //$NON-NLS-1$
	                    	if (parts.length == 2) {
	                    	    final String key = parts[0].trim();
	                    	    final String value = parts[1].trim();
	                    	    langProps.put(key, value);
	                    	} else {
	                    		throw new IOException("El archivo \"" + metadataFile + "\" no esta formado correctamente."); //$NON-NLS-1$ //$NON-NLS-2$
	                    	}
	                    }
	                }
	            } else {
	            	throw new IOException("El archivo \"" + metadataFile + "\" no se encuentra en el zip.");  //$NON-NLS-1$//$NON-NLS-2$
	            }
	        } catch (final IOException e) {
	        	throw new IOException("Error al leer el archivo " + metadataFile, e); //$NON-NLS-1$
	        }
	        
	        return langProps;
	}
	
	private static String readMetadataLanguageName(final File langFile) throws IOException {
        String result = null;
        try (FileInputStream fis = new FileInputStream(langFile);
        	BufferedReader reader = new BufferedReader(new InputStreamReader(fis))) {
            String line;
            while ((line = reader.readLine()) != null) { 
            	final String[] parts = line.split("=", 2); //$NON-NLS-1$
            	if (LANGUAGE_NAME_PROP.equals(parts[0])) {
            		result = parts[1];
            	}
            }          
        } catch (final IOException e) {
        	throw new IOException("Error al leer el archivo " + langFile, e); //$NON-NLS-1$
        }
        return result;
	}
	
	public static Locale readMetadataBaseLocale(final Locale locale) throws IOException {
        Locale result = null;
        final File localeDir = new File(languagesDir, locale.getLanguage() + "_" + locale.getCountry()); //$NON-NLS-1$
    	final File metadataFile = new File(localeDir, METADATA_FILENAME);
        try (FileInputStream fis = new FileInputStream(metadataFile);
        	BufferedReader reader = new BufferedReader(new InputStreamReader(fis))) {
            String line;
            while ((line = reader.readLine()) != null) { 
            	final String[] parts = line.split("=", 2); //$NON-NLS-1$
            	if (FALLBACK_LOCALE.equals(parts[0])) {
            		final String baseLocale = parts[1];
            		final String[] localeParts = baseLocale.split("_", 2); //$NON-NLS-1$
            		result = new Locale(localeParts[0], localeParts[1]);
            	}
            }          
        } catch (final IOException e) {
        	throw new IOException("Error al leer el archivo " + metadataFile, e); //$NON-NLS-1$
        }
        return result;       
	}
	
	private static void unzipFile(final File zipFile, final String dirPath) {

		try (ZipInputStream zis = new ZipInputStream(new FileInputStream(zipFile))) {
			ZipEntry entry;
			while ((entry = zis.getNextEntry()) != null) {

				final String entryName = entry.getName();

				File outputFile;
				if (entryName.startsWith("help/") || entryName.startsWith("help\\")) { //$NON-NLS-1$ //$NON-NLS-2$
					final File helpInstallDir = new File(getApplicationDirectory() + File.separator + "help"); //$NON-NLS-1$
					if (helpInstallDir.isDirectory()) {
						helpInstallDir.mkdirs();
					}
					outputFile = new File(helpInstallDir, entryName.substring(("help" + File.separator).length())); //$NON-NLS-1$
				} else {
					outputFile = new File(dirPath, entryName);
				}

				if (entry.isDirectory()) {
					outputFile.mkdirs();
				} else {
					final File parentDir = new File(outputFile.getParent());
					if (!parentDir.exists()) {
						parentDir.mkdirs();
					}

					try (FileOutputStream fos = new FileOutputStream(outputFile)) {
						final byte[] buffer = new byte[1024];
						int len;
						while ((len = zis.read(buffer)) > 0) {
							fos.write(buffer, 0, len);
						}
					}
				}
				zis.closeEntry();
			}
		} catch (final IOException e) {
			LOGGER.log(Level.WARNING, "No se ha podido descomprimir el fichero de idioma", e); //$NON-NLS-1$
		}

		zipFile.delete();
	}
    
    public static Locale [] getImportedLocales() {
    	Locale [] result = null;
    	if (languagesDir.exists()) {
	    	final File [] localeDirs = languagesDir.listFiles();
	    	result =  new Locale[languagesDir.listFiles().length];
	    	for (int i = 0; i < localeDirs.length; i++) {
	            if (localeDirs[i].isDirectory()) {
	            	final String[] parts = localeDirs[i].getName().split("_", 2); //$NON-NLS-1$
	            	final String lang = parts[0];
	            	final String country = parts[1];
	                result[i] = new Locale(lang, country);
	            }
	        }
    	}
    	return result;
    }
    
    public static String getLanguageName(final Locale locale) {
    	final File localeDir = new File(languagesDir, locale.getLanguage() + "_" + locale.getCountry()); //$NON-NLS-1$
    	final File metadataFile = new File(localeDir, METADATA_FILENAME);
    	String result = null;
    	if (metadataFile.exists()) {
    		try {
				result = readMetadataLanguageName(metadataFile);
			} catch (final IOException e) {
				LOGGER.log(Level.WARNING, "No se ha leer el nombre de idioma", e); //$NON-NLS-1$
				return null;
			}
    	}
    	return result;
    }
    
    /**
     * Comprueba si el idioma pertenece a alguno de los que ofrece Autofirma por defecto.
     * @param locale Idioma a comprobar.
     * @return true en caso de que sea un idioma de los que proporciona Autofirma.
     */
    public static boolean isDefaultLocale(final Locale locale) {
    	boolean result = false;
    	for (final Locale l : AFIRMA_DEFAULT_LOCALES) {
    		if (l.equals(locale)) {
    			result = true;
    		}
    	}
    	return result;
    }
    
    /**
     * Comprueba si existe una version importada por el usuario de los idiomas que ofrece Autofirma.
     * @return true en caso de que exista una version importada del idioma configurado.
     */
    public static boolean existDefaultLocaleNewVersion() {
		final File localeDir = new File(LanguageManager.getLanguagesDir(), Locale.getDefault().getLanguage() + "_" + Locale.getDefault().getCountry()); //$NON-NLS-1$
        return localeDir.exists();
    }
    
	/**
	 * Si es posible, devuelve el comando a ejecutar para arrancar la aplicaci&oacute;n.
	 * Si no se encuentra un modo de arrancar la nueva instancia de la aplicaci&oacute;n,
	 * no se hace nada.
	 * @return Comando de arranque.
	 */
	public static List<String> getResetApplicationCommand() {

		File currentFile;
		try {
			currentFile = new File(LanguageManager.class.getProtectionDomain().getCodeSource().getLocation().toURI());
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se ha podido identificar el fichero ejecutable", e); //$NON-NLS-1$
			return null;
		}

		// Compone el comando necesario para arrancar la aplicacion
		final List<String> command = getCommand(currentFile);
		
		return command;
	}

	/**
	 * Devuelve el comando necesario para ejecutar la aplicaci&oacute;n o {@code null}
	 * si no hay una forma efectiva de ejecutarla
	 * @param currentFile Fichero o directorio con la aplicaci&oacute;n.
	 * @return Par&aacute;meros para la ejecuci&oacute;n de la aplicaci&oacute;n.
	 */
	private static List<String> getCommand(final File currentFile) {

		// La aplicacion se ejecutan las clases Java. No va a poder ejecutarse sin las
		// dependencias, por lo que se omite
		if (currentFile.isDirectory()) {
			return null;
		}

		// La aplicacion se ejecuta desde un JAR
		List<String> command;
		if (currentFile.getName().toLowerCase().endsWith(".jar")) { //$NON-NLS-1$

			// Si ese JAR forma parte de un ejecutable macOS, usamos el ejecutable
			final File appMac = getMacApp(currentFile);
			if (appMac != null && appMac.isFile()) {
				command = new ArrayList<>();
				command.add(appMac.getAbsolutePath());
			}
			else {
				final String java = System.getProperty("java.home") + File.separator + "bin" + File.separator + "java"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				command = new ArrayList<>();
				command.add(java);
				command.add("-jar"); //$NON-NLS-1$
				command.add(currentFile.getPath());
			}
		}
		// La aplicacion es un ejecutable de Windows
		else if (currentFile.getName().toLowerCase().endsWith(".exe")) { //$NON-NLS-1$
			command = new ArrayList<>();
			command.add(currentFile.getPath());
		}
		// En cualquier otro caso, no reiniciamos
		else {
			command = null;
		}

		return command;
	}
	
	/**
	 * Comprueba si un fichero JAR se encuentra dentro de la estructura de una aplicaci&oacute;n Mac
	 * y devuelve el ejecutable del mismo nombre para su arranque.
	 * @param jarFile Fichero JAR
	 * @return Fichero de ejecuci&oacute;n o {@code null} si no se puede localizar.
	 */
	private static File getMacApp(final File jarFile) {

		final File jarDir = jarFile.getParentFile();
		if (jarDir != null && "JAR".equals(jarDir.getName())) { //$NON-NLS-1$
			final File resourcesDir = jarDir.getParentFile();
			if (resourcesDir != null && "Resources".equals(resourcesDir.getName())) { //$NON-NLS-1$
				final File contentsDir = resourcesDir.getParentFile();
				if (contentsDir != null && "Contents".equals(contentsDir.getName())) { //$NON-NLS-1$
					final File macOSDir = new File(contentsDir, "MacOS"); //$NON-NLS-1$
					if (macOSDir.isDirectory()) {
						final String exeName = jarFile.getName().substring(0, jarFile.getName().length() - ".jar".length()); //$NON-NLS-1$
						final File exeFile = new File(macOSDir, exeName);
						if (exeFile.isFile()) {
							return exeFile;
						}
					}
				}
			}
		}
		return null;
	}

    public static File getLanguagesDir() {
    	return languagesDir;
    }
    
	/** Recupera el directorio en el que se encuentra la aplicaci&oacute;n.
	 * @return Directorio de ejecuci&oacute;n. */
	private static File getApplicationDirectory() {

		// Identificamos el directorio de instalacion
		try {
			return new File(
				LanguageManager.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath()
			).getParentFile();
		}
		catch (final URISyntaxException e) {
			LOGGER.warning("No se pudo localizar el directorio del fichero en ejecucion: " + e); //$NON-NLS-1$
		}

		return null;
	}

}
