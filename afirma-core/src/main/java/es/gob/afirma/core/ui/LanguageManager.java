package es.gob.afirma.core.ui;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ErrorCode;
import es.gob.afirma.core.misc.AOFileUtils;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.Platform;

/**
 * Gestiona los idiomas
 */
public class LanguageManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String METADATA_FILENAME = "metadata.info"; //$NON-NLS-1$

	public static final String LOCALE_PROP = "locale"; //$NON-NLS-1$

	public static final String LANGUAGE_NAME_PROP = "language.name"; //$NON-NLS-1$

	private static final String FALLBACK_LOCALE = "fallback.locale"; //$NON-NLS-1$

	private static final File HELP_DIR = new File(Platform.getUserHome() + File.separator + ".afirma" + File.separator //$NON-NLS-1$
			+ "Autofirma" + File.separator + "help"); //$NON-NLS-1$ //$NON-NLS-2$

	private static File languagesDir;

	public static final Locale [] AFIRMA_DEFAULT_LOCALES = {
	    	new Locale("es", "ES"), //$NON-NLS-1$ //$NON-NLS-2$     
	        new Locale("ca", "ES"), //$NON-NLS-1$ //$NON-NLS-2$
	        new Locale("gl", "ES"), //$NON-NLS-1$ //$NON-NLS-2$
	        new Locale("eu", "ES"), //$NON-NLS-1$ //$NON-NLS-2$
	        new Locale("va", "ES"),  //$NON-NLS-1$ //$NON-NLS-2$
	        new Locale("en", "US") //$NON-NLS-1$ //$NON-NLS-2$
	};

	public static void init(final File langDir) {
		languagesDir = langDir;
	}

	/**
	 * Importa y agrega un nuevo idioma.
	 * @param langFile Fichero de idioma.
	 * @throws AOException Error al importar idioma.
	 * @return Propiedades del idioma.
	 */
	public static Map<String, String> addLanguage(final File langFile) throws AOException {

		Map<String, String> langProps = null;

		try {
			langProps = readMetadataInfo(langFile);
		} catch (final Exception e) {
			throw new AOException(new ErrorCode("230001")); //$NON-NLS-1$
		}

		final String localeName = langProps.get(LOCALE_PROP);

		try {
			copyLanguageToDirectory(langFile, new File(languagesDir, localeName));
		} catch (final Exception e) {
			throw new AOException(new ErrorCode("230001")); //$NON-NLS-1$
		}
		
		return langProps;
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
	private static File copyLanguageToDirectory(final File langFile, final File langDir) throws IOException {

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
	        final Map<String, String> langProps = new HashMap<>();

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
					outputFile = new File(HELP_DIR, entryName.substring(("help" + File.separator).length())); //$NON-NLS-1$
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


    public static File getLanguagesDir() {
    	return languagesDir;
    }

}
