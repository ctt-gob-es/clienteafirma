package es.gob.afirma.triphase.server;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.Properties;
import java.util.logging.Logger;

/**
 * Manejador para el acceso a los datos del fichero de configuraci&oacute;n del servicio.
 */
public class ConfigManager {

	private static final String CONFIG_FILE = "tps_config.properties"; //$NON-NLS-1$

	private static final String OLD_CONFIG_FILE = "config.properties"; //$NON-NLS-1$

	/** Variable de entorno que determina el directorio en el que buscar el fichero de configuraci&oacute;n. */
	private static final String ENVIRONMENT_VAR_CONFIG_DIR = "clienteafirma.config.path"; //$NON-NLS-1$

	private static final String CONFIG_PARAM_DOCUMENT_MANAGER_CLASS = "document.manager"; //$NON-NLS-1$
	private static final String CONFIG_PARAM_DOCUMENT_CACHE_MANAGER_CLASS = "document.cache.manager"; //$NON-NLS-1$
	private static final String CONFIG_PARAM_ALLOW_ORIGIN = "Access-Control-Allow-Origin"; //$NON-NLS-1$

	/** Propiedad para configurar el proveedor de Apache o no. */
	private static final String CONFIG_PARAM_PROVIDER_APACHE = "xml.provider.apache"; //$NON-NLS-1$

	private static final String CONFIG_PARAM_VERIFICATION_KEY = "verification.key"; //$NON-NLS-1$
	/** Propiedad que indica si se deber habilitar o no la cach&eacute; */
	private static final String CONFIG_PARAM_CACHE_ENABLED = "cacheEnabled"; //$NON-NLS-1$

	private static final String CONFIG_PARAM_TEMP_DIR = "tmpdir"; //$NON-NLS-1$

	private static final String CONFIG_PARAM_CONCURRENT_MODE_ENABLE = "concurrent.enable"; //$NON-NLS-1$

	private static final String CONFIG_PARAM_CONCURRENT_TIMEOUT = "concurrent.timeout"; //$NON-NLS-1$

	private static final String CONFIG_PARAM_CONCURRENT_MAX_SIGNS = "concurrent.maxsigns"; //$NON-NLS-1$

	/** Propiedad que indica el n&uacute;mero m&aacute;ximo de p&aacute;ginas para comprobar un posible PDF Shadow Attack */
	private static final String CONFIG_PARAM_MAX_PAGES_TO_CHECK_PSA = "maxPagesToCheckShadowAttack"; //$NON-NLS-1$

	/** Propiedad que indica el n&uacute;mero m&aacute;ximo de documentos permitidos en un lote de firmas. */
	private static final String CONFIG_PARAM_BATCH_MAX_DOCUMENTS = "batch.maxDocuments"; //$NON-NLS-1$

	/** Propiedad que indica el tama&ntilde;o m&aacute;ximo global permitido para una petici&oacute;n */
	private static final String CONFIG_PARAM_BATCH_MAX_REQUEST_SIZE = "batch.maxSize"; //$NON-NLS-1$

	/**
	 * Propiedad que indica el tama&ntilde;o m&aacute;ximo permitido
	 * para la/las referencias a documentos en la petici&oacute;n
	 */
	private static final String CONFIG_PARAM_BATCH_MAX_REFERENCE_SIZE = "batch.maxReferenceSize"; //$NON-NLS-1$

	private static final long DEFAULT_CONCURRENT_TIMEOUT = 30;

	private static final int DEFAULT_CONCURRENT_MAXSIGNS = 10;

	/** N&uacute;mero de p&aacute;ginas por defecto en las que comprobar el PSA. */
	private static final int DEFAULT_PARAM_MAX_PAGES_TO_CHECK_PSA = 10;

	/** Valor para comprobar el PSA sobre todas las p&aacute;ginas. */
	private static final String VALUE_CHECK_ALL_PAGES = "all"; //$NON-NLS-1$


	/** Or&iacute;genes permitidos por defecto desde los que se pueden realizar peticiones al servicio. */
	private static final String ALL_ORIGINS_ALLOWED = "*"; //$NON-NLS-1$

	private static final String SYS_PROP_PREFIX = "${"; //$NON-NLS-1$

	private static final String SYS_PROP_SUFIX = "}"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final Properties config;

	static {

		String configDir;
		try {
			configDir = System.getProperty(ENVIRONMENT_VAR_CONFIG_DIR);
		}
		catch (final Exception e) {
			LOGGER.warning(
				"No se ha podido obtener el directorio del fichero de configuracion: " + e //$NON-NLS-1$
			);
			configDir = null;
		}

		if (configDir == null) {
			LOGGER.warning("No se definio la variable " + ENVIRONMENT_VAR_CONFIG_DIR + " con el directorio del fichero de configuracion"); //$NON-NLS-1$ //$NON-NLS-2$
		}

		// Cargamos la configuracion del servicio
		Properties configProperties = loadConfigFile(configDir, CONFIG_FILE);
		if (configProperties == null) {
			configProperties = loadConfigFile(configDir, OLD_CONFIG_FILE);
		}

		if (configProperties == null) {
			throw new RuntimeException("No se ha encontrado el fichero de configuracion del servicio"); //$NON-NLS-1$
		}

		config = new Properties();
		for (final String k : configProperties.keySet().toArray(new String[0])) {
			config.setProperty(k, mapSystemProperties(configProperties.getProperty(k)));
		}

		if (!config.containsKey(CONFIG_PARAM_DOCUMENT_MANAGER_CLASS)) {
			throw new IllegalArgumentException(
				"No se ha indicado el document manager o (" + CONFIG_PARAM_DOCUMENT_MANAGER_CLASS  //$NON-NLS-1$
				+ ") en el fichero de propiedades"  //$NON-NLS-1$
			);
		}

	}

	/** Intenta cargar un fichero propiedades del directorio proporcionado o, en caso de
	 * no encontrarlo ah&iacute;, se busca en el <i>classpath</i>.
	 * @param configDir Directorio del fichero de configuraci&oacute;n.
	 * @param configFilename Nombre del fichero de propedades.
	 * @return Propiedades cargadas o {@code null} si no se pudo cargar el fichero. */
	private static Properties loadConfigFile(final String configDir, final String configFilename) {

		Properties configProperties = null;

		if (configDir != null) {
			try {
				final File configFile = new File(configDir, configFilename).getCanonicalFile();
				try (final InputStream configIs = new FileInputStream(configFile);) {
					configProperties = new Properties();
					configProperties.load(configIs);
				}

				LOGGER.info("Se cargo el fichero de configuracion externo " + configFile.getAbsolutePath()); //$NON-NLS-1$
			}
			catch (final Exception e) {
				LOGGER.warning(
						"No se pudo cargar el fichero de configuracion " + configFilename + //$NON-NLS-1$
						" desde el directorio " + configDir + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
				configProperties = null;
			}
		}

		if (configProperties == null) {
			LOGGER.info(
				"Se cargara el fichero de configuracion " + configFilename + " desde el CLASSPATH" //$NON-NLS-1$ //$NON-NLS-2$
			);

			try (final InputStream configIs = SignatureService.class.getClassLoader().getResourceAsStream(configFilename);) {
				configProperties = new Properties();
				configProperties.load(configIs);
				LOGGER.info("Se cargo el fichero de configuracion interno " + configFilename); //$NON-NLS-1$
			}
			catch (final Exception e) {
				LOGGER.warning(
					"No se pudo cargar el fichero de configuracion " + configFilename + " desde el CLASSPATH: " + e //$NON-NLS-1$ //$NON-NLS-2$
				);
				configProperties = null;
			}
		}

		return configProperties;
	}

	public static String getDocManagerClassName() {
		return config.getProperty(CONFIG_PARAM_DOCUMENT_MANAGER_CLASS);
	}

	public static String getDocCacheManagerClassName() {
		return config.getProperty(CONFIG_PARAM_DOCUMENT_CACHE_MANAGER_CLASS);
	}

	public static String getAccessControlAllowOrigin() {
		return config.getProperty(CONFIG_PARAM_ALLOW_ORIGIN, ALL_ORIGINS_ALLOWED);
	}

	public static String isCacheEnabled() {
		return config.getProperty(CONFIG_PARAM_CACHE_ENABLED);
	}

	public static long getBatchMaxDocuments() {
		try {
			return Long.parseLong(config.getProperty(CONFIG_PARAM_BATCH_MAX_DOCUMENTS));
		} catch (final Exception e) {
			return 0;
		}
	}

	public static long getBatchMaxRequestSize() {
		try {
			return Long.parseLong(config.getProperty(CONFIG_PARAM_BATCH_MAX_REQUEST_SIZE));
		} catch (final Exception e) {
			return 0;
		}
	}

	public static long getBatchMaxReferenceSize() {
		try {
			return Long.parseLong(config.getProperty(CONFIG_PARAM_BATCH_MAX_REFERENCE_SIZE));
		} catch (final Exception e) {
			return 0;
		}
	}

	public static String getHMacKey() {
		final String verificationKey = config.getProperty(CONFIG_PARAM_VERIFICATION_KEY);
		return verificationKey != null && verificationKey.length() > 0 ? verificationKey : null;
	}

	public static Properties getConfig() {
		return (Properties) config.clone();
	}


	/**
	 * Mapea las propiedades del sistema que haya en el texto que se referencien de
	 * la forma: ${propiedad}
	 * @param text Texto en el que se pueden encontrar las referencias a las propiedades
	 * del sistema.
	 * @return Cadena con las particulas traducidas a los valores indicados como propiedades
	 * del sistema. Si no se encuentra la propiedad definida, no se modificar&aacute;
	 */
	private static String mapSystemProperties(final String text) {

		if (text == null) {
			return null;
		}

		int pos = -1;
		int pos2 = 0;
		String mappedText = text;
		while ((pos = mappedText.indexOf(SYS_PROP_PREFIX, pos + 1)) > -1 && pos2 > -1) {
			pos2 = mappedText.indexOf(SYS_PROP_SUFIX, pos + SYS_PROP_PREFIX.length());
			if (pos2 > pos) {
				final String prop = mappedText.substring(pos + SYS_PROP_PREFIX.length(), pos2);
				final String value = System.getProperty(prop, null);
				if (value != null) {
					mappedText = mappedText.replace(SYS_PROP_PREFIX + prop + SYS_PROP_SUFIX, value);
				}
			}
		}
		return mappedText;
	}

	public static File getTempDir() {
		if (config.getProperty(CONFIG_PARAM_TEMP_DIR) == null) {
			return null;
		}
		return new File(config.getProperty(CONFIG_PARAM_TEMP_DIR));
	}

	public static boolean isConcurrentModeEnable() {
		return Boolean.parseBoolean(config.getProperty(CONFIG_PARAM_CONCURRENT_MODE_ENABLE));
	}

	public static long getConcurrentTimeout() {
		try {
			return Long.parseLong(config.getProperty(CONFIG_PARAM_CONCURRENT_TIMEOUT));
		}
		catch (final Exception e) {
			return DEFAULT_CONCURRENT_TIMEOUT;
		}
	}

	public static int getConcurrentMaxSigns() {
		try {
			return Integer.parseInt(config.getProperty(CONFIG_PARAM_CONCURRENT_MAX_SIGNS));
		}
		catch (final Exception e) {
			return DEFAULT_CONCURRENT_MAXSIGNS;
		}
	}

	public static int getMaxPagesToCheckPSA() {
		int maxPages;
		final String maxPagesValue = config.getProperty(CONFIG_PARAM_MAX_PAGES_TO_CHECK_PSA);
		if (VALUE_CHECK_ALL_PAGES.equalsIgnoreCase(maxPagesValue)) {
			maxPages = Integer.MAX_VALUE;
		}
		else {
			try {
				maxPages = Integer.parseInt(maxPagesValue);
			}
			catch (final Exception e) {
				maxPages = DEFAULT_PARAM_MAX_PAGES_TO_CHECK_PSA;
			}
		}
		return maxPages;
	}

	public static boolean isProviderApacheConfigured() {
		return !Boolean.FALSE.toString().equalsIgnoreCase(config.getProperty(CONFIG_PARAM_PROVIDER_APACHE));
	}
}
