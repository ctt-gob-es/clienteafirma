package es.gob.afirma.signers.batch.json;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.triphase.server.ConfigManager;
import es.gob.afirma.triphase.server.cache.DocumentCacheManager;

/**
 * Factor&iacute;a con la que obtener un gestor para el guardado de datos en cach&eacute;.
 */
public class DocumentCacheFactory {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static Constructor<DocumentCacheManager> constructor = null;
	private static boolean needConfig = false;
	private static boolean initialized = false;

	/**
	 * Obtiene un gestor para el guardado de datos en cach&eacute;.
	 * @return Gestor para el guardado de datos en cach&eacute;.
	 */
	public static DocumentCacheManager newDocumentCacheManager() {

		// Si la cache no esta inicializada, la inicializamos
		if (!initialized) {
			try {
				loadCacheManagerConstructor();
			} catch (final IOException e) {
				LOGGER.log(Level.WARNING, "No se pudo inicializar la cache", e); //$NON-NLS-1$
			}
			initialized = true;
		}

		// Si no estada inicializada la cache, no podremos usarla
		if (constructor == null) {
			LOGGER.log(Level.WARNING, "No se usara cache por no haber podido inicializarla"); //$NON-NLS-1$
			return null;
		}

		// Creamos un nuevo gestor de cache
		DocumentCacheManager docCacheManager;
		try {
			if (needConfig) {
				docCacheManager = constructor.newInstance(ConfigManager.getConfig());
			}
			else {
				docCacheManager = constructor.newInstance();
			}
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se pudo construir el gestor de cache con la configuracion suministrada: " + e); //$NON-NLS-1$
			docCacheManager = null;
		}

		return docCacheManager;
	}

	/**
	 * Carga los elementos elementos necesarios para el uso de la cach&eacute;.
	 * @throws IOException Cuando no se pudieron inicializar los elementos para el uso de la cach&eacute;.
	 */
	private static void loadCacheManagerConstructor() throws IOException {


		final String docCacheManagerClassName = ConfigManager.getDocCacheManagerClassName();

		final Class<?> docCacheManagerClass;
		try {
			docCacheManagerClass = Class.forName(docCacheManagerClassName, false, DocumentCacheFactory.class.getClassLoader());
		}
		catch (final ClassNotFoundException e) {
			throw new IOException("La clase DocumentCacheManager indicada no existe: "+ docCacheManagerClassName, e); //$NON-NLS-1$
		}

		if (!DocumentCacheManager.class.isAssignableFrom(docCacheManagerClass)) {
			throw new IOException("Debe implementar la interfaz DocumentCacheManager la clase de cache configurada: " + docCacheManagerClassName); //$NON-NLS-1$
		}

		Constructor<?> docCacheManagerConstructor;
		try {
			 docCacheManagerConstructor = docCacheManagerClass.getConstructor(Properties.class);
			 needConfig = true;
		}
		catch (final Exception e) {
			try {
				docCacheManagerConstructor = docCacheManagerClass.getConstructor();
				needConfig = false;
			}
			catch (final Exception e2) {
				throw new IOException("La clase DocumentCacheManager debe tener un constructor vacio o que reciba un Properties", e); //$NON-NLS-1$
			}
		}

		LOGGER.info("Se usara la siguiente clase para el guardado en cache de los datos: " + docCacheManagerClassName); //$NON-NLS-1$

		constructor = (Constructor<DocumentCacheManager>) docCacheManagerConstructor;
	}
}
