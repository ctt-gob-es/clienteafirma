package es.gob.afirma.signfolder.server.proxy;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.logging.Logger;

/**
 * Configuraci&oacute;n para la gesti&oacute;n del almacenamiento temporal de ficheros en servidor.
 */
public class RetrieveConfig {

	/** Clave para la configuraci&oacute;n del directorio para la creacion de ficheros temporales. */
	static final String TMP_DIR_KEY =  "tmpDir"; //$NON-NLS-1$

	/** Directorio temporal por defecto. */
	private static String DEFAULT_TMP_DIR;

	/** Clave para la configuraci&oacute;n del tiempo de caducidad de los ficheros temporales. */
	private static final String EXPIRATION_TIME_KEY =  "expTime"; //$NON-NLS-1$

	/** Milisegundos que, por defecto, tardan los mensajes en caducar. */
	private static final long DEFAULT_EXPIRATION_TIME = 60000; // 1 minuto

	private final Properties config;

	static {
		try {
			DEFAULT_TMP_DIR = System.getProperty("java.io.tmpdir"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			try {
				DEFAULT_TMP_DIR = File.createTempFile("tmp", null).getParentFile().getAbsolutePath(); //$NON-NLS-1$
			}
			catch (final Exception e1) {
				DEFAULT_TMP_DIR = null;
				Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
					"No se ha podido cargar un directorio temporal por defecto, se debera configurar expresamente en el fichero de propiedades: "  + e1 //$NON-NLS-1$
				);
			}
		}
	}

	/**
	 * Crear el objeto de configuracion para el servicio de almacenamiento.
	 * @param context Contexto del servlet.
	 */
	public RetrieveConfig() {
		this.config = new Properties();
	}

	/**
	 * Carga del
	 * @param path Ruta del fichero de configuraci&oacute;n (si existe).
	 * @throws FileNotFoundException Cuando no se encuentra el fichero de configuraci&oacute;n.
	 * @throws IOException Cuanto ocurre un error durante la lectura del fichero.
	 */
	public void load(final String path) throws FileNotFoundException, IOException {
		if (path != null) {
 			try {
 				final InputStream is = RetrieveConfig.class.getClassLoader().getResourceAsStream(path);
				this.config.load(is);
				is.close();
			}
 			catch (final IOException e) {
				Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
					"No se ha podido cargar el fichero con las propiedades: " + e.toString() //$NON-NLS-1$
				);
			}
		}
	}

	/**
	 * Recupera el directorio configurado para la creaci&oacute;n de ficheros temporales o el por defecto.
	 * @return Directorio temporal.
	 * @throws NullPointerException Cuando no se indicala ruta del directorio temporal ni se puede obtener
	 * del sistema.
	 */
	public File getTempDir() {
		File tmpDir = new File(this.config.getProperty(TMP_DIR_KEY, DEFAULT_TMP_DIR).trim());
		if (!tmpDir.exists() ||!tmpDir.canRead()) {
			Logger.getLogger("es.gob.afirma").warning("El directorio temporal indicado en el fichero de propiedades no existe, se usara el por defecto"); //$NON-NLS-1$ //$NON-NLS-2$
			tmpDir = new File(DEFAULT_TMP_DIR);
			if (!tmpDir.exists() ||!tmpDir.canRead()) {
				throw new IllegalStateException("No se ha podido definir un directorio temporal"); //$NON-NLS-1$
			}
		}
		return tmpDir;
	}

	/**
	 * Recupera el tiempo en milisegundos que puede almacenarse un fichero antes de considerarse caducado.
	 * @return Tiempo m&aacute;ximo en milisegundos que puede tardarse en recoger un fichero antes de que
	 * caduque.
	 */
	public long getExpirationTime() {
		try {
			return this.config.containsKey(EXPIRATION_TIME_KEY) ?
					Long.parseLong(this.config.getProperty(EXPIRATION_TIME_KEY)) : DEFAULT_EXPIRATION_TIME;
		} catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").warning("Tiempo de expiracion invalido en el fichero de configuracion, se usara" + DEFAULT_EXPIRATION_TIME); //$NON-NLS-1$ //$NON-NLS-2$
			return DEFAULT_EXPIRATION_TIME;
		}
	}
}
