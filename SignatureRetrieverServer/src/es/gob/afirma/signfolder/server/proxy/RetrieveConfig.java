package es.gob.afirma.signfolder.server.proxy;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.logging.Logger;

import javax.servlet.ServletContext;

/**
 * Configuraci&oacute;n para la gesti&oacute;n del almacenamiento temporal de ficheros en servidor.
 */
public class RetrieveConfig {

	/** Clave para la configuraci&oacute;n del directorio para la creacion de ficheros temporales. */
	private static final String TMP_DIR_KEY =  "tmpDir"; //$NON-NLS-1$

	/** Directorio temporal por defecto. */
	private static final String DEFAULT_TMP_DIR = System.getProperty("java.io.tmpdir"); //$NON-NLS-1$

	/** Clave para la configuraci&oacute;n del tiempo de caducidad de los ficheros temporales. */
	private static final String EXPIRATION_TIME_KEY =  "expTime"; //$NON-NLS-1$

	/** Milisegundos que, por defecto, tardan los mensajes en caducar. */
	private static final long DEFAULT_EXPIRATION_TIME = 5000; // 5 segundos

	/** Clave para la configuraci&oacute;n del tiempo m&iacute;nimo entre proceso de limpieza del directorio temporal. */
	private static final String REMOVE_PROCESS_INTERVAL_KEY = "remInt"; //$NON-NLS-1$

	/** Milisegundos que, por defecto, deben trascurrir entre cada proceso de limpieza del directorio temporal. */
	private static final long DEFAULT_REMOVE_PROCESS_INTERVAL = 10000; // 10 segundos

	private final ServletContext context;

	private final Properties config;

	/**
	 * Crear el objeto de configuracion para el servicio de almacenamiento.
	 * @param context Contexto del servlet.
	 */
	public RetrieveConfig(final ServletContext context) {
		this.config = new Properties();
		this.context = context;
	}

	/**
	 * Carga del
	 * @param path Ruta del fichero de configuraci&oacute;n (si existe).
	 * @throws FileNotFoundException Cuando no se encuentra el fichero de configuraci&oacute;n.
	 * @throws IOException Cuanto ocurre un error durante la lectura del fichero.
	 */
	public void load(final String path) throws FileNotFoundException, IOException {
		if (path != null) {
 			try (final InputStream is = this.context.getResourceAsStream(path)) {
				this.config.load(is);
			} catch (final IOException e) {
				Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
						"No se ha podido cargar el fichero con las propiedades: " + e.toString()); //$NON-NLS-1$
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
		return new File(this.config.getProperty(TMP_DIR_KEY, DEFAULT_TMP_DIR));
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

	/**
	 * Recupera el tiempo que debe transcurrir como m&iacute;nimo entre cada proceso de limpieza del
	 * directorio temporal.
	 * @return Tiempo m&iacute;nimo en milisegundos que debe transcurrir entre cada proceso de limpieza.
	 */
	public long getRemoveProcessInterval() {
		try {
			return this.config.containsKey(REMOVE_PROCESS_INTERVAL_KEY) ?
					Long.parseLong(this.config.getProperty(REMOVE_PROCESS_INTERVAL_KEY)) : DEFAULT_REMOVE_PROCESS_INTERVAL;
		} catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").warning("Intervalor de proceso invalido en el fichero de configuracion, se usara" + DEFAULT_REMOVE_PROCESS_INTERVAL); //$NON-NLS-1$ //$NON-NLS-2$
			return DEFAULT_REMOVE_PROCESS_INTERVAL;
		}
	}
}
