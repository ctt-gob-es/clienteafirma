package es.gob.afirma.standalone.configurator.common;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.prefs.BackingStoreException;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.http.DataDownloader;
import es.gob.afirma.standalone.configurator.common.PreferencesPlistHandler.InvalidPreferencesFileException;

public class ConfigUpdaterManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/**
	 * Indica si la configuraci&oacute;n est&aacute; actualizada.
	 * @return {@code true} si la configuraci&oacute;n est&aacute; actualizada, {@code false} en caso contrario.
	 */
	public static boolean needCheckConfigUpdates() {
		return PreferencesManager.needCheckConfigUpdates();
	}

	/**
	 * Guarda la configuraci&oacute;n base de la aplicaci&oacute;n a partir de una URL y establece
	 * la pol&iacute;tica de actualizaciones posteriores.
	 * @param url Direcci&oacute;n desde la que descargar la configuraci&oacute;n por defecto de la
	 * aplicaci&oacute;n.
	 * @param allowUpdates {@code true} indica que se debe comprobar cuando toque si hay una
	 * actualizaci&oacute;n de la configuraci&oacute;n en la URL indicada, {@code false} cuando no
	 * se desee actualizar esta configuraci&oacute;n posteriormente.
	 */
	public static void savePrefsConfigFile(final String url, final boolean allowUpdates) {
		try {
			final byte[] updatedConfigData = readFromUrl(url);
			final ConfigDataInfo configDataInfo = new ConfigDataInfo(updatedConfigData);

			// Importa la configuracion como nueva configuracion por defecto
			importSystemPreferences(updatedConfigData);

			// Actualizamos la informacion del fichero usado para la importacion y la fecha
			PreferencesManager.setConfigFileInfo(url, allowUpdates, configDataInfo);
			PreferencesManager.setConfigCheckDate();
			PreferencesManager.flushSystemPrefs();
		} catch (final Exception e) {
			LOGGER.log(Level.WARNING, "Error al actualizar el registro del fichero de configuracion", e); //$NON-NLS-1$
		}
	}

	/** Actualiza los registros y las preferencias del archivo de configuraci&oacute;n
	 * en caso de que sea necesario. */
	public static void updatePrefsConfigFile() {

		final String configUrl = PreferencesManager.getConfigFileUrl();

		// Descargamos el fichero de configuracion
		ConfigDataInfo configDataInfo;
		try {
			final byte[] updatedConfigData = readFromUrl(configUrl);
			configDataInfo = new ConfigDataInfo(updatedConfigData);

		} catch (final Exception e) {
			LOGGER.log(Level.WARNING, "Error al actualizar la configuracion por defecto a nivel de sistema", e); //$NON-NLS-1$
			return;
		}

		// Comprobamos si la configuracion es nueva y la actualizamos
		if (PreferencesManager.isNewConfigFile(configDataInfo)) {
			try {
				// Importa la configuracion como nueva configuracion por defecto
				LOGGER.info("Se actualiza la configuracion desde: " + LoggerUtil.getTrimStr(LoggerUtil.getCleanUserHomePath(configUrl))); //$NON-NLS-1$
				importSystemPreferences(configDataInfo.getData());

				// Actualizamos la informacion del fichero usado para la importacion
				PreferencesManager.setConfigFileInfo(configDataInfo);
			} catch (final Exception e) {
				LOGGER.log(Level.WARNING, "Error al actualizar la configuracion por defecto a nivel de sistema", e); //$NON-NLS-1$
			}
		}

		// Actualizamos la fecha de la ultima comprobacion para evitar que se reintente actualizar continuamente
		try {
			PreferencesManager.setConfigCheckDate();
			PreferencesManager.flushSystemPrefs();
		} catch (final Exception e) {
			LOGGER.log(Level.WARNING, "Error al actualizar el intento de actualizacion", e); //$NON-NLS-1$
		}
	}

	/**
	 * Importa las preferencias contenidas en el archivo indicado por par&aacute;metro.
	 * @param configData Datos del archivo de configuraci&oacute;n.
	 * @param configPath Ruta del fichero de configuraci&oacute;n.
	 * @throws InvalidPreferencesFileException Si el formato del fichero de configuraci&oacute;n no era v&aacute;lido.
	 * @throws BackingStoreException Si No se puede importar la configuraci&oacute;n.
	 */
	private static void importSystemPreferences(final byte[] configData) throws InvalidPreferencesFileException, BackingStoreException {
		PreferencesManager.clearAllSystemPrefs();
		PreferencesPlistHandler.importSystemPreferencesFromXml(configData);
	}


	/**
	 * Lee un archivo local o descarga un archivo remoto.
	 * @param filePath Ruta del archivo.
	 * @return Contenido del archivo.
	 * @throws IOException Cuando no se puede leer el archivo.
	 */
	private static byte[] readFromUrl(final String filePath) throws IOException {
		byte[] configData = null;
		if (filePath.toLowerCase(Locale.US).startsWith("https:")) { //$NON-NLS-1$
			try {
				configData = DataDownloader.downloadData(filePath, false, true);
			} catch(final Exception e) {
				throw new IOException("No se ha podido descargar el fichero del enlace remoto", e); //$NON-NLS-1$
			}
		} else {
			try (
					final InputStream fis = new FileInputStream(filePath);
					final InputStream bis = new BufferedInputStream(fis);
					)
			{
				configData = AOUtil.getDataFromInputStream(bis);
			} catch(final Exception e) {
				throw new IOException("No se ha podido leer el archivo", e); //$NON-NLS-1$
			}
		}

		return configData;
	}
}
