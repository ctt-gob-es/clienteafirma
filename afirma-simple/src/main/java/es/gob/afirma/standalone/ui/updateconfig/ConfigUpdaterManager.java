package es.gob.afirma.standalone.ui.updateconfig;

import java.security.MessageDigest;
import java.util.logging.Logger;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import es.gob.afirma.core.misc.http.DataDownloader;
import es.gob.afirma.standalone.ui.preferences.PreferencesManager;
import es.gob.afirma.standalone.ui.preferences.PreferencesPlistHandler;

public class ConfigUpdaterManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Objecto general de preferencias donde se guarda la configuraci&oacute;n de la
	 * aplicaci&oacute;n. */
	private static final Preferences SYSTEM_PREFERENCES;

	public static final String PREFERENCE_UPDATE_CONFIG_FILE_URL = "configFileUrl"; //$NON-NLS-1$
	public static final String PREFERENCE_UPDATE_CONFIG_FILE_SHA256 = "configFileSHA256"; //$NON-NLS-1$
	public static final String PREFERENCE_UPDATE_NEED_UPDATE_CONFIG = "needUpdateConfig"; //$NON-NLS-1$

	static {
		SYSTEM_PREFERENCES = Preferences.systemNodeForPackage(ConfigUpdaterManager.class);
	}

	/** Recupera la cadena con el valor de una propiedad de configuraci&oacute;n.
	 * @param key Clave del valor que queremos recuperar.
	 * @return El valor almacenado. */
	public static String get(final String key) {
		return SYSTEM_PREFERENCES.get(key, null);
	}

	/** Recupera el valor {@code true} o {@code false} almacenado entre las preferencias de la
	 * aplicaci&oacute;n.
	 * @param key Clave del valor que queremos recuperar.
	 * @return La preferencia almacenada o la configurada en el sistema si no se encontr&oacute;. */
	public static Boolean getBoolean(final String key) {
		final String value = SYSTEM_PREFERENCES.get(key, null);
		if (value != null) {
			return Boolean.parseBoolean(value);
		}
		return null;
	}

	/** Registra la cadena con el valor de una propiedad de configuraci&oacute;n.
	 * @param key Clave del valor que queremos registrar.
	 * @param value Valor que queremos registrar. */
	public static void put(final String key, final String value) {
		SYSTEM_PREFERENCES.put(key, value);
	}

	/** Registra la cadena con el valor booleano de una propiedad de configuraci&oacute;n.
	 * @param key Clave del valor que queremos registrar.
	 * @param value Valor que queremos registrar. */
	public static void putBoolean(final String key, final boolean value) {
		SYSTEM_PREFERENCES.putBoolean(key, value);
	}

	/** Almacena en las preferencias del sistema de la aplicaci&oacute;n todos los valores
	 * establecidos hasta el momento.
	 * @throws BackingStoreException Cuando ocurre un error durante el guardado. */
	public static void flushSystemPrefs() throws BackingStoreException {
		SYSTEM_PREFERENCES.flush();
	}

	/** Actualiza los registros y las preferencias del archivo de configuraci&oacute;n
	 * en caso de que sea necesario. */
	public static void updatePrefsConfigFile() {
		try {
			final byte[] updatedConfigData = DataDownloader.downloadData(get(PREFERENCE_UPDATE_CONFIG_FILE_URL));
			final String configFileSHA256 = get(PREFERENCE_UPDATE_CONFIG_FILE_SHA256);
			final byte [] hash =  MessageDigest.getInstance("SHA-256").digest(updatedConfigData); //$NON-NLS-1$
			final String newConfigDataHash = bytesToHex(hash);
			if (!configFileSHA256.equals(newConfigDataHash)) {
				importSystemPreferences(updatedConfigData);
				put(PREFERENCE_UPDATE_CONFIG_FILE_SHA256, newConfigDataHash);
			}
		} catch (final Exception e) {
			LOGGER.warning(
					"Error al actualizar el registro del fichero de configuracion: " + e //$NON-NLS-1$
				);
		}
	}

	/** Transforma los bytes de un hash a formato hexadecimal.
	 * @param hash Hash a transformar.
	 * @return Hash en formato hexadecimal. */
	public static String bytesToHex(final byte[] hash) {
	    final StringBuilder hexString = new StringBuilder(2 * hash.length);
	    for (int i = 0; i < hash.length; i++) {
	        final String hex = Integer.toHexString(0xff & hash[i]);
	        if(hex.length() == 1) {
	            hexString.append('0');
	        }
	        hexString.append(hex);
	    }
	    return hexString.toString();
	}

	/** Importa las preferencias contenidas en el archivo indicado por par&aacute;metro.
	 * @param configData Datos del archivo de configuraci&oacute;n.
	 * @param configPath Ruta del fichero de configuraci&oacute;n. */
	public static void importSystemPreferences(final byte[] configData) {
		try {
			PreferencesManager.clearAllSystemPrefs();
			PreferencesPlistHandler.importSystemPreferencesFromXml(new String(configData), false);
		}
		catch (final Exception e) {
			LOGGER.warning(
				"Error al importar preferencias al sistema desde fichero de configuracion: " + e //$NON-NLS-1$
			);
		}
	}

}
