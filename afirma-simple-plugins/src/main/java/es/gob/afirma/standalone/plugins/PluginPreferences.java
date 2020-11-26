package es.gob.afirma.standalone.plugins;

import java.util.Properties;
import java.util.logging.Logger;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

/**
 * Gestiona la configuraci&oacute;n almacenada del plugin.
 */
public class PluginPreferences {

	private final Preferences preferences;

	private static PluginPreferences instance = null;

	/**
	 * Carga la configuraci&oacute;n del plugin.
	 * @param plugin El plugin en ejecuci&oacute;n.
	 * @return Preferencias del plugin.
	 */
	public static PluginPreferences getInstance(final AfirmaPlugin plugin) {
		if (instance == null) {
			instance = new PluginPreferences(plugin);
		}
		return instance;
	}

	private PluginPreferences(final AfirmaPlugin plugin) {
		this.preferences = Preferences.userNodeForPackage(plugin.getClass());
	}

	/**
	 * Almacenamos la configuraci&oacute;n del plugin.
	 * @param config Configuraci&oacute;n que deseamos almacenar.
	 */
	public void saveConfig(final Properties config) {
		// Eliminamos la configuracion que hubiese antes
		try {
			for (final String key : this.preferences.childrenNames()) {
				this.preferences.remove(key);
			}
		} catch (final BackingStoreException e) {
			Logger.getLogger(PluginPreferences.class.getName()).warning(
					"No se pudo eliminar la configuracion anterior del plugin: " + e); //$NON-NLS-1$
		}

		// Almacenamos la nueva configuracion
		for (final String key : config.keySet().toArray(new String[config.size()])) {
			this.preferences.put(key, config.getProperty(key));
		}
		try {
			this.preferences.flush();
		} catch (final BackingStoreException e) {
			Logger.getLogger(PluginPreferences.class.getName()).warning(
					"No se pudo guardar la nueva configuracion del plugin: " + e); //$NON-NLS-1$
		}
	}

	/**
	 * Obtiene la configuraci&oacute;n almacenada del plugin.
	 * @return Configuraci&oacute;n del plugin.
	 */
	public Properties recoverConfig() {
		final Properties config = new Properties();
		try {
			for (final String key : this.preferences.keys()) {
				config.setProperty(key, this.preferences.get(key, "")); //$NON-NLS-1$
			}
		} catch (final BackingStoreException e) {
			Logger.getLogger(PluginPreferences.class.getName()).warning(
					"No se pudo recuperar toda la configuracion del plugin: " + e); //$NON-NLS-1$
		}
		return config;
	}
}
