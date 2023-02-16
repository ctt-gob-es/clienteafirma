package es.gob.afirma.standalone.plugins.manager;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;
import java.util.prefs.Preferences;

import es.gob.afirma.standalone.plugins.AfirmaPlugin;

/**
 * Gestiona las preferencias de los plugins instalados.
 */
public class PluginsPreferences {

	private final Preferences preferences;

	private final String name;

	private static Map<String, PluginsPreferences> instances = null;

	/**
	 * Obtiene el manejador de preferencias del plugin indicado.
	 * @param plugin Plugin del que se desean obtener las preferencias.
	 * @return Preferencias del plugin.
	 */
	public static PluginsPreferences getInstance(AfirmaPlugin plugin) {

		final String name = plugin.getInfo().getInternalName();

		if (instances == null) {
			instances = new HashMap<>();
		}
		if (!instances.containsKey(name)) {
			instances.put(name, new PluginsPreferences(plugin));
		}
		return instances.get(name);
	}

	private PluginsPreferences(AfirmaPlugin plugin) {
		this.name = plugin.getInfo().getInternalName();
		this.preferences = Preferences.userNodeForPackage(plugin.getClass());
	}

	/**
	 * Almacenamos la configuraci&oacute;n del plugin.
	 * @param config Configuraci&oacute;n que deseamos almacenar.
	 */
	public void saveConfig(Properties config) {
		// Eliminamos la configuracion que hubiese antes
		try {
			for (final String key : this.preferences.childrenNames()) {
				this.preferences.remove(key);
			}
		} catch (final Exception e) {
			Logger.getLogger(PluginsPreferences.class.getName()).warning(
					"No se pudo eliminar la configuracion anterior del plugin: " + e); //$NON-NLS-1$
		}

		// Almacenamos la nueva configuracion
		for (final String key : config.keySet().toArray(new String[config.size()])) {
			this.preferences.put(key, config.getProperty(key));
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
		} catch (final Exception e) {
			Logger.getLogger(PluginsPreferences.class.getName()).warning(
					"No se pudo recuperar toda la configuracion del plugin: " + e); //$NON-NLS-1$
		}
		return config;
	}

	/**
	 * Elimina la configuraci&oacute;n almacenada del plugin.
	 */
	public void removeConfig() {
		try {
			this.preferences.removeNode();
		} catch (final Exception e) {
			Logger.getLogger(PluginsPreferences.class.getName()).warning(
					"No se pudo eliminar la configuracion del plugin: " + e); //$NON-NLS-1$
		}
		instances.remove(this.name);
	}
}
