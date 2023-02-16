package es.gob.afirma.standalone.plugins;

import java.awt.Window;
import java.util.Properties;

/**
 * Define a nivel generico una accion que se puede realizar desde un bot&oacute;n de un plugin.
 * Para definir una accion personalizada, se deber&iacute;a implementar alguna de las subinterfaces
 * que extienden a esta.
 */
public abstract class PluginAction {

	private AfirmaPlugin plugin = null;

	public final void setPlugin(final AfirmaPlugin plugin) {
		this.plugin = plugin;
	}

	/**
	 * Obtiene la configuraci&oacute;n establecida para el plugin a traves de su panel
	 * de configuraci&oacute;n.
	 * @return Configuraci&oacute;n establecida.
	 */
	protected final Properties getConfig() {
		if (this.plugin != null) {
			return PluginPreferences.getInstance(this.plugin).recoverConfig();
		}
		return new Properties();
	}

	/**
	 * Actualiza la configuraci&oacute;n proporcionada para el plugin, permitiendo que se actualice a
	 * traves de los di&aacute;logos y acciones que ejecuta.
	 * @param config Configuraci&oacute;n que debe establecerse.
	 */
	protected final void saveConfig(final Properties config) {
		if (this.plugin != null) {
			PluginPreferences.getInstance(this.plugin).saveConfig(config);
		}
	}

	@SuppressWarnings("unused")
	public void start(final Window parent) {
		// Por defecto, no se hace nada
	}

}
