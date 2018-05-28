package es.gob.afirma.standalone.plugins;

import java.util.Properties;

import javax.swing.JPanel;

/**
 * Panel base para establecer la configuraci&oacute;n de un plugin.
 */
public abstract class ConfiguratorPanel extends JPanel {

	/** Serial Id. */
	private static final long serialVersionUID = 4506966256726666945L;

	/**
	 * Proporciona la configuraci&oacute;n guardada asociada al plugin.
	 * @param config
	 */
	public abstract void init(Properties config);

	/**
	 * Obtiene la configuraci&oacute;n del plugin que debe guardarse.
	 * @return Configuraci&oacute;n que se desea establecer para el plugin.
	 */
	public abstract Properties getConfig();
}
