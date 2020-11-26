package es.gob.afirma.plugin.hash.action;

import java.awt.Window;
import java.util.Properties;

import es.gob.afirma.plugin.hash.CreateHashFileDialog;
import es.gob.afirma.standalone.plugins.PluginAction;

/**
 * Clase del plugin de huella digital encargada de gestionar la acci&oacute; de calcular la huella digital de un fichero.
 */
public class CalculateHashFileAction extends PluginAction {

	@Override
	public void start(final Window parent) {
		final Properties config = getConfig();
		
		for (final String k : config.keySet().toArray(new String[0])) {
			System.out.println(k + ": " + config.getProperty(k));
		}
		
		CreateHashFileDialog.startHashCreation(parent, config);

		for (final String k : config.keySet().toArray(new String[0])) {
			System.out.println(k + ": " + config.getProperty(k));
		}

		saveConfig(config);
	}

}
