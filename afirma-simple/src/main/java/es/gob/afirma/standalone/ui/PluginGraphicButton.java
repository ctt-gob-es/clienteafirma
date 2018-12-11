package es.gob.afirma.standalone.ui;

import javax.swing.JButton;

import es.gob.afirma.standalone.plugins.PluginButton;

/**
 * Bot&oacute;n del plugin.
 */
public class PluginGraphicButton {

	private final JButton graphicButton;

	private final PluginButton button;

	/**
	 * Crea un bot&oacute;n para la ejecuci&oacute;n de una tarea del plugin.
	 * @param button Informaci&oacute;n del bot&oacute;n.
	 * @param graphicButton Componente gr&aacute;fico.
	 */
	public PluginGraphicButton(PluginButton button, JButton graphicButton) {
		this.button = button;
		this.graphicButton = graphicButton;
	}

	/**
	 * Obtienen el componente gr&aacute;fico con el que se muestra el bot&oacute;n.
	 * @return Componente de tipo bot&oacute;n.
	 */
	public JButton getGraphicButton() {
		return this.graphicButton;
	}


	/**
	 * Obtiene la informaci&oacute;n para la presentaci&oacute;n de un bot&oacute;n del plugin.
	 * @return Informaci&oacute;n del bot&oacute;n.
	 */
	public PluginButton getButton() {
		return this.button;
	}

}
