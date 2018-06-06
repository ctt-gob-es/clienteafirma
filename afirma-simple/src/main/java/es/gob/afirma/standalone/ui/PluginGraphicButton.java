package es.gob.afirma.standalone.ui;

import javax.swing.JButton;

import es.gob.afirma.standalone.plugins.PluginButton;

public class PluginGraphicButton {

	private final JButton graphicButton;

	private final PluginButton button;

	public PluginGraphicButton(PluginButton button, JButton graphicButton) {
		this.button = button;
		this.graphicButton = graphicButton;
	}

	public JButton getGraphicButton() {
		return this.graphicButton;
	}


	public PluginButton getButton() {
		return this.button;
	}

}
