/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.Component;
import java.awt.EventQueue;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.plugins.OutputData;
import es.gob.afirma.standalone.plugins.PluginIntegrationWindow;
import es.gob.afirma.standalone.plugins.SignatureProcessAction;

/** Panel con el resultado de un proceso de firma masiva. */
public final class SignResultListPanel extends JPanel implements PluginButtonsContainer {

    /** Serial ID. */
	private static final long serialVersionUID = 1896328450345342947L;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Referencia a la aplicaci&oacute;n de firma. */
    private final SimpleAfirma saf;

    /** Bot&oacute;n para volver a la pantalla anterior. */
    private final JButton returnButton = new JButton();

    /** Panel principal en el que se muestran los botones de los plugins. */
    JPanel mainPluginsButtonsPanel = null;

    /** Panel concreto en el que se muestran los botones de los plugins. */
    JPanel pluginButtonsPanel = null;

    List<SignOperationConfig> currentSignConfigs = null;
    X509Certificate currentSigningCert = null;

    /**
     * Construye el panel con el resultado de un proceso de firma masiva.
     * @param simpleAfirma Componente principal de la aplicaci&oacute;n.
     * @param signConfig Listado de datos referentes a las operaciones realizadas.
     * @param outDir Directorio en el que se han almacenado las firmas.
     * @param signingCert Certificado utilizado para la firma.
     */
    public SignResultListPanel(SimpleAfirma simpleAfirma, List<SignOperationConfig> signConfig,
    		final File outDir, X509Certificate signingCert) {
    	this.saf = simpleAfirma;
    	this.currentSignConfigs = signConfig;
    	this.currentSigningCert = signingCert;
        createUI(signConfig, outDir, signingCert);
	}

	/** Agrega el contenido gr&aacute;fico al panel.
     * @param signature Firma creada.
     * @param sigPath Ruta del fichero firmado.
     * @param signingCert Certificado usado para firmar.
     * @param signValidity Validez de la firma. */
    private void createUI(final List<SignOperationConfig> signConfigList, final File outDir,
                          final X509Certificate signingCert) {

    	// Recorremos el listado para comprobar el estado de las firmas. Si todas estan bien,
    	// se mostrara el resultado de todo correcto; si hay alguna que haya fallado, no
    	// tendra definido cual es su fichero de salida y se mostrara que ha ocurrido un error
    	SignValidity validity = new SignValidity(SIGN_DETAIL_TYPE.OK, null);
    	for (int i = 0; i < signConfigList.size() && validity.getValidity() != SIGN_DETAIL_TYPE.KO; i++) {
    		final SignOperationConfig signConfig = signConfigList.get(i);
    		if (signConfig.getSignatureFile() == null) {
    			validity = new SignValidity(SIGN_DETAIL_TYPE.KO, null);
    			break;
    		}
    	}

        final SignResultPanel infoPanel = new SignResultPanel(validity, false, null);
        final JPanel componentPanel = new MassiveResultProcessPanel(signConfigList, outDir, signingCert);

        final JPanel returnPanel = new JPanel(true);
        returnPanel.setLayout(new BoxLayout(returnPanel, BoxLayout.Y_AXIS));

        this.returnButton.setText(SimpleAfirmaMessages.getString("SignDetailPanel.0")); //$NON-NLS-1$
        this.returnButton.setMnemonic('m');
        this.returnButton.setAlignmentX(Component.CENTER_ALIGNMENT);
        returnPanel.add(this.returnButton);
        this.returnButton.addActionListener(ae -> goToBack());

        // Establecemos la configuracion de color
        if (!LookAndFeelManager.HIGH_CONTRAST) {
            setBackground(LookAndFeelManager.WINDOW_COLOR);
            returnPanel.setBackground(LookAndFeelManager.WINDOW_COLOR);
        }

        // Agregamos un panel adicional en el que se mostraran los botones de los plugins
        this.mainPluginsButtonsPanel = buildMainPluginsButtonsPanel();

        setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;
        c.insets = new Insets(11, 11, 11, 11);
        add(infoPanel, c);
        c.weighty = 1.0;
        c.gridy++;
        c.insets = new Insets(0, 11, 11, 11);
        add(componentPanel, c);
        c.weighty = 0.0;
        c.gridy++;
        add(returnPanel, c);
        c.gridy++;
        add(this.mainPluginsButtonsPanel, c);

        refreshPluginButtonsContainer();

        this.returnButton.requestFocusInWindow();
    }

    /** Vuelve a la pantalla de selecci&oacute;n de fichero para la firma. */
    void goToBack() {
        this.saf.loadMainApp();
    }

    /**
	 * Construye el panel en el que se mostraran los botones propios de los plugins instalados.
	 * @return Panel para los botones de los plugins.
	 */
	private JPanel buildMainPluginsButtonsPanel() {
		final JPanel mainPanel = new JPanel(new GridBagLayout());

		this.pluginButtonsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
    	c.weightx = 1.0;
    	c.gridy = 0;
    	mainPanel.add(new JSeparator(SwingConstants.HORIZONTAL), c);
    	c.gridy++;
    	mainPanel.add(this.pluginButtonsPanel, c);

    	if (!LookAndFeelManager.HIGH_CONTRAST) {
    		mainPanel.setBackground(LookAndFeelManager.WINDOW_COLOR);
    		this.pluginButtonsPanel.setBackground(LookAndFeelManager.WINDOW_COLOR);
    	}

    	return mainPanel;
	}

	@Override
	public void refreshPluginButtonsContainer() {
		final List<PluginGraphicButton> pluginsButtons = PluginsUiComponentsBuilder.getPluginsButtons(
    			PluginIntegrationWindow.MULTI_RESULT);

		for (final PluginGraphicButton button : pluginsButtons) {
			button.getGraphicButton().addActionListener(new PluginButtonActionListener(
					this,
					this.currentSigningCert,
					(SignatureProcessAction) button.getButton().getAction()));
    	}

    	EventQueue.invokeLater(new Runnable() {
			@Override
			public void run() {
		        if (pluginsButtons.isEmpty()) {
		        	SignResultListPanel.this.mainPluginsButtonsPanel.setVisible(false);
		        }
		        else {
		        	SignResultListPanel.this.mainPluginsButtonsPanel.setVisible(false);
		        	SignResultListPanel.this.pluginButtonsPanel.removeAll();
		        	for (final PluginGraphicButton button : pluginsButtons) {
		        		SignResultListPanel.this.pluginButtonsPanel.add(button.getGraphicButton());
		        	}
		        	SignResultListPanel.this.mainPluginsButtonsPanel.setVisible(true);
		        }
			}
		});
	}

	/** Acci&oacute;n gen&eacute;rica que se asigna a cada bot&oacute;n de plugin
	 * que aparezca en la pantalla. */
	class PluginButtonActionListener implements ActionListener {

		final SignResultListPanel signResultPanel;
		final X509Certificate cert;
		final SignatureProcessAction action;

		public PluginButtonActionListener(SignResultListPanel signResultPanel, final X509Certificate signingCert, SignatureProcessAction action) {
			this.signResultPanel = signResultPanel;
			this.cert = signingCert;
			this.action = action;
		}

		@Override
		public void actionPerformed(ActionEvent e) {

			final List<SignOperationConfig> configs = this.signResultPanel.currentSignConfigs;
			final List<OutputData> datas = new ArrayList<>();
			for (final SignOperationConfig config : configs) {
				final OutputData data = new OutputData();
				data.setDataFile(config.getSignatureFile());
				data.setSignatureFormat(config.getSignatureFormatName());
				datas.add(data);
			}

			new Thread(new Runnable() {
				@Override
				public void run() {
					PluginButtonActionListener.this.action.processSignatures(
							datas.toArray(new OutputData[datas.size()]), PluginButtonActionListener.this.cert,
							SwingUtilities.getWindowAncestor(SignResultListPanel.this));
				}
			}).start();
		}
	}
}
