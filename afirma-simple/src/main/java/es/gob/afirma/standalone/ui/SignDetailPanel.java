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
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.security.cert.X509Certificate;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.crypto.CompleteSignInfo;
import es.gob.afirma.standalone.plugins.OutputData;
import es.gob.afirma.standalone.plugins.PluginAction;
import es.gob.afirma.standalone.plugins.SignatureProcessAction;

/** Panel con detalles de una firma electr&oacute;nica. */
public final class SignDetailPanel extends JPanel implements PluginButtonsContainer {

    /** Serial ID. */
    private static final long serialVersionUID = 7567869419737753210L;

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Anchura m&iacute;nima que deber&aacute; tener el panel. */
	private static final int DEFAULT_WINDOW_WIDTH = 600;
	/** Altura m&iacute;nima que deber&aacute; tener el panel. */
	private static final int DEFAULT_WINDOW_HEIGHT = 500;

    /** Referencia a la aplicaci&oacute;n de firma. */
    private final SimpleAfirma saf;

    /** Bot&oacute;n para volver a la pantalla anterior. */
    private final JButton returnButton = new JButton();

    /** Panel principal en el que se muestran los botones de los plugins. */
    JPanel mainPluginsButtonsPanel = null;

    /** Panel concreto en el que se muestran los botones de los plugins. */
    JPanel pluginButtonsPanel = null;

    /**
     * Panel en el que se muestra la informaci&oacute;n de la firma.
     */
    SignDataPanel componentPanel;

    SignOperationConfig signConfig = null;

    private X509Certificate signingCert = null;

    /** Construye el panel para mostrar el detalle de una firma electr&oacute;nica.
     * @param saf Referencia a la misma aplicaci&oacute;n
     * @param sig Firma electr&oacute;nica que se desea visualizar
     * @param signConfig Configuraci&oacute;n de firma utilizada.
     *        usa para cargarla
     * @param signingCert Certificado usado para generar la &uacute;ltima firma
     * @param signValidity Tipo de panel informativo de cabecera
     * @param fileTypeIcon Icono vectorial indicativo del tipo de contenido. Si es <code>null</code> se determina al vuelo y se usa una version
     *        <i>raster</i> */
    public SignDetailPanel(final SimpleAfirma saf,
                           final byte[] sig,
                           final SignOperationConfig signConfig,
                           final X509Certificate signingCert,
                           final List<SignValidity> signValidity,
                           final JComponent fileTypeIcon) {
        this.saf = saf;

        this.signConfig = signConfig;
        this.signingCert = signingCert;

        createUI(sig, signConfig, signingCert, signValidity, fileTypeIcon);
    }

    /** Agrega el contenido gr&aacute;fico al panel.
     * @param signature Firma creada.
     * @param config Configuraci&oacute;n y resultado de la firma generada.
     * @param cert Certificado usado para firmar.
     * @param signValidity Validez de la firma.
     * @param fileTypeIcon Icono del fichero firmado (dependiente de su tipo). */
    private void createUI(final byte[] signature,
    					  final SignOperationConfig config,
                          final X509Certificate cert,
                          final List<SignValidity> signValidity,
                          final JComponent fileTypeIcon) {

    	setPreferredSize(new Dimension(DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT));

        byte[] sig = signature != null ? signature.clone() : null;

        // Cargamos los datos de firma si no nos los proporcionaron en el constructor
        if (sig == null && config != null && config.getSignatureFile() != null) {
            if (!config.getSignatureFile().exists()) {
            	LOGGER.severe("La ruta de firma proporcionada no corresponde a ningun fichero");  //$NON-NLS-1$
            }
            else if (!config.getSignatureFile().canRead()) {
                LOGGER.severe("No se tienen permisos de lectura del fichero indicado");  //$NON-NLS-1$
            }
            else {
                try (
            		InputStream fis = new FileInputStream(config.getSignatureFile());
            		InputStream bis = new BufferedInputStream(fis);
        		) {
                    sig = AOUtil.getDataFromInputStream(bis);
                }
                catch (final IOException e) {
                    LOGGER.severe("No se ha podido leer el fichero de firma: " + e); //$NON-NLS-1$
                }
            }
        }

        final JPanel infoPanel = new SignResultPanel(signValidity, true, null, sig);
        this.componentPanel = new SignDataPanel(config.getSignatureFile(), sig, fileTypeIcon, cert, null, config.getExtraParams());

        final JPanel buttonPanel = new JPanel(true);
        buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.Y_AXIS));

        this.returnButton.setText(SimpleAfirmaMessages.getString("SignDetailPanel.0")); //$NON-NLS-1$
        this.returnButton.setMnemonic('m');
        this.returnButton.setAlignmentX(Component.CENTER_ALIGNMENT);
        buttonPanel.add(this.returnButton);
        this.returnButton.addActionListener(ae -> goToBack());

        // Establecemos la configuracion de color
        if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST && Platform.getOS() == Platform.OS.WINDOWS) {
            setBackground(LookAndFeelManager.DEFAULT_COLOR);
            buttonPanel.setBackground(LookAndFeelManager.DEFAULT_COLOR);
        }

        // Agregamos un panel adicional en el que se mostraran los botones de los plugins
        this.mainPluginsButtonsPanel = buildMainPluginsButtonsPanel();

        // Agregamos al panel principal los componentes creados
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
        add(this.componentPanel, c);
        c.weighty = 0.0;
        c.gridy++;
        add(buttonPanel, c);
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

    	if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST && Platform.getOS() == Platform.OS.WINDOWS) {
    		mainPanel.setBackground(LookAndFeelManager.DEFAULT_COLOR);
    		this.pluginButtonsPanel.setBackground(LookAndFeelManager.DEFAULT_COLOR);
    	}

    	return mainPanel;
	}

	@Override
	public void refreshPluginButtonsContainer() {
		final List<PluginGraphicButton> pluginsButtons = PluginsUiComponentsBuilder.getPluginsButtons(
    			PluginIntegrationWindow.SINGLE_RESULT);

		for (final PluginGraphicButton button : pluginsButtons) {
			final PluginAction action = button.getButton().getAction();
    		button.getGraphicButton().addActionListener(new PluginButtonActionListener(this, this.signingCert, action));
    	}

    	EventQueue.invokeLater(() -> {
		    if (pluginsButtons.isEmpty()) {
		    	SignDetailPanel.this.mainPluginsButtonsPanel.setVisible(false);
		    }
		    else {
		    	SignDetailPanel.this.mainPluginsButtonsPanel.setVisible(false);
		    	SignDetailPanel.this.pluginButtonsPanel.removeAll();
		    	for (final PluginGraphicButton button : pluginsButtons) {
		    		SignDetailPanel.this.pluginButtonsPanel.add(button.getGraphicButton());
		    	}
		    	SignDetailPanel.this.mainPluginsButtonsPanel.setVisible(true);
		    }
		});
	}

	class PluginButtonActionListener implements ActionListener {

		final SignDetailPanel signDetailPanel;
		final X509Certificate cert;
		final PluginAction action;

		public PluginButtonActionListener(final SignDetailPanel signDetailPanel, final X509Certificate signingCert, final PluginAction action) {
			this.signDetailPanel = signDetailPanel;
			this.cert = signingCert;
			this.action = action;
		}

		@Override
		public void actionPerformed(final ActionEvent e) {

			final OutputData data = new OutputData();
			data.setDataFile(this.signDetailPanel.signConfig.getSignatureFile());
			data.setSignatureFormat(this.signDetailPanel.signConfig.getSignatureFormatName());

			final Map<BigInteger, X509Certificate> certs = new HashMap<>();
			final CompleteSignInfo signInfo = this.signDetailPanel.componentPanel.getCurrentSignInfo();
			final AOTreeModel tree = signInfo.getSignsTree();
			for (int i = 0; i < AOTreeModel.getChildCount(tree.getRoot()); i++) {
				readCertsFromBranch((AOTreeNode) AOTreeModel.getChild(tree.getRoot(), i), certs);
			}
			data.setCerts(certs.values().toArray(new X509Certificate[certs.size()]));

			new Thread(() -> {
				if (PluginButtonActionListener.this.action instanceof SignatureProcessAction) {
					((SignatureProcessAction) PluginButtonActionListener.this.action).processSignatures(
							new OutputData[] { data }, PluginButtonActionListener.this.cert,
							SwingUtilities.getWindowAncestor(SignDetailPanel.this));
				}
				else {
					PluginButtonActionListener.this.action.start(
							SwingUtilities.getWindowAncestor(SignDetailPanel.this));
				}
			}).start();
		}

		private void readCertsFromBranch(final AOTreeNode node, final Map<BigInteger, X509Certificate> certs) {
			final AOSimpleSignInfo signInfo = (AOSimpleSignInfo) node.getUserObject();
			if (signInfo.getCerts() != null && signInfo.getCerts().length > 0
					&& signInfo.getCerts()[0] != null) {
				certs.put(signInfo.getCerts()[0].getSerialNumber(), signInfo.getCerts()[0]);
			}

			for (int i = 0; i < AOTreeModel.getChildCount(node); i++) {
				readCertsFromBranch((AOTreeNode) AOTreeModel.getChild(node, i), certs);
			}
		}
	}

}
