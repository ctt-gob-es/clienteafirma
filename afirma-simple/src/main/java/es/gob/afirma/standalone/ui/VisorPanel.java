/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.RuntimeConfigNeededException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.cms.AOCMSSigner;
import es.gob.afirma.signers.odf.AOODFSigner;
import es.gob.afirma.signers.ooxml.AOOOXMLSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.xades.AOFacturaESigner;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.signers.xmldsig.AOXMLDSigSigner;
import es.gob.afirma.signvalidation.SignValider;
import es.gob.afirma.signvalidation.SignValiderFactory;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signvalidation.SignValidity.VALIDITY_ERROR;
import es.gob.afirma.standalone.DataAnalizerUtil;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.VisorFirma;
import es.gob.afirma.standalone.crypto.CompleteSignInfo;
import es.gob.afirma.standalone.plugins.OutputData;
import es.gob.afirma.standalone.plugins.PluginAction;
import es.gob.afirma.standalone.plugins.SignatureProcessAction;

/** Visor de firmas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s.
 * @author Carlos Gamuci. */
public final class VisorPanel extends JPanel implements KeyListener, PluginButtonsContainer {

    /** Version ID. */
    private static final long serialVersionUID = 8309157734617505338L;

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Anchura m&iacute;nima que deber&aacute; tener el panel. */
	private static final int DEFAULT_WINDOW_WIDTH = 600;
	/** Altura m&iacute;nima que deber&aacute; tener el panel. */
	private static final int DEFAULT_WINDOW_HEIGHT = 540;

    private final VisorFirma visorFirma;

    /** Panel principal en el que se muestran los botones de los plugins. */
    JPanel mainPluginsButtonsPanel = null;

    /** Panel concreto en el que se muestran los botones de los plugins. */
    JPanel pluginButtonsPanel = null;

    SignDataPanel signDataPanel = null;

    /** Fichero cargado actualmente. Puede ser nulo si se cargaron datos en memoria. */
    File signatureFile = null;


    /** Construye un panel con la informaci&oacute;n extra&iacute;da de una firma. Si no se
     * indica la firma, esta se cargar&aacute; desde un fichero. Es obligatorio introducir
     * alguno de los dos par&aacute;metros.
     * @param signFile Fichero de firma.
     * @param sign Firma.
     * @param vf VisorFirma para las acciones de los botones
     * @param allowReload <code>true</code> si se desea dar a usuario la opci&oacute;n de ver otras firmas en el
     *                    visor carg&aacute;ndolas mediante un bot&oacute;n, <code>false</code> en caso contrario. */
    public VisorPanel(final File signFile, final byte[] sign, final VisorFirma vf, final boolean allowReload) {
        super(true);
        this.visorFirma = vf;
        this.signatureFile = signFile;
        createUI(signFile, sign, allowReload);
    }

    private void createUI(final File signFile, final byte[] sign, final boolean addReloadButton) {
        if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST && Platform.getOS() == Platform.OS.WINDOWS) {
            setBackground(LookAndFeelManager.DEFAULT_COLOR);
        }
        setLayout(new GridBagLayout());
        setBorder(BorderFactory.createEmptyBorder(15, 15, 15, 15));

        setPreferredSize(new Dimension(DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT));

        openSign(signFile, sign, addReloadButton);
    }

    private void openSign(final File signFile, final byte[] signature, final boolean addReloadButton) {

        if (signFile == null && signature == null) {
        	LOGGER.warning("Se ha intentado abrir una firma nula");  //$NON-NLS-1$
            return;
        }

        byte[] sign = signature != null ?  signature.clone() : null;

        if (sign == null && signFile != null) {
            try ( final FileInputStream fis = new FileInputStream(signFile); ) {
                sign = AOUtil.getDataFromInputStream(fis);
            }
            catch (final Exception e) {
            	LOGGER.warning(
            		"No se ha podido cargar el fichero de firma: " + e //$NON-NLS-1$
        		);
            }
        }

        List<SignValidity> validityList = new ArrayList<>();
        if (sign != null) {
            try {
            	validityList = validateSign(sign);
            }
            catch (final Exception e) {
            	LOGGER.log(
            			Level.WARNING,
            			"No se ha podido comprobar la validez de la firma: " + e, e //$NON-NLS-1$
            			);
            	validityList.add(new SignValidity(SIGN_DETAIL_TYPE.KO, null));
            }
        } else {
        	validityList.add(new SignValidity(SIGN_DETAIL_TYPE.UNKNOWN, null));
        }

        final X509Certificate cert = getCertificate(sign);

        final JPanel resultPanel = new SignResultPanel(validityList, true, this, sign);
        this.signDataPanel = new SignDataPanel(
    		signFile,
    		sign,
    		null,
    		cert, // Certificado
    		this, 
    		null
		);

        final JPanel bottonPanel = new JPanel(true);
        bottonPanel.setLayout(new FlowLayout(FlowLayout.TRAILING));

        if (addReloadButton) {
            final JButton openSign = new JButton(SimpleAfirmaMessages.getString("VisorPanel.1")); //$NON-NLS-1$
            openSign.setMnemonic('V');
            bottonPanel.add(openSign);
            openSign.addKeyListener(this);
            openSign.addActionListener(
        		e -> {
				    if (VisorPanel.this.getVisorFirma() != null) {
				        VisorPanel.this.getVisorFirma().loadNewSign();
				    }
				}
    		);
        }

        final JButton closeVisor = new JButton(SimpleAfirmaMessages.getString("VisorPanel.0")); //$NON-NLS-1$
        closeVisor.setMnemonic('C');
        closeVisor.addKeyListener(this);
        closeVisor.addActionListener(
    		e -> {
			    if (VisorPanel.this.getVisorFirma() != null) {
			        VisorPanel.this.getVisorFirma().closeApplication(0);
			    }
			}
		);
        bottonPanel.add(closeVisor);

        // Establecemos la configuracion de color
        if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST && Platform.getOS() == Platform.OS.WINDOWS) {
            bottonPanel.setBackground(LookAndFeelManager.DEFAULT_COLOR);
        }

        // Agregamos un panel adicional en el que se mostraran los botones de los plugins
        this.mainPluginsButtonsPanel = buildMainPluginsButtonsPanel();

        setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;
        c.insets = new Insets(11, 11, 11, 11);
        add(resultPanel, c);
        c.weighty = 1.0;
        c.gridy++;
        c.insets = new Insets(0, 11, 11, 11);
        add(this.signDataPanel, c);
        c.weighty = 0.0;
        c.gridy++;
        add(bottonPanel, c);
        c.gridy++;
        add(this.mainPluginsButtonsPanel, c);

        refreshPluginButtonsContainer();

        repaint();

    }

    VisorFirma getVisorFirma() {
        return this.visorFirma;
    }

    private static X509Certificate getCertificate(final byte[] sign) {
		if (sign == null) {
			return null;
		}
		final AOTreeModel tree;
		try {
			if (new AOFacturaESigner().isSign(sign)) {
				tree = new AOFacturaESigner().getSignersStructure(sign, true);
			}
			else if (new AOXAdESSigner().isSign(sign)) {
				tree = new AOXAdESSigner().getSignersStructure(sign, true);
			}
			else if (new AOXMLDSigSigner().isSign(sign)) {
				tree = new AOXMLDSigSigner().getSignersStructure(sign, true);
			}
			else if (new AOPDFSigner().isSign(sign)) {
				tree = new AOPDFSigner().getSignersStructure(sign, true);
			}
			else if (new AOCAdESSigner().isSign(sign)) {
				tree = new AOCAdESSigner().getSignersStructure(sign, true);
			}
			else if (new AOCMSSigner().isSign(sign)) {
				tree = new AOCMSSigner().getSignersStructure(sign, true);
			}
			else if (new AOOOXMLSigner().isSign(sign)) {
				tree = new AOOOXMLSigner().getSignersStructure(sign, true);
			}
			else if (new AOODFSigner().isSign(sign)) {
				tree = new AOODFSigner().getSignersStructure(sign, true);
			}
			else {
				return null;
			}
		}
		catch(final Exception e) {
			LOGGER.warning(
				"No se ha podido obtener el certificado de la firma: " + e //$NON-NLS-1$
			);
			return null;
		}
		// Solo se muestra la informacion del certificado si hay una unica firma
		if (tree == null || AOTreeModel.getChildCount(tree.getRoot()) != 1) {
			return null;
		}
		final AOTreeNode node = (AOTreeNode) AOTreeModel.getChild(tree.getRoot(),0);
		final AOSimpleSignInfo ssi = (AOSimpleSignInfo) node.getUserObject();
		if (ssi == null) {
			return null;
		}
		final X509Certificate[] certs = ssi.getCerts();
		if (certs == null || certs.length < 1) {
			return null;
		}
		return certs[0];
	}

	/** Comprueba la validez de la firma.
     * @param sign Firma que se desea comprobar.
     * @return {@code true} si la firma es v&acute;lida, {@code false} en caso contrario.
     * @throws IOException Si ocurren problemas relacionados con la lectura de la firma. */
    public static List<SignValidity> validateSign(final byte[] sign) throws IOException {
    	List<SignValidity> validityList = new ArrayList<>();
    	final SignValider sv = SignValiderFactory.getSignValider(sign);
        if (sv != null) {
        	try {
        		validityList = sv.validate(sign);
        	}
        	catch (final RuntimeConfigNeededException e) {
        		// No ocurrira nunca por no estar configurada la validacion laxa
				throw new IOException("Error en la validacion de la firma", e); //$NON-NLS-1$
			}
        }
        else if(DataAnalizerUtil.isSignedODF(sign)) {
        	validityList.add(new SignValidity(SIGN_DETAIL_TYPE.UNKNOWN, VALIDITY_ERROR.ODF_UNKOWN_VALIDITY));
		}
		else if(DataAnalizerUtil.isSignedOOXML(sign)) {
			validityList.add(new SignValidity(SIGN_DETAIL_TYPE.UNKNOWN, VALIDITY_ERROR.OOXML_UNKOWN_VALIDITY));
		}

        if (validityList.size() == 0) {
        	validityList.add(new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.UNKOWN_SIGNATURE_FORMAT));
        }

		return validityList;
    }

	@Override
	public void keyPressed(final KeyEvent e) {
		if (e.getKeyCode() == KeyEvent.VK_ESCAPE && VisorPanel.this.visorFirma != null) {
			VisorPanel.this.visorFirma.closeApplication(0);
		}
	}

	@Override
	public void keyReleased(final KeyEvent e) {
		// Vacio
	}

	@Override
	public void keyTyped(final KeyEvent e) {
		// Vacio
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
    			PluginIntegrationWindow.VISOR);

    	for (final PluginGraphicButton button : pluginsButtons) {
			button.getGraphicButton().addActionListener(new PluginButtonActionListener(
					this.signDataPanel,
					this.signatureFile,
					button.getButton().getAction()));
    	}

    	EventQueue.invokeLater(() -> {
		    if (pluginsButtons == null || pluginsButtons.isEmpty()) {
		    	VisorPanel.this.mainPluginsButtonsPanel.setVisible(false);
		    }
		    else {
		    	VisorPanel.this.mainPluginsButtonsPanel.setVisible(false);
		    	VisorPanel.this.pluginButtonsPanel.removeAll();
		    	for (final PluginGraphicButton button : pluginsButtons) {
		    		VisorPanel.this.pluginButtonsPanel.add(button.getGraphicButton());
		    	}
		    	VisorPanel.this.mainPluginsButtonsPanel.setVisible(true);
		    }
		});
    }

	/** Acci&oacute;n gen&eacute;rica que se asigna a cada bot&oacute;n de plugin
	 * que aparezca en la pantalla. */
	class PluginButtonActionListener implements ActionListener {

		final SignDataPanel panel;
		final File signFile;
		final PluginAction action;

		public PluginButtonActionListener(final SignDataPanel panel, final File signFile, final PluginAction action) {
			this.panel = panel;
			this.signFile = signFile;
			this.action = action;
		}

		@Override
		public void actionPerformed(final ActionEvent e) {

			final CompleteSignInfo signInfo = this.panel.getCurrentSignInfo();

			final OutputData data = new OutputData();
			data.setDataFile(this.signFile);
			data.setSignatureFormat(signInfo.getSignInfo().getFormat());

			final Map<BigInteger, X509Certificate> certs = new HashMap<>();
			final AOTreeModel tree = signInfo.getSignsTree();
			for (int i = 0; i < AOTreeModel.getChildCount(tree.getRoot()); i++) {
				readCertsFromBranch((AOTreeNode) AOTreeModel.getChild(tree.getRoot(), i), certs);
			}
			data.setCerts(certs.values().toArray(new X509Certificate[certs.size()]));

			new Thread(() -> {
				if (PluginButtonActionListener.this.action instanceof SignatureProcessAction) {
				((SignatureProcessAction) PluginButtonActionListener.this.action).processSignatures(
					new OutputData[] { data },
					null,
					SwingUtilities.getWindowAncestor(VisorPanel.this));
				}
				else {
					PluginButtonActionListener.this.action.start(
							SwingUtilities.getWindowAncestor(VisorPanel.this));
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
