/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.dnd.DropTarget;
import java.io.File;
import java.io.IOException;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.util.Date;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.VisorFirma;
import es.gob.afirma.standalone.ui.SignOperationConfig.CryptoOperation;
import es.gob.afirma.standalone.ui.preferences.PreferencesDialog;
import es.gob.afirma.standalone.ui.preferences.PreferencesManager;

final class SignPanelFilePanel extends JPanel {

    private static final long serialVersionUID = -8648491975442788750L;

    private final JCheckBox pdfVisible = new JCheckBox(
		SimpleAfirmaMessages.getString("SignPanel.44"), //$NON-NLS-1$
		PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_PADES_VISIBLE)
	);

    boolean isVisibleSignature() {
    	return this.pdfVisible.isSelected();
    }

    private final JCheckBox pdfStamp = new JCheckBox(
		SimpleAfirmaMessages.getString("SignPanel.120"), //$NON-NLS-1$
		PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_PADES_STAMP)
	);

    boolean isVisibleStamp() {
    	return this.pdfStamp.isSelected();
    }

    SignPanelFilePanel(final SignOperationConfig signConfig,
    		final DropTarget dropTarget) {

    	super(true);

    	// Puede arrastrarse un fichero a cualquiera de estos componentes para cargarlo
    	setDropTarget(dropTarget);

    	SwingUtilities.invokeLater(() -> createUI(
    			signConfig.getFileType(),
    			signConfig.getSigner(),
    			signConfig.getSignatureFormatName(),
    			NumberFormat.getNumberInstance().format(signConfig.getDataFile().length() / 1024),
    			signConfig.getDataFile(),
    			new Date(signConfig.getDataFile().lastModified()),
    			signConfig.getCryptoOperation()
    			));
    }

    void createUI(final FileType fileType,
    			  final AOSigner signer,
    			  final String signatureName,
    			  final String fileSize,
                  final File file,
                  final Date fileLastModified,
                  final CryptoOperation operation) {

        setBorder(BorderFactory.createLineBorder(Color.black));
        setLayout(new GridBagLayout());

        final JLabel pathLabel = new JLabel(file.getAbsolutePath());
        pathLabel.setFont(pathLabel.getFont().deriveFont(Font.BOLD, pathLabel.getFont().getSize() + 3f));


        final JLabel signLabel = new JLabel(
        		SimpleAfirmaMessages.getString("SignPanel.103", signatureName)); //$NON-NLS-1$
        final JLabel descLabel = new JLabel(
        		SimpleAfirmaMessages.getString("SignPanel.46", fileType.getFileDescription())); //$NON-NLS-1$
        final JLabel dateLabel = new JLabel(
        		SimpleAfirmaMessages.getString("SignPanel.47",  //$NON-NLS-1$
        				DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.SHORT)
        				.format(fileLastModified))
        		);

        final JLabel sizeLabel = new JLabel(
    		SimpleAfirmaMessages.getString("SignPanel.49") + (fileSize.equals("0") ? "<1" : fileSize) + " KB" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		);

        final JPanel detailPanel = new JPanel();
        detailPanel.setLayout(new BoxLayout(detailPanel, BoxLayout.Y_AXIS));
        detailPanel.add(pathLabel);
        detailPanel.add(Box.createRigidArea(new Dimension(0, 8)));
        detailPanel.add(signLabel);
        detailPanel.add(Box.createRigidArea(new Dimension(0, 8)));
        detailPanel.add(descLabel);
        detailPanel.add(dateLabel);
        detailPanel.add(Box.createRigidArea(new Dimension(0, 8)));
        detailPanel.add(sizeLabel);

        // Definimos aqui el boton
        JButton openFileButton = null;

        final String ext = getExtension(file);
        if (ext == null || !isExecutable(ext) && !isLink(ext)) {

        	openFileButton = new JButton(SimpleAfirmaMessages.getString("SignPanel.51")); //$NON-NLS-1$
        	openFileButton.setMnemonic('v');
        	openFileButton.addActionListener(
        			ae -> {
        				if (file.getName().endsWith(".csig") || file.getName().endsWith(".xsig")) { //$NON-NLS-1$ //$NON-NLS-2$
        					new VisorFirma(false, null).initialize(false, file);
        				}
        				else {
        					try {
        						Desktop.getDesktop().open(file);
        					}
        					catch (final IOException e) {
        						Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
        								"Error abriendo el fichero: " + e //$NON-NLS-1$
        								);
        						AOUIFactory.showErrorMessage(
        								SignPanelFilePanel.this,
        								SimpleAfirmaMessages.getString("SignPanel.53"), //$NON-NLS-1$
        								SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
        								JOptionPane.ERROR_MESSAGE
        								);
        						return;
        					}
        				}
        			}
        			);
        	pathLabel.setLabelFor(openFileButton);
        	descLabel.setLabelFor(openFileButton);
        	dateLabel.setLabelFor(openFileButton);
        	sizeLabel.setLabelFor(openFileButton);
        }

        // Establecemos la configuracion de color
        Color bgColor = Color.WHITE;
        // Configuramos los colores
        if (!LookAndFeelManager.HIGH_CONTRAST && !Platform.OS.MACOSX.equals(Platform.getOS())) {
        	bgColor = LookAndFeelManager.WINDOW_COLOR;
        }
        setBackground(bgColor);
        detailPanel.setBackground(bgColor);

        if (signer instanceof AOPDFSigner) {
            this.pdfVisible.setMnemonic('H');
            this.pdfVisible.setBackground(bgColor);
            detailPanel.add(Box.createRigidArea(new Dimension(0, 8)));
            detailPanel.add(this.pdfVisible);

            this.pdfStamp.setMnemonic('S');
            this.pdfStamp.setBackground(bgColor);
            detailPanel.add(Box.createRigidArea(new Dimension(0, 8)));
            detailPanel.add(this.pdfStamp);

            if(operation == CryptoOperation.COSIGN) {
            	this.pdfStamp.setSelected(false);
            	this.pdfStamp.setEnabled(false);
                detailPanel.add(Box.createRigidArea(new Dimension(0, 5)));
                detailPanel.add(new JLabel(SimpleAfirmaMessages.getString("SignPanel.121"))); //$NON-NLS-1$
            }
        }

        // Habilita boton de opciones avanzadas de multifirma
        if (fileType == FileType.SIGN_CADES || fileType == FileType.SIGN_XADES) {
        	final JButton avanzado = new JButton(SimpleAfirmaMessages.getString("SignPanel.119")); //$NON-NLS-1$

        	detailPanel.add(Box.createRigidArea(new Dimension(0, 6)));
        	avanzado.setMnemonic('a');
        	avanzado.addActionListener(
    			ae -> {
    				if(fileType == FileType.SIGN_CADES) {
    					PreferencesDialog.show(null, true, 2);
    				}
    				else {
    					PreferencesDialog.show(null, true, 3);
    				}
    			}
        	);
            detailPanel.add(avanzado);
        }

    	final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weighty = 1.0;
        c.ipadx = 60;
        c.ipady = 60;
        c.insets = new Insets(11, 0, 11, 11);
        c.anchor = GridBagConstraints.NORTHWEST;
        final Component icon = fileType.getIcon();
        if (icon != null) {
        	icon.setMinimumSize(new Dimension(110,  110));
        	add(icon, c);
        }

        if (openFileButton != null) {
        	c.fill = GridBagConstraints.HORIZONTAL;
        	c.weightx = 0.0;
        	c.weighty = 0.0;
        	c.gridx = 2;
        	c.ipadx = 0;
        	c.ipady = 0;
        	c.insets = new Insets(11, 6, 11, 11);
        	c.anchor = GridBagConstraints.NORTHEAST;
        	add(openFileButton, c);
        }

        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.weighty = 1.0;
        c.gridx = 1;
        c.ipadx = 0;
        c.ipady = 0;
        c.insets = new Insets(14, 0, 11, 5);
        c.anchor = GridBagConstraints.NORTH;
        add(detailPanel, c);
    }



    /**
     * Indica si el fichero es un ejecutable en base a la extensi&oacute;n de su nombre.
     * @param file Fichero.
     * @return {@code true} si el nombre del fichero tiene extensi&oacute;nde ejecutable,
     * {@code false} en caso contrario.
     */
	private static boolean isExecutable(String ext) {
		return DataFileAnalizer.isExecutableExtension(ext);
	}

	private static boolean isLink(String ext) {
		return "LNK".equals(ext.toUpperCase()); //$NON-NLS-1$
	}

    /**
     * Indica si el fichero es un ejecutable en base a la extensi&oacute;n de su nombre.
     * @param file Fichero.
     * @return {@code true} si el nombre del fichero tiene extensi&oacute;nde ejecutable,
     * {@code false} en caso contrario.
     */
	private static String getExtension(File file) {

		String ext = null;
		final String filename = file.getName();
		final int dotPos = filename.lastIndexOf('.');
		if (dotPos > 0 && dotPos < filename.length() - 1) {
			ext = filename.substring(dotPos + 1);
		}
		return ext;
	}
}
