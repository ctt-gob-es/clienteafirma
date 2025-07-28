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
import java.awt.Rectangle;
import java.io.File;
import java.io.IOException;
import java.util.logging.Logger;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.Scrollable;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.VisorFirma;

final class SignPanelFilePanel extends JPanel implements Scrollable {

    private static final long serialVersionUID = -8648491975442788750L;

    private SignatureConfigInfoPanel configInfoPanel;

    private String accesibleDescription;

    SignPanelFilePanel(final SignOperationConfig signConfig) {
    	super(true);

    	SwingUtilities.invokeLater(() -> createUI(signConfig));
    }

    void createUI(final SignOperationConfig signConfig) {

    	final FileType fileType = signConfig.getFileType();
    	final File file = signConfig.getDataFile();

        setLayout(new GridBagLayout());
        setAlignmentY(Component.TOP_ALIGNMENT);

        // Configuramos los colores
        Color bgColor = null;
        if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
        	bgColor = Color.WHITE;
        	setBackground(bgColor);
        }
        setFocusable(true);

        // Panel con el detalle del documento
        final JPanel detailPanel = createDetailsPanel(signConfig, bgColor);

        // Boton para la apertura del fichero
        JButton openFileButton = null;

        final String ext = getExtension(file);
        if (ext == null || !isExecutable(ext) && !isLink(ext)) {

        	openFileButton = new JButton(SimpleAfirmaMessages.getString("SignPanel.51")); //$NON-NLS-1$
        	openFileButton.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("SignPanel.52")); //$NON-NLS-1$
        	this.accesibleDescription += SimpleAfirmaMessages.getString("SignPanel.52"); 	//$NON-NLS-1$
        	openFileButton.setMnemonic('c');
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
        								SimpleAfirmaMessages.getString("SignPanel.53"), //$NON-NLS-1$
        								SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
        								JOptionPane.ERROR_MESSAGE,
        								null
        								);
        						return;
        					}
        				}
        			}
        			);
        }

        getAccessibleContext().setAccessibleDescription(this.accesibleDescription);

        final JPanel buttonPanel = new JPanel();
        if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
        	buttonPanel.setBackground(bgColor);
        }
        buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.Y_AXIS));
        buttonPanel.setAlignmentX(Component.RIGHT_ALIGNMENT);
        if (openFileButton != null) {
        	buttonPanel.add(openFileButton);
        }

    	final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.weightx = 0.0;
        c.insets = new Insets(11, 0, 11, 11);
        c.anchor = GridBagConstraints.NORTHWEST;
        final Component icon = fileType.getIcon();
        if (icon != null) {
        	icon.setMinimumSize(new Dimension(128, 128));
        	add(icon, c);
        }

        c.weightx = 1.0;
        c.gridx = 1;
        c.ipadx = 0;
        c.ipady = 0;
        c.insets = new Insets(11, 0, 11, 5);
        c.anchor = GridBagConstraints.NORTH;
        add(detailPanel, c);

    	c.weightx = 0.0;
    	c.gridx = 2;
    	c.ipadx = 0;
    	c.ipady = 0;
    	c.insets = new Insets(11, 6, 11, 11);
    	c.anchor = GridBagConstraints.NORTHEAST;
    	add(buttonPanel, c);

    	c.fill = GridBagConstraints.VERTICAL;
    	c.anchor = GridBagConstraints.SOUTH;
    	c.weighty = 1.0;
    	final JPanel emptyPanel = new JPanel();
    	if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
    		emptyPanel.setBackground(bgColor);
    	}
    	add(emptyPanel, c);
    }

    /**
     * Indica si el fichero es un ejecutable en base a la extensi&oacute;n de su nombre.
     * @param ext Extensi&oacute;n de fichero.
     * @return {@code true} si el nombre del fichero tiene extensi&oacute;nde ejecutable,
     * {@code false} en caso contrario.
     */
	private static boolean isExecutable(final String ext) {
		return DataFileAnalizer.isExecutableExtension(ext);
	}

	private static boolean isLink(final String ext) {
		return "LNK".equals(ext.toUpperCase()); //$NON-NLS-1$
	}

	private JPanel createDetailsPanel(final SignOperationConfig signConfig, final Color bgColor) {

    	final File file = signConfig.getDataFile();

		final JLabel pathLabel = new JLabel(file.getAbsolutePath());
		this.accesibleDescription += file.getAbsolutePath();
        pathLabel.setFont(pathLabel.getFont().deriveFont(Font.BOLD, pathLabel.getFont().getSize() + 3f));

        // Panel de informacion del documento
        final JLabel documentInfoLabel = new JLabel(SimpleAfirmaMessages.getString("SignPanel.145")); //$NON-NLS-1$
        this.accesibleDescription += SimpleAfirmaMessages.getString("SignPanel.145"); //$NON-NLS-1$
        documentInfoLabel.setFont(documentInfoLabel.getFont().deriveFont(Font.BOLD));

        final JPanel documentInfoPanel = new DocumentInfoPanel(signConfig, bgColor);
        this.accesibleDescription += ((DocumentInfoPanel) documentInfoPanel).getAccesibleDescription();

        JLabel validationInfoLabel = null;
        JPanel validationInfoPanel = null;
        if (signConfig.getSignValidity() != null) {
	        // Panel de validacion del documento
	        validationInfoLabel = new JLabel(SimpleAfirmaMessages.getString("SignPanel.155")); //$NON-NLS-1$
	        this.accesibleDescription += SimpleAfirmaMessages.getString("SignPanel.155"); //$NON-NLS-1$
	        validationInfoLabel.setFont(validationInfoLabel.getFont().deriveFont(Font.BOLD));

	        validationInfoPanel = new ValidationInfoPanel(signConfig, bgColor);
	        this.accesibleDescription += ((ValidationInfoPanel) validationInfoPanel).getAccesibleDescription();
        }

        // Panel de configuracion de firma
		final JLabel signConfigLabel = new JLabel(SimpleAfirmaMessages.getString("SignPanel.142")); //$NON-NLS-1$
		this.accesibleDescription += SimpleAfirmaMessages.getString("SignPanel.142"); //$NON-NLS-1$
		signConfigLabel.setFont(signConfigLabel.getFont().deriveFont(Font.BOLD));
		this.configInfoPanel = new SignatureConfigInfoPanel(signConfig, bgColor, this);
		this.accesibleDescription += this.configInfoPanel.getAccesibleDescription();

        // Componemos el panel
		final JPanel detailPanel = new JPanel(new GridBagLayout());
		if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
			detailPanel.setBackground(bgColor);
		}

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.weightx = 1.0;
        c.anchor = GridBagConstraints.NORTHWEST;
        c.gridy = 0;
        detailPanel.add(pathLabel, c);
        c.gridy++;
        c.insets = new Insets(11, 0, 0, 0);
        detailPanel.add(documentInfoLabel, c);
        c.gridy++;
        c.insets = new Insets(4, 11, 0, 0);

        c.fill = GridBagConstraints.NONE;


        detailPanel.add(documentInfoPanel, c);
    	c.gridy++;
    	if (signConfig.getSignValidity() != null) {
	    	c.insets = new Insets(11, 0, 0, 0);
	        detailPanel.add(validationInfoLabel, c);
	        c.gridy++;
	        c.insets = new Insets(4, 11, 0, 0);
	        detailPanel.add(validationInfoPanel, c);
	        c.gridy++;
    	}
        c.insets = new Insets(11, 0, 0, 0);
    	detailPanel.add(signConfigLabel, c);
        c.gridy++;
        c.insets = new Insets(4, 11, 0, 0);
        detailPanel.add(this.configInfoPanel, c);

        return detailPanel;
	}

    /**
     * Indica si el fichero es un ejecutable en base a la extensi&oacute;n de su nombre.
     * @param file Fichero.
     * @return {@code true} si el nombre del fichero tiene extensi&oacute;nde ejecutable,
     * {@code false} en caso contrario.
     */
	private static String getExtension(final File file) {

		String ext = null;
		final String filename = file.getName();
		final int dotPos = filename.lastIndexOf('.');
		if (dotPos > 0 && dotPos < filename.length() - 1) {
			ext = filename.substring(dotPos + 1);
		}
		return ext;
	}

    boolean isVisibleSignature() {
    	return this.configInfoPanel.isPdfVisibleSignatureSelected();
    }

    boolean isVisibleStamp() {
    	return this.configInfoPanel.isPdfStampSignatureSelected();
    }

    String getCertificationLevel() {
    	return this.configInfoPanel.getPdfSignatureCertificationLevel();
    }

	@Override
	public Dimension getPreferredScrollableViewportSize() {
		return getPreferredSize();
	}

	@Override
	public int getScrollableUnitIncrement(final Rectangle visibleRect, final int orientation, final int direction) {
		return 30;
	}

	@Override
	public int getScrollableBlockIncrement(final Rectangle visibleRect, final int orientation, final int direction) {
		return 30;
	}

	@Override
	public boolean getScrollableTracksViewportWidth() {
		return true;
	}

	@Override
	public boolean getScrollableTracksViewportHeight() {
		return false;
	}

	public SignatureConfigInfoPanel getConfigInfoPanel() {
		return this.configInfoPanel;
	}

	public void setConfigInfoPanel(final SignatureConfigInfoPanel configInfoPanel) {
		this.configInfoPanel = configInfoPanel;
	}

}

