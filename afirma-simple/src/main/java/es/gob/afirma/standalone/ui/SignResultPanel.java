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
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.KeyListener;
import java.awt.image.BufferedImage;
import java.util.logging.Logger;

import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

final class SignResultPanel extends JPanel {

    private static final long serialVersionUID = -7982793036430571363L;

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private final JEditorPane descTextLabel = new JEditorPane();
    private final JLabel resultTextLabel = new JLabel();

    SignResultPanel(final SignValidity validity, final boolean singleSign, final KeyListener extKeyListener) {
        SwingUtilities.invokeLater(() -> createUI(validity, singleSign, extKeyListener));
    }

    void createUI(final SignValidity validity, final boolean singleSign, final KeyListener extKeyListener) {

        // Para que se detecten apropiadamente los hipervinculos hay que establecer
        // el tipo de contenido antes que el contenido
        this.descTextLabel.setContentType("text/html"); //$NON-NLS-1$

        String iconFilename;
        switch (validity.getValidity()) {
        case KO:
            iconFilename = "ko_icon_large.png"; //$NON-NLS-1$
            break;
        case OK:
        case GENERATED:
            iconFilename = "ok_icon_large.png"; //$NON-NLS-1$
            break;
        default:
            iconFilename = "unknown_icon_large.png"; //$NON-NLS-1$
        }

        final BufferedImage image = ImageLoader.loadImage(iconFilename);
        final ScalablePane resultOperationIcon = new ScalablePane(image, true);
        resultOperationIcon.setBackground(new Color(255, 255, 255, 0));
        resultOperationIcon.setFocusable(false);
        resultOperationIcon.setMinimumSize(new Dimension(120, 120));

        String errorMessage;
        final String resultOperationIconTooltip;
        switch (validity.getValidity()) {
            case GENERATED:
                this.resultTextLabel.setText(SimpleAfirmaMessages.getString("SignResultPanel.2")); //$NON-NLS-1$
                this.descTextLabel.setText(SimpleAfirmaMessages.getString("SignResultPanel.3")); //$NON-NLS-1$
                resultOperationIconTooltip = SimpleAfirmaMessages.getString("SignResultPanel.4"); //$NON-NLS-1$
                break;
            case OK:
            	if (singleSign) {
            		this.resultTextLabel.setText(SimpleAfirmaMessages.getString("SignResultPanel.8")); //$NON-NLS-1$
            		this.descTextLabel.setText(SimpleAfirmaMessages.getString("SignResultPanel.9")); //$NON-NLS-1$
            		resultOperationIconTooltip = SimpleAfirmaMessages.getString("SignResultPanel.10"); //$NON-NLS-1$
            	}
            	else {
            		this.resultTextLabel.setText(SimpleAfirmaMessages.getString("SignResultPanel.27")); //$NON-NLS-1$
            		this.descTextLabel.setText(SimpleAfirmaMessages.getString("SignResultPanel.28")); //$NON-NLS-1$
            		resultOperationIconTooltip = SimpleAfirmaMessages.getString("SignResultPanel.29"); //$NON-NLS-1$
            	}
                break;
            case KO:
                if (singleSign) {
                	this.resultTextLabel.setText(SimpleAfirmaMessages.getString("SignResultPanel.5")); //$NON-NLS-1$
                    if (validity.getError() != null) {
                		switch (validity.getError()) {
                		case CORRUPTED_SIGN: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.14"); break; //$NON-NLS-1$
                		case CERTIFICATE_EXPIRED: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.16"); break; //$NON-NLS-1$
                		case CERTIFICATE_NOT_VALID_YET: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.17"); break; //$NON-NLS-1$
                		case CERTIFICATE_PROBLEM: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.18"); break; //$NON-NLS-1$
                		case NO_MATCH_DATA: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.19"); break; //$NON-NLS-1$
                		case CRL_PROBLEM: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.20"); break; //$NON-NLS-1$
                		case ALGORITHM_NOT_SUPPORTED: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.22"); break; //$NON-NLS-1$

                		default:
                			errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.6"); //$NON-NLS-1$
                			LOGGER.warning("No se ha identificado el motivo por el que la firma no es valida: " + validity.getError()); //$NON-NLS-1$
                		}
                	}
                	else {
                		errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.6"); //$NON-NLS-1$
                	}
                	this.descTextLabel.setText("<html><p>" + errorMessage + "</p></html>"); //$NON-NLS-1$ //$NON-NLS-2$
                    resultOperationIconTooltip = SimpleAfirmaMessages.getString("SignResultPanel.6"); //$NON-NLS-1$
                }
                else {
                	this.resultTextLabel.setText(SimpleAfirmaMessages.getString("SignResultPanel.30")); //$NON-NLS-1$
                    errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.31"); //$NON-NLS-1$
                	this.descTextLabel.setText("<html><p>" + errorMessage + "</p></html>"); //$NON-NLS-1$ //$NON-NLS-2$
                    resultOperationIconTooltip = SimpleAfirmaMessages.getString("SignResultPanel.32"); //$NON-NLS-1$
                }
                break;
            default:
                this.resultTextLabel.setText(SimpleAfirmaMessages.getString("SignResultPanel.11")); //$NON-NLS-1$
                if (validity.getError() != null) {
                    switch (validity.getError()) {
                    case NO_DATA: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.15"); break; //$NON-NLS-1$
                    case PDF_UNKOWN_VALIDITY: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.24"); break; //$NON-NLS-1$
                    case OOXML_UNKOWN_VALIDITY: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.25"); break; //$NON-NLS-1$
                    case ODF_UNKOWN_VALIDITY: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.26"); break; //$NON-NLS-1$
                    default:
                        errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.12"); //$NON-NLS-1$
                    }
                }
                else {
                    errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.12"); //$NON-NLS-1$
                }
                this.descTextLabel.setText("<html><p>" + errorMessage + "</p></html>"); //$NON-NLS-1$ //$NON-NLS-2$
                resultOperationIconTooltip = SimpleAfirmaMessages.getString("SignResultPanel.13"); //$NON-NLS-1$
                break;
        }

        resultOperationIcon.setToolTipText(resultOperationIconTooltip);

        final EditorFocusManager editorFocusManager = new EditorFocusManager(
    		this.descTextLabel,
    		(he, linkIndex) -> {
			    try {
			        if (he.getURL() != null) {
			            Desktop.getDesktop().browse(he.getURL().toURI());
			        }
			    }
			    catch (final Exception e) {
			    	LOGGER.warning(
						"Error abriendo el fichero: " + e //$NON-NLS-1$
					);
			    	AOUIFactory.showErrorMessage(
			            SignResultPanel.this,
			            SimpleAfirmaMessages.getString("SignResultPanel.0") + he.getURL(), //$NON-NLS-1$
			            SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
			            JOptionPane.ERROR_MESSAGE
			        );
			    }
			}
		);

        this.descTextLabel.addFocusListener(editorFocusManager);
        this.descTextLabel.addHyperlinkListener(editorFocusManager);
        this.descTextLabel.addKeyListener(editorFocusManager);
        if (extKeyListener != null) {
        	this.descTextLabel.addKeyListener(extKeyListener);
        }

        this.descTextLabel.setEditable(false);
        this.descTextLabel.setOpaque(false);

        this.resultTextLabel.setFont(getFont().deriveFont(Font.PLAIN, 26));
        this.resultTextLabel.setLabelFor(this.descTextLabel);

        // Establecemos la configuracion de color
        if (!LookAndFeelManager.HIGH_CONTRAST) {
            setBackground(LookAndFeelManager.WINDOW_COLOR);
            this.resultTextLabel.setForeground(new Color(3399));
        }

        setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 0.0;
        c.weighty = 1.0;
        c.gridheight = 2;
        c.insets = new Insets(11, 11, 0, 5);
       	this.add(resultOperationIcon, c);
        c.weightx = 1.0;
        c.weighty = 0.0;
        c.gridx = 1;
        c.gridheight = 1;
        c.insets = new Insets(11, 6, 0, 11);
        this.add(this.resultTextLabel, c);
        c.weighty = 1.0;
        c.gridy = 1;
        c.insets = new Insets(0, 6, 5, 11);
        this.add(this.descTextLabel, c);

    }
}
