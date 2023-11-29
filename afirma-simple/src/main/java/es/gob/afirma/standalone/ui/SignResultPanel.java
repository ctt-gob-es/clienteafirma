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
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.KeyListener;
import java.awt.image.BufferedImage;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

final class SignResultPanel extends JPanel {

    private static final long serialVersionUID = -7982793036430571363L;

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private final JLabel descTextLabel = new JLabel();
    private final JLabel resultTextLabel = new JLabel();
    private final JLabel linkLabel = new JLabel();
    private final JLabel validationLinkLabel = new JLabel();

    private String accessibleDescription = ""; //$NON-NLS-1$

    SignResultPanel(final List<SignValidity> validity, final boolean singleSign, final KeyListener extKeyListener) {
        SwingUtilities.invokeLater(() -> createUI(validity, singleSign, extKeyListener, null));
    }

    SignResultPanel(final List<SignValidity> validity, final boolean singleSign, final KeyListener extKeyListener, final byte [] signData) {
        SwingUtilities.invokeLater(() -> createUI(validity, singleSign, extKeyListener, signData));
    }

    void createUI(final List<SignValidity> validity, final boolean singleSign, final KeyListener extKeyListener, final byte [] signData) {

        String iconFilename;
        switch (validity.get(0).getValidity()) {
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
        resultOperationIcon.setBackground(LookAndFeelManager.TRANSPARENT_COLOR);
        resultOperationIcon.setFocusable(false);
        resultOperationIcon.setMinimumSize(new Dimension(120, 120));

        String errorMessage;
        final String resultOperationIconTooltip;
        switch (validity.get(0).getValidity()) {
            case GENERATED:

                this.resultTextLabel.setText(SimpleAfirmaMessages.getString("SignResultPanel.2")); //$NON-NLS-1$
                this.descTextLabel.setText("<html><p>" + SimpleAfirmaMessages.getString("SignResultPanel.3") + "</p></html>"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                this.linkLabel.setText(SimpleAfirmaMessages.getString("SignResultPanel.33")); //$NON-NLS-1$

            	// Este gestor se encargara de controlar los eventos de foco y raton
                LabelLinkManager labelLinkManager = new LabelLinkManager(this.linkLabel);
                labelLinkManager.setLabelLinkListener(new URLLabelLinkImpl(
                		SimpleAfirmaMessages.getString("SignResultPanel.33") //$NON-NLS-1$
                ));

                if (extKeyListener != null) {
                	this.linkLabel.addKeyListener(extKeyListener);
                }

                this.linkLabel.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("SignDataPanel.46") //$NON-NLS-1$
                		+ SimpleAfirmaMessages.getString("SignResultPanel.33")); //$NON-NLS-1$

            	// Este gestor se encargara de controlar los eventos de foco y raton
                final LabelLinkManager detailsSignLinkManager = new LabelLinkManager(this.validationLinkLabel);
                detailsSignLinkManager.setLabelLinkListener(new ValidationErrorsLabelLinkImpl(
                		signData, validity
                ));

                this.validationLinkLabel.setText(SimpleAfirmaMessages.getString("SignResultPanel.38")); //$NON-NLS-1$
                this.validationLinkLabel.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("SignDataPanel.46") //$NON-NLS-1$
                		+ SimpleAfirmaMessages.getString("SignResultPanel.38")); //$NON-NLS-1$

                this.accessibleDescription += SimpleAfirmaMessages.getString("SignResultPanel.2") //$NON-NLS-1$
                						 + SimpleAfirmaMessages.getString("SignResultPanel.3") //$NON-NLS-1$
                						 + SimpleAfirmaMessages.getString("SignDataPanel.46") //$NON-NLS-1$
                						 + SimpleAfirmaMessages.getString("SignResultPanel.33"); //$NON-NLS-1$

                resultOperationIconTooltip = SimpleAfirmaMessages.getString("SignResultPanel.4"); //$NON-NLS-1$
                break;
            case OK:

                String okMessage;
            	if (singleSign) {
            		okMessage = SimpleAfirmaMessages.getString("SignResultPanel.9"); //$NON-NLS-1$
            		this.resultTextLabel.setText(SimpleAfirmaMessages.getString("SignResultPanel.8")); //$NON-NLS-1$
            		this.descTextLabel.setText("<html><p>" + okMessage + "</p></html>"); //$NON-NLS-1$ //$NON-NLS-2$
            		resultOperationIconTooltip = SimpleAfirmaMessages.getString("SignResultPanel.10"); //$NON-NLS-1$
                    this.accessibleDescription += SimpleAfirmaMessages.getString("SignResultPanel.8") //$NON-NLS-1$
   						 						+ okMessage
   						 						+ SimpleAfirmaMessages.getString("SignResultPanel.33"); //$NON-NLS-1$

                	// Este gestor se encargara de controlar los eventos de foco y raton
                    final LabelLinkManager okDetailsSignLinkManager = new LabelLinkManager(this.validationLinkLabel);
                    okDetailsSignLinkManager.setLabelLinkListener(new ValidationErrorsLabelLinkImpl(
                    		signData, validity
                    ));

                    this.validationLinkLabel.setText(SimpleAfirmaMessages.getString("SignResultPanel.38")); //$NON-NLS-1$
                    this.validationLinkLabel.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("SignDataPanel.46") //$NON-NLS-1$
                    		+ SimpleAfirmaMessages.getString("SignResultPanel.38")); //$NON-NLS-1$
            	}
            	else {
            		okMessage = SimpleAfirmaMessages.getString("SignResultPanel.28"); //$NON-NLS-1$
            		this.resultTextLabel.setText(SimpleAfirmaMessages.getString("SignResultPanel.27")); //$NON-NLS-1$
            		this.descTextLabel.setText("<html><p>" + okMessage + "</p></html>"); //$NON-NLS-1$ //$NON-NLS-2$
            		resultOperationIconTooltip = SimpleAfirmaMessages.getString("SignResultPanel.29"); //$NON-NLS-1$

                    this.accessibleDescription += SimpleAfirmaMessages.getString("SignResultPanel.27") //$NON-NLS-1$
   						 						+ SimpleAfirmaMessages.getString("SignResultPanel.28") //$NON-NLS-1$
                    								+ SimpleAfirmaMessages.getString("SignResultPanel.33"); //$NON-NLS-1$
            	}
                this.linkLabel.setText(SimpleAfirmaMessages.getString("SignResultPanel.33")); //$NON-NLS-1$

            	// Este gestor se encargara de controlar los eventos de foco y raton
                labelLinkManager = new LabelLinkManager(this.linkLabel);
                labelLinkManager.setLabelLinkListener(new URLLabelLinkImpl(
						SimpleAfirmaMessages.getString("SignResultPanel.33") //$NON-NLS-1$
				));

                if (extKeyListener != null) {
                	this.linkLabel.addKeyListener(extKeyListener);
                }

                this.linkLabel.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("SignDataPanel.46") //$NON-NLS-1$
                		+ SimpleAfirmaMessages.getString("SignResultPanel.33")); //$NON-NLS-1$
                break;
            case KO:
                if (singleSign) {
                	this.resultTextLabel.setText(SimpleAfirmaMessages.getString("SignResultPanel.5")); //$NON-NLS-1$
                    if (validity.get(0).getError() != null) {
                		switch (validity.get(0).getError()) {
                		case CORRUPTED_SIGN: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.14"); break; //$NON-NLS-1$
                		case CERTIFICATE_EXPIRED: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.16"); break; //$NON-NLS-1$
                		case CERTIFICATE_NOT_VALID_YET: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.17"); break; //$NON-NLS-1$
                		case CERTIFICATE_PROBLEM: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.18"); break; //$NON-NLS-1$
                		case NO_MATCH_DATA: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.19"); break; //$NON-NLS-1$
                		case CRL_PROBLEM: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.20"); break; //$NON-NLS-1$
                		case ALGORITHM_NOT_SUPPORTED: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.22"); break; //$NON-NLS-1$
                		case MODIFIED_FORM: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.35"); break; //$NON-NLS-1$
                		case MODIFIED_DOCUMENT: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.36"); break; //$NON-NLS-1$
                		case OVERLAPPING_SIGNATURE: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.37"); break; //$NON-NLS-1$

                		default:
                			errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.6"); //$NON-NLS-1$
                			LOGGER.warning("No se ha identificado el motivo por el que la firma no es valida: " + validity.get(0).getError()); //$NON-NLS-1$
                		}
                	}
                	else {
                		errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.6"); //$NON-NLS-1$
                	}
                	this.descTextLabel.setText("<html><p>" + errorMessage + "</p></html>"); //$NON-NLS-1$ //$NON-NLS-2$
                    resultOperationIconTooltip = SimpleAfirmaMessages.getString("SignResultPanel.6"); //$NON-NLS-1$

                    this.accessibleDescription += SimpleAfirmaMessages.getString("SignResultPanel.5") //$NON-NLS-1$
   						 						+ errorMessage;

                	// Este gestor se encargara de controlar los eventos de foco y raton
                    final LabelLinkManager koSSDetailsSignLinkManager = new LabelLinkManager(this.validationLinkLabel);
                    koSSDetailsSignLinkManager.setLabelLinkListener(new ValidationErrorsLabelLinkImpl(
                    		signData, validity
                    ));

                    this.validationLinkLabel.setText(SimpleAfirmaMessages.getString("SignResultPanel.38")); //$NON-NLS-1$
                    this.validationLinkLabel.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("SignDataPanel.46") //$NON-NLS-1$
                    		+ SimpleAfirmaMessages.getString("SignResultPanel.38")); //$NON-NLS-1$
                }
                else {
                	this.resultTextLabel.setText(SimpleAfirmaMessages.getString("SignResultPanel.30")); //$NON-NLS-1$
                    errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.31"); //$NON-NLS-1$
                	this.descTextLabel.setText("<html><p>" + errorMessage + "</p></html>"); //$NON-NLS-1$ //$NON-NLS-2$
                    this.linkLabel.setText(SimpleAfirmaMessages.getString("SignResultPanel.33")); //$NON-NLS-1$

                	// Este gestor se encargara de controlar los eventos de foco y raton
                    labelLinkManager = new LabelLinkManager(this.linkLabel);
                    labelLinkManager.setLabelLinkListener(new URLLabelLinkImpl(
							SimpleAfirmaMessages.getString("SignResultPanel.33") //$NON-NLS-1$
					));

                    if (extKeyListener != null) {
                    	this.linkLabel.addKeyListener(extKeyListener);
                    }

                    this.linkLabel.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("SignDataPanel.46") //$NON-NLS-1$
                    		+ SimpleAfirmaMessages.getString("SignResultPanel.33")); //$NON-NLS-1$
                    resultOperationIconTooltip = SimpleAfirmaMessages.getString("SignResultPanel.32"); //$NON-NLS-1$
                    this.accessibleDescription += SimpleAfirmaMessages.getString("SignResultPanel.30") //$NON-NLS-1$
   						 						+ errorMessage
   						 						+ SimpleAfirmaMessages.getString("SignResultPanel.33"); //$NON-NLS-1$
                }
                break;
            default:
                this.resultTextLabel.setText(SimpleAfirmaMessages.getString("SignResultPanel.11")); //$NON-NLS-1$
                if (validity.get(0).getError() != null) {
                    switch (validity.get(0).getError()) {
                    case NO_DATA: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.15"); break; //$NON-NLS-1$
                    case CANT_VALIDATE_EXTERNALLY_DETACHED: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.39"); break; //$NON-NLS-1$
                    case SIGN_PROFILE_NOT_CHECKED: errorMessage = SimpleAfirmaMessages.getString("SignResultPanel.40"); break; //$NON-NLS-1$
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
                this.accessibleDescription += SimpleAfirmaMessages.getString("SignResultPanel.11") //$NON-NLS-1$
						 						+ errorMessage
						 						+ SimpleAfirmaMessages.getString("SignResultPanel.13"); //$NON-NLS-1$

            	// Este gestor se encargara de controlar los eventos de foco y raton
                final LabelLinkManager defDetailsSignLinkManager = new LabelLinkManager(this.validationLinkLabel);
                defDetailsSignLinkManager.setLabelLinkListener(new ValidationErrorsLabelLinkImpl(
                		signData, validity
                ));

                this.validationLinkLabel.setText(SimpleAfirmaMessages.getString("SignResultPanel.38")); //$NON-NLS-1$
                this.validationLinkLabel.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("SignDataPanel.46") //$NON-NLS-1$
                		+ SimpleAfirmaMessages.getString("SignResultPanel.38")); //$NON-NLS-1$
                break;
        }

        resultOperationIcon.setToolTipText(resultOperationIconTooltip);

        this.resultTextLabel.setFont(getFont().deriveFont(Font.PLAIN, 26));
        this.resultTextLabel.setLabelFor(this.descTextLabel);

        // Establecemos la configuracion de color cuando no se encuentra
        // activado el alto contraste y estamos en Windows (en donde se
        // utiliza un Look&Feel determinado)
        if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST && Platform.getOS() == Platform.OS.WINDOWS) {
            setBackground(LookAndFeelManager.SECUNDARY_COLOR);
        }

        setLayout(new GridBagLayout());

        setFocusable(true);
        getAccessibleContext().setAccessibleDescription(this.accessibleDescription);

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 0.0;
        c.weighty = 1.0;
        c.gridheight = 3;
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
        c.insets = new Insets(0, 6, 0, 11);
        this.add(this.descTextLabel, c);
        c.weighty = 0.0;
        c.gridy = 2;
        c.insets = new Insets(0, 6, 0, 11);
        this.add(this.linkLabel, c);
        c.gridy = 3;
        c.insets = new Insets(0, 6, 0, 11);
        this.add(this.validationLinkLabel, c);

    }

}
