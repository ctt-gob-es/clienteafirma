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
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.io.File;
import java.security.cert.X509Certificate;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;

import es.gob.afirma.cert.signvalidation.SignValidity;
import es.gob.afirma.cert.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/** Panel con el resultado de un proceso de firma masiva. */
public final class SignResultListPanel extends JPanel {

    /** Serial ID. */
	private static final long serialVersionUID = 1896328450345342947L;

	/** Referencia a la aplicaci&oacute;n de firma. */
    private final SimpleAfirma saf;

    /** Bot&oacute;n para volver a la pantalla anterior. */
    private final JButton returnButton = new JButton();

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

        setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.insets = new Insets(11, 11, 11, 11);
        add(infoPanel, c);
        c.weighty = 1.0;
        c.gridy = 1;
        c.insets = new Insets(0, 11, 11, 11);
        add(componentPanel, c);
        c.weighty = 0.0;
        c.gridy = 2;
        c.insets = new Insets(0, 11, 11, 11);
        add(returnPanel, c);
        this.returnButton.requestFocusInWindow();
    }

    /** Vuelve a la pantalla de selecci&oacute;n de fichero para la firma. */
    void goToBack() {
        this.saf.loadMainApp();
    }
}
