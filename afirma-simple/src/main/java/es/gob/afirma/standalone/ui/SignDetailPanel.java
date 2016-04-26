/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;

import es.gob.afirma.cert.signvalidation.SignValidity;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/** Panel con detalles de una firma electr&oacute;nica. */
public final class SignDetailPanel extends JPanel {

    /** Serial ID. */
    private static final long serialVersionUID = 7567869419737753210L;

    /** Referencia a la aplicaci&oacute;n de firma. */
    private final SimpleAfirma saf;

    /** Bot&oacute;n para volver a la pantalla anterior. */
    private final JButton returnButton = new JButton();

    /** Construye el panel para mostrar el detalle de una firma electr&oacute;nica.
     * @param saf Referencia a la misma aplicaci&oacute;n
     * @param sig Firma electr&oacute;nica que se desea visualizar
     * @param sigPath Ruta del fichero de firma, si no se proporciona la firma en s&iacute; se
     *        usa para cargarla
     * @param signingCert Certificado usado para generar la &uacute;ltima firma
     * @param signValidity Tipo de panel informativo de cabecera
     * @param fileTypeIcon Icono vectorial indicativo del tipo de contenido. Si es <code>null</code> se determina al vuelo y se usa una version
     *        <i>raster</i> */
    public SignDetailPanel(final SimpleAfirma saf,
                           final byte[] sig,
                           final String sigPath,
                           final X509Certificate signingCert,
                           final SignValidity signValidity,
                           final JComponent fileTypeIcon) {
        this.saf = saf;
        createUI(sig, sigPath, signingCert, signValidity, fileTypeIcon);
    }

    /** Agrega el contenido gr&aacute;fico al panel.
     * @param signature Firma creada.
     * @param sigPath Ruta del fichero firmado.
     * @param signingCert Certificado usado para firmar.
     * @param signValidity Validez de la firma.
     * @param fileTypeIcon Icono del fichero firmado (dependiente de su tipo). */
    private void createUI(final byte[] signature,
                          final String sigPath,
                          final X509Certificate signingCert,
                          final SignValidity signValidity,
                          final JComponent fileTypeIcon) {

        byte[] sig = signature != null ? signature.clone() : null;

        // Cargamos los datos de firma si no nos los proporcionaron en el constructor
        if (sig == null && sigPath != null) {
            final File signFile = new File(sigPath);
            if (!signFile.exists()) {
                Logger.getLogger("es.gob.afirma").severe("La ruta de firma proporcionada no corresponde a ningun fichero");  //$NON-NLS-1$ //$NON-NLS-2$
            }
            else if (!signFile.canRead()) {
                Logger.getLogger("es.gob.afirma").severe("No se tienen permisos de lectura del fichero indicado");  //$NON-NLS-1$//$NON-NLS-2$
            }
            else {
                InputStream fis = null;
                InputStream bis = null;
                try {
                    fis = new FileInputStream(signFile);
                    bis = new BufferedInputStream(fis);
                    sig = AOUtil.getDataFromInputStream(bis);
                }
                catch (final IOException e) {
                    Logger.getLogger("es.gob.afirma").severe("No se ha podido leer el fichero de firma: " + e); //$NON-NLS-1$ //$NON-NLS-2$
                }
                finally {
                    try {
                        if (fis != null) {
                            fis.close();
                        }
                    }
                    catch (final Exception e) { /* Ignoramos los errores */ }
                    try {
                        if (bis != null) {
                            bis.close();
                        }
                    }
                    catch (final Exception e) { /* Ignoramos los errores */ }
                }
            }
        }

        final JPanel infoPanel = new SignResultPanel(signValidity, null);
        final JPanel componentPanel = new SignDataPanel(new File(sigPath), sig, fileTypeIcon, signingCert, null);

        final JPanel returnPanel = new JPanel(true);
        returnPanel.setLayout(new BoxLayout(returnPanel, BoxLayout.Y_AXIS));

        this.returnButton.setText(SimpleAfirmaMessages.getString("SignDetailPanel.0")); //$NON-NLS-1$
        this.returnButton.setMnemonic('m');
        this.returnButton.setAlignmentX(Component.CENTER_ALIGNMENT);
        returnPanel.add(this.returnButton);
        this.returnButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent ae) {
                goToBack();
            }
        });

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
