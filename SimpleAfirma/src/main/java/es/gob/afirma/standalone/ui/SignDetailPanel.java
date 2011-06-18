/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
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

import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.standalone.SimpleAfirma;

/** Panel con detalles de una firma electr&oacute;nica. */
public final class SignDetailPanel extends JPanel {

    /** Tipo del resultado de la firma. */
    public enum SIGN_DETAIL_TYPE {
        /** Firma v&aacute;lida. */
        OK,
        /** Firma inv&aacute;lida. */
        KO,
        /** Firma generada en la misma aplicaci&oacute;n, se considera siempre v&aacute;lida. */
        GENERATED
    }

    /** Serial ID */
    private static final long serialVersionUID = 7567869419737753210L;

    /** Referencia a la aplicaci&ocute;n de firma. */
    private final SimpleAfirma saf;
    
    /** Bot&oacute;n para volver a la pantalla anterior. */
    private JButton returnButton = new JButton();
    
    /** Construye el panel para mostrar el detalle de una firma electr&oacute;nica.
     * @param saf Referencia a la misma aplicaci&oacute;n
     * @param sig Firma electr&oacute;nica que se desea visualizar
     * @param sigPath Ruta del fichero de firma, si no se proporciona la firma en s&iacute; se
     *        usa para cargarla
     * @param signingCert Certificado usado para generar la &uacute;ltima firma
     * @param panelType Tipo de panel informativo de cabecera
     * @param fileTypeIcon Icono vectorial indicativo del tipo de contenido. Si es <code>null</code> se determina al vuelo y se usa una version
     *        <i>raster</i> */
    public SignDetailPanel(final SimpleAfirma saf,
                           final byte[] sig,
                           final String sigPath,
                           final X509Certificate signingCert,
                           final SignDetailPanel.SIGN_DETAIL_TYPE panelType,
                           final JComponent fileTypeIcon) {
        this.saf = saf;
        createUI(sig, sigPath, signingCert, panelType, fileTypeIcon);
    }

    /** Agrega el contenido gr&aacute;fico al panel. */
    private void createUI(byte[] sig,
                          final String sigPath,
                          final X509Certificate signingCert,
                          final SignDetailPanel.SIGN_DETAIL_TYPE panelType,
                          final JComponent fileTypeIcon) {

        this.setBackground(SimpleAfirma.WINDOW_COLOR);

        // Cargamos los datos de firma si no nos los proporcionaron en el constructor
        if (sig == null && sigPath != null) {
            final File signFile = new File(sigPath);
            if (!signFile.exists()) {
                Logger.getLogger("es.gob.afirma").severe("La ruta de firma proporcionada no corresponde a ningun fichero");
            }
            else if (signFile.canRead()) {
                Logger.getLogger("es.gob.afirma").severe("No se tienen permisos de lectura del fichero indicado");
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
                    Logger.getLogger("es.gob.afirma").severe("No se ha podido leer el fichero de firma: " + e);
                }
                finally {
                    try {
                        if (fis != null) fis.close();
                    }
                    catch (final Exception e) {}
                    try {
                        if (bis != null) bis.close();
                    }
                    catch (final Exception e) {}
                }
            }
        }

        final JPanel infoPanel = new SignResultPanel(panelType);
        final JPanel componentPanel = new SignDataPanel(sigPath, sig, fileTypeIcon, signingCert);

        final JPanel returnPanel = new JPanel(true);
        returnPanel.setLayout(new BoxLayout(returnPanel, BoxLayout.Y_AXIS)); 
        returnPanel.setBackground(SimpleAfirma.WINDOW_COLOR);
        this.returnButton.setText("Firmar otro fichero");
        this.returnButton.setMnemonic('m');
        this.returnButton.setAlignmentX(Component.CENTER_ALIGNMENT);
        returnPanel.add(this.returnButton);
        this.returnButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent ae) {
                goToBack();
            }
        });

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
    }
    
    /** Vuelve a la pantalla de selecci&oacute;n de fichero para la firma. */
    private void goToBack() {
        this.saf.loadMainApp(false);
    }
}
