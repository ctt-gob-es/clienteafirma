/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.standalone.ui;

import java.awt.Color;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.net.URI;
import java.util.logging.Logger;

import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.event.HyperlinkEvent;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.batik.swing.JSVGCanvas;

import es.gob.afirma.signature.SignValidity;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.Messages;

final class SignResultPanel extends JPanel {

    private static final long serialVersionUID = -7982793036430571363L;

    private final JEditorPane descTextLabel = new JEditorPane();
    private final JLabel resultTextLabel = new JLabel();

    SignResultPanel(final SignValidity validity) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                createUI(validity);
            }
        });
    }

    void createUI(final SignValidity validity) {

        // Para que se detecten apropiadamente los hipervinculos hay que establecer
        // el tipo de contenido antes que el contenido
        this.descTextLabel.setContentType("text/html"); //$NON-NLS-1$
        
        final JSVGCanvas resultOperationIcon = new JSVGCanvas();
        resultOperationIcon.setFocusable(false);
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        try {
            String iconFilename;
            switch (validity.getValidity()) {
            case KO:
                iconFilename = "ko_icon.svg"; //$NON-NLS-1$
                break;
            case OK:
                iconFilename = "ok_icon.svg"; //$NON-NLS-1$
                break;
            case GENERATED:
                iconFilename = "ok_icon.svg"; //$NON-NLS-1$
                break;
            default:
                iconFilename = "unknown_icon.svg"; //$NON-NLS-1$
            }
            resultOperationIcon.setDocument(dbf.newDocumentBuilder()
               .parse(this.getClass()
                          .getResourceAsStream("/resources/" + iconFilename))); //$NON-NLS-1$
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido cargar el icono de resultado o validez de firma, este no se mostrara: " + e); //$NON-NLS-1$ //$NON-NLS-2$
        }

        String errorMessage;
        final String resultOperationIconTooltip;
        switch (validity.getValidity()) {
            case GENERATED:
                this.resultTextLabel.setText(Messages.getString("SignResultPanel.2")); //$NON-NLS-1$
                this.descTextLabel.setText(Messages.getString("SignResultPanel.3")); //$NON-NLS-1$
                resultOperationIconTooltip = Messages.getString("SignResultPanel.4"); //$NON-NLS-1$
                break;
            case OK:
                this.resultTextLabel.setText(Messages.getString("SignResultPanel.8")); //$NON-NLS-1$
                this.descTextLabel.setText(Messages.getString("SignResultPanel.9")); //$NON-NLS-1$
                resultOperationIconTooltip = Messages.getString("SignResultPanel.10"); //$NON-NLS-1$
                break;
            case KO:
                this.resultTextLabel.setText(Messages.getString("SignResultPanel.5")); //$NON-NLS-1$
                if (validity.getError() != null) {
                    switch (validity.getError()) {
                    case CORRUPTED_SIGN: errorMessage = Messages.getString("SignResultPanel.14"); break; //$NON-NLS-1$
                    case CERTIFICATE_EXPIRED: errorMessage = Messages.getString("SignResultPanel.16"); break; //$NON-NLS-1$
                    case CERTIFICATE_NOT_VALID_YET: errorMessage = Messages.getString("SignResultPanel.17"); break; //$NON-NLS-1$
                    case CERTIFICATE_PROBLEM: errorMessage = Messages.getString("SignResultPanel.18"); break; //$NON-NLS-1$
                    case NO_MATCH_DATA: errorMessage = Messages.getString("SignResultPanel.19"); break; //$NON-NLS-1$
                    case CRL_PROBLEM: errorMessage = Messages.getString("SignResultPanel.20"); break; //$NON-NLS-1$
                    case ALGORITHM_NOT_SUPPORTED: errorMessage = Messages.getString("SignResultPanel.22"); break; //$NON-NLS-1$
                    default:
                        errorMessage = Messages.getString("SignResultPanel.6"); //$NON-NLS-1$
                    }
                } 
                else {
                    errorMessage = Messages.getString("SignResultPanel.6"); //$NON-NLS-1$
                }
                this.descTextLabel.setText("<html><p>" + errorMessage + "</p></html>"); //$NON-NLS-1$ //$NON-NLS-2$
                resultOperationIconTooltip = Messages.getString("SignResultPanel.6"); //$NON-NLS-1$
                break;
            default:
                this.resultTextLabel.setText(Messages.getString("SignResultPanel.11")); //$NON-NLS-1$
                if (validity.getError() != null) {
                    switch (validity.getError()) {
                    case NO_DATA: errorMessage = Messages.getString("SignResultPanel.15"); break; //$NON-NLS-1$
                    default:
                        errorMessage = Messages.getString("SignResultPanel.12"); //$NON-NLS-1$
                    }
                } 
                else {
                    errorMessage = Messages.getString("SignResultPanel.12"); //$NON-NLS-1$
                }
                this.descTextLabel.setText("<html><p>" + errorMessage + "</p></html>"); //$NON-NLS-1$ //$NON-NLS-2$
                resultOperationIconTooltip = Messages.getString("SignResultPanel.13"); //$NON-NLS-1$
                break;
        }
        resultOperationIcon.setPreferredSize(new Dimension(120, 120));
        resultOperationIcon.setToolTipText(resultOperationIconTooltip);

        final EditorFocusManager editorFocusManager = new EditorFocusManager (this.descTextLabel, new EditorFocusManagerAction() {  
            @Override
            public void openHyperLink(final HyperlinkEvent he, int linkIndex) {
                try {
                    if (he.getURL() != null) {
                        Desktop.getDesktop().browse(he.getURL().toURI());
                    }
                    else {
                        Desktop.getDesktop().browse(new URI(Messages.getString("SignResultPanel.23." + linkIndex))); //$NON-NLS-1$
                    }
                }
                catch (final Exception e) {
                    UIUtils.showErrorMessage(
                        SignResultPanel.this,
                        Messages.getString("SignResultPanel.0") + he.getURL(), //$NON-NLS-1$
                        Messages.getString("SignResultPanel.1"), //$NON-NLS-1$
                        JOptionPane.ERROR_MESSAGE
                    );
                }
            }
        });
        
        this.descTextLabel.addFocusListener(editorFocusManager);
        this.descTextLabel.addHyperlinkListener(editorFocusManager);
        this.descTextLabel.addKeyListener(editorFocusManager);
        
        this.descTextLabel.setEditable(false);
        this.descTextLabel.setOpaque(false);
        
        this.resultTextLabel.setFont(this.getFont().deriveFont(Font.PLAIN, 26));
        this.resultTextLabel.setLabelFor(this.descTextLabel);

        // Establecemos la configuracion de color
        if (!LookAndFeelManager.HIGH_CONTRAST) {
            setBackground(LookAndFeelManager.WINDOW_COLOR);
            this.resultTextLabel.setForeground(new Color(3399));
        }
        
        this.setLayout(new GridBagLayout());
        
        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 0.0;
        c.weighty = 1.0;
        c.gridheight = 2;
        c.insets = new Insets(11, 11, 11, 5);
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
