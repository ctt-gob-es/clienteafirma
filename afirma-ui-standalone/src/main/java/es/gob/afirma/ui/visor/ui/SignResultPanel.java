package es.gob.afirma.ui.visor.ui;

import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.net.URI;
import java.util.logging.Logger;

import javax.swing.ImageIcon;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.event.HyperlinkEvent;
import javax.xml.parsers.DocumentBuilderFactory;

import es.gob.afirma.signature.SignValidity;
import es.gob.afirma.ui.principal.Main;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;

final class SignResultPanel extends JPanel {

    private static final long serialVersionUID = -7982793036430571363L;

    private final JEditorPane descTextLabel = new JEditorPane();
    private final JLabel resultTextLabel = new JLabel();

    SignResultPanel(final SignValidity validity) {
    	createUI(validity);
    }

    void createUI(final SignValidity validity) {
    	
        // Para que se detecten apropiadamente los hipervinculos hay que establecer
        // el tipo de contenido antes que el contenido
        this.descTextLabel.setContentType("text/html"); //$NON-NLS-1$
        
        this.resultTextLabel.setFont(this.getFont().deriveFont(Font.PLAIN, 26));
        this.resultTextLabel.setLabelFor(this.descTextLabel);

        Utils.remarcar(this.resultTextLabel);
        Utils.setContrastColor(this.resultTextLabel);
        Utils.setFontBold(this.resultTextLabel);
        
        final JLabel resultOperationIcon = new JLabel();
        resultOperationIcon.setFocusable(false);
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        try {
            String iconFilename;
            switch (validity.getValidity()) {
            case KO:
                iconFilename = "icon_ko.png"; //$NON-NLS-1$
                break;
            case OK:
                iconFilename = "icon_ok.png"; //$NON-NLS-1$
                break;
            case GENERATED:
                iconFilename = "icon_ok.png"; //$NON-NLS-1$
                break;
            default:
                iconFilename = "icon_unknown.png"; //$NON-NLS-1$
            }
            resultOperationIcon.setIcon(
                    new ImageIcon(
                            getClass().getResource("/resources/images/" + iconFilename))); //$NON-NLS-1$
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido cargar el icono de resultado o validez de firma, este no se mostrara: " + e); //$NON-NLS-1$ //$NON-NLS-2$
        }

        String errorMessage;
        final String resultOperationIconTooltip;
        switch (validity.getValidity()) {
            case GENERATED:
                this.resultTextLabel.setText(Messages.getString("SignResultPanel.2")); //$NON-NLS-1$
                if (GeneralConfig.isHighContrast() || Main.isOSHighContrast){
                	this.descTextLabel.setText(Messages.getString("SignResultPanel.25")); //$NON-NLS-1$
                	this.descTextLabel.getAccessibleContext().setAccessibleName(resultTextLabel.getText()+". "+Messages.getString("SignResultPanel.3_Lector_pantalla") +". "+ Messages.getString("SignResultPanel.enter_link"));
                } else {
                	this.descTextLabel.setText(Messages.getString("SignResultPanel.3")); //$NON-NLS-1$
                	this.descTextLabel.getAccessibleContext().setAccessibleName(resultTextLabel.getText()+". "+Messages.getString("SignResultPanel.3_Lector_pantalla") +". "+ Messages.getString("SignResultPanel.enter_link"));
                }
                resultOperationIconTooltip = Messages.getString("SignResultPanel.4"); //$NON-NLS-1$
                break;
            case OK:
                this.resultTextLabel.setText(Messages.getString("SignResultPanel.8")); //$NON-NLS-1$
                if (GeneralConfig.isHighContrast() || Main.isOSHighContrast){
                	this.descTextLabel.setText(Messages.getString("SignResultPanel.24")); //$NON-NLS-1$
                	this.descTextLabel.getAccessibleContext().setAccessibleName(resultTextLabel.getText()+". "+Messages.getString("SignResultPanel.9_Lector_pantalla") +". "+ Messages.getString("SignResultPanel.enter_link"));
                } else {
                	this.descTextLabel.setText(Messages.getString("SignResultPanel.9")); //$NON-NLS-1$
                	this.descTextLabel.getAccessibleContext().setAccessibleName(resultTextLabel.getText()+". "+Messages.getString("SignResultPanel.9_Lector_pantalla") +". "+ Messages.getString("SignResultPanel.enter_link"));
                }
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
                if (GeneralConfig.isHighContrast() || Main.isOSHighContrast){
                	this.descTextLabel.setText("<html><p style=\"color:#FFFFFF\">" + errorMessage + "</p></html>"); //$NON-NLS-1$ //$NON-NLS-2$
                } else {
                	this.descTextLabel.setText("<html><p>" + errorMessage + "</p></html>"); //$NON-NLS-1$ //$NON-NLS-2$
                }
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
                if (GeneralConfig.isHighContrast() || Main.isOSHighContrast){
                	this.descTextLabel.setText("<html><p style=\"color:#FFFFFF\">" + errorMessage + "</p></html>"); //$NON-NLS-1$ //$NON-NLS-2$
                } else {
                	this.descTextLabel.setText("<html><p>" + errorMessage + "</p></html>"); //$NON-NLS-1$ //$NON-NLS-2$
                }
                resultOperationIconTooltip = Messages.getString("SignResultPanel.13"); //$NON-NLS-1$
                break;
        }
        resultOperationIcon.setPreferredSize(new Dimension(120, 120));
        resultOperationIcon.setToolTipText(resultOperationIconTooltip);

        
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
        
        final EditorFocusManager editorFocusManager = new EditorFocusManager (this.descTextLabel, new EditorFocusManagerAction() {  
            @Override
            public void openHyperLink(final HyperlinkEvent he, int linkIndex) {
                try {
                    if (he.getURL() != null) {
                        Desktop.getDesktop().browse(he.getURL().toURI());
                    }
                    else {
                        Desktop.getDesktop().browse(new URI(Messages.getString("SignResultPanel.23." + linkIndex, "SignResultPanel.23.default"))); //$NON-NLS-1$ //$NON-NLS-2$
                    }
                }
                catch (final Exception e) {
                    CustomDialog.showMessageDialog(
                        SignResultPanel.this, true,
                        Messages.getString("SignResultPanel.0") + he.getURL(), //$NON-NLS-1$
                        Messages.getString("SignResultPanel.1"), //$NON-NLS-1$
                        JOptionPane.ERROR_MESSAGE
                    );
                }
            }
        });
        
        this.descTextLabel.setEditable(false);
        this.descTextLabel.setOpaque(false);        
        
        Utils.remarcar(this.descTextLabel);
        Utils.setContrastColor(this.descTextLabel);
        Utils.setFontBold(this.descTextLabel);
        
        this.descTextLabel.addFocusListener(editorFocusManager);
        this.descTextLabel.addHyperlinkListener(editorFocusManager);
        this.descTextLabel.addKeyListener(editorFocusManager);
        
        
        
    }
}
