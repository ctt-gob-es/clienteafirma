package es.gob.afirma.standalone.ui;

import java.awt.Desktop;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.logging.Logger;

import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.batik.swing.JSVGCanvas;

import es.gob.afirma.standalone.Messages;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.ui.SignDetailPanel.SIGN_DETAIL_TYPE;

final class SignResultPanel extends JPanel {

    private static final long serialVersionUID = -7982793036430571363L;
    
    private final JEditorPane descTextLabel = new JEditorPane();
    private final JLabel resultTextLabel = new JLabel();
    
    SignResultPanel(final SIGN_DETAIL_TYPE type) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                createUI(type);
            }
        });
    }
    
    private void createUI(final SIGN_DETAIL_TYPE type) {

        final JSVGCanvas resultOperationIcon = new JSVGCanvas();
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        try {
            resultOperationIcon.setDocument(dbf.newDocumentBuilder()
               .parse(this.getClass()
                          .getResourceAsStream("/resources/" + (type.equals(SIGN_DETAIL_TYPE.KO)  //$NON-NLS-1$
                                ? "ko_icon.svg" //$NON-NLS-1$
                                : "ok_icon.svg")))); //$NON-NLS-1$
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido cargar el icono de resultado o validez de firma, este no se mostrara: " + e); //$NON-NLS-1$ //$NON-NLS-2$
        }

        this.descTextLabel.setContentType("text/html"); //$NON-NLS-1$
        this.descTextLabel.addHyperlinkListener(new HyperlinkListener() {
            @Override
            public void hyperlinkUpdate(final HyperlinkEvent he) {
                if (HyperlinkEvent.EventType.ACTIVATED.equals(he.getEventType())) {
                    try {
                        Desktop.getDesktop().browse(he.getURL().toURI());
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
            }
        });
        this.descTextLabel.setEditable(false);
        this.descTextLabel.setOpaque(false);
 
        final String resultOperationIconTooltip;
        switch (type) {
            case GENERATED:
                this.resultTextLabel.setText(Messages.getString("SignResultPanel.2")); //$NON-NLS-1$
                this.descTextLabel.setText(Messages.getString("SignResultPanel.3")); //$NON-NLS-1$
                resultOperationIconTooltip = Messages.getString("SignResultPanel.4"); //$NON-NLS-1$
                break;
            case KO:
                this.resultTextLabel.setText(Messages.getString("SignResultPanel.5")); //$NON-NLS-1$
                this.descTextLabel.setText(Messages.getString("SignResultPanel.6")); //$NON-NLS-1$
                resultOperationIconTooltip = Messages.getString("SignResultPanel.7"); //$NON-NLS-1$
                break;
            default:
                this.resultTextLabel.setText(Messages.getString("SignResultPanel.8")); //$NON-NLS-1$
                this.descTextLabel.setText(Messages.getString("SignResultPanel.9")); //$NON-NLS-1$
                resultOperationIconTooltip = Messages.getString("SignResultPanel.10"); //$NON-NLS-1$
                break;
        }
        resultOperationIcon.setToolTipText(resultOperationIconTooltip);
        
        this.resultTextLabel.setFont(this.getFont().deriveFont(Font.BOLD, this.getFont().getSize() + 8));

        this.setLayout(new GridBagLayout());
        setBackground(SimpleAfirma.WINDOW_COLOR);
        
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
