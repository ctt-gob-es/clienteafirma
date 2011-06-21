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
                          .getResourceAsStream("/resources/" + (type.equals(SIGN_DETAIL_TYPE.KO)
                                ? "ko_icon.svg"
                                : "ok_icon.svg"))));
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido cargar el icono de resultado o validez de firma, este no se mostrara: " + e);
        }

        this.descTextLabel.setContentType("text/html");
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
                                "No ha sido posible recuperar la informaci\u00F3n adicional,\n,pruebe a abrir la siguiente URL desde un navegador Web:\n" + he.getURL(),
                                "Error",
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
                this.resultTextLabel.setText("Proceso de firma completado satisfactoriamente");
                this.descTextLabel.setText("<html><p>La firma cumple con los requisitos del esquema nacional de interoperabilidad en cuanto a firmas digitales y documentos firmados. <a href=\"http://www.google.com/\">M&aacute;s informaci&oacute;n en la Web</a>.</p></html>");
                resultOperationIconTooltip = "Se ha generado correctamente una firma electr\u00F3nica";
                break;
            case KO:
                this.resultTextLabel.setText("La firma no es v\u00E1lida o no es una firma compatible con @firma");
                this.descTextLabel.setText("<html><p>La firma electr\u00F3nica seleccionada no es v\u00E1lida o no es compatible con @firma.</p></html>");
                resultOperationIconTooltip = "La firma electr\u00F3nica seleccionada no es v\u00E1lida o no es compatible con @firma";
                break;
            default:
                this.resultTextLabel.setText("La firma es v\u00E1lida");
                this.descTextLabel.setText("<html><p>Para determinar la completa validez legal debe comprobar adem\u00E1s la validez de los certificados usados para firmar</p></html>");
                resultOperationIconTooltip = "La firma electr\u00F3nica es v\u00E1lida en cuanto a estructura";
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
