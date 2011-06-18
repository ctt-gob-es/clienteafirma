package es.gob.afirma.standalone.ui;

import java.awt.Component;
import java.awt.Dimension;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;

import es.gob.afirma.exceptions.AOCancelledOperationException;
import es.gob.afirma.standalone.SimpleAfirma;

public final class DNIePasswordCallback extends PasswordCallback {

    private static final long serialVersionUID = -3568711790506952344L;

    public DNIePasswordCallback(Component c) {
        super("", false);
        parent = c;
    }
    /**
     * Componente padre sobre el que se mostrar&aacute; el di&aacute;logo para
     * la inserci&oacute;n de la contrase&ntilde;a.
     */
    private final Component parent;
    
    @Override
    public char[] getPassword() {
        final String text = "Introduzca el PIN de su DNI electrónico";
        final JPasswordField pwd = new JPasswordField(10);
        final JLabel lbText = new JLabel(text);
        lbText.setMinimumSize(new Dimension(lbText.getFontMetrics(lbText.getFont()).stringWidth(text), lbText.getSize().height));
        lbText.setLabelFor(pwd);
        final JPanel panel = new JPanel();
        panel.setBackground(SimpleAfirma.WINDOW_COLOR);
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        panel.add(lbText);
        panel.add(pwd);
        
        final JOptionPane pane = new JOptionPane(panel, JOptionPane.QUESTION_MESSAGE, JOptionPane.OK_CANCEL_OPTION) {
            private static final long serialVersionUID = -3012522768561175760L;
            @Override
            public void selectInitialValue() {
                pwd.requestFocusInWindow();
            }
        };
        pane.createDialog(parent, "Introduzca el PIN de su DNIe").setVisible(true);

        final Object selectedValue = pane.getValue();
        if (selectedValue == null) {
            return new char[0];
        }
        if (((Integer) selectedValue).intValue() == JOptionPane.OK_OPTION) {
            return pwd.getPassword();
        }
        throw new AOCancelledOperationException(
            "La insercion de contrasena ha sido cancelada por el usuario" //$NON-NLS-1$
        );
    }
}
