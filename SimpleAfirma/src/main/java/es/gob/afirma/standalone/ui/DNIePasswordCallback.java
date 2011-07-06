package es.gob.afirma.standalone.ui;

import java.awt.Component;
import java.awt.Dimension;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;

import es.gob.afirma.exceptions.AOCancelledOperationException;
import es.gob.afirma.standalone.Messages;

/**
 * Di&aacute;logo para solicitar el PIN del DNIe.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 *
 */
public final class DNIePasswordCallback extends PasswordCallback {

    private static final long serialVersionUID = -3568711790506952344L;

    /**
     * Contruye un di&aacute;logo para solicitar el PIN del DNIe.
     * @param c Componente padre para la modalidad
     */
    public DNIePasswordCallback(final Component c) {
        super("DNI", false); //$NON-NLS-1$
        this.parent = c;
    }
    /**
     * Componente padre sobre el que se mostrar&aacute; el di&aacute;logo para
     * la inserci&oacute;n de la contrase&ntilde;a.
     */
    private final Component parent;

    @Override
    public char[] getPassword() {
        final String text = Messages.getString("DNIePasswordCallback.1"); //$NON-NLS-1$
        final JPasswordField pwd = new JPasswordField(10);
        final JLabel lbText = new JLabel(text);
        lbText.setMinimumSize(new Dimension(lbText.getFontMetrics(lbText.getFont()).stringWidth(text), lbText.getSize().height));
        lbText.setLabelFor(pwd);
        final JPanel panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        panel.add(lbText);
        panel.add(pwd);

        final JOptionPane pane = new JOptionPane(panel, JOptionPane.QUESTION_MESSAGE, JOptionPane.OK_CANCEL_OPTION, new ImageIcon(this.getClass().getResource("/resources/dnie_logo.png"))) { //$NON-NLS-1$
            private static final long serialVersionUID = -3012522768561175760L;
            @Override
            public void selectInitialValue() {
                pwd.requestFocusInWindow();
            }
        };
        pane.createDialog(this.parent, Messages.getString("DNIePasswordCallback.3")).setVisible(true); //$NON-NLS-1$

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
