/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.callbacks;

import java.awt.Component;
import java.awt.Dimension;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;

import es.gob.afirma.exceptions.AOCancelledOperationException;

/** <i>PasswordCallbak</i> que muestra un di&aacute;logo Swing para solicitar una contrase&ntilde;a. */
public final class UIPasswordCallbackLite extends PasswordCallback {

    private static final long serialVersionUID = 1719174318602363633L;

    /** Componente padre sobre el que se mostrar&aacute; el di&aacute;logo para la inserci&oacute;n
     * de la contrase&ntilde;a. */
    private Component parent = null;

    /** Crea una <i>CallBack</i> para solicitar al usuario una contrase&ntilde;a mediante
     * un di&aacute;logo gr&aacute;fico. La contrase&ntilde;a no se retiene ni almacena
     * internamente en ning&uacute;n momento
     * @param prompt Texto del di&aacute;logo para solicitar la contrase&ntilde;a
     * @param parent Componente padre para la modalidad del di&aacute;logo */
    public UIPasswordCallbackLite(final String prompt, final Component parent) {
        super(prompt, false);
        this.parent = parent;
    }

    @Override
    public char[] getPassword() {
        return getPassword(this.getPrompt(), this.parent);
    }

    /** Muestra un di&aacute;logo para pedir una contrase&ntilde;a al usuario.
     * @param text Texto con el que se solicitar&aacute; la entrada de texto al usuario (<i>prompt</i>)
     * @param charSet Juego de caracteres aceptados para la contrase&ntilde;a
     * @param beep <code>true</code> si se desea un sonido de advertencia al introducir un caracter
     *        no v&aacute;lido, <code>false</code> en caso contrario
     * @param c Componente padre (para la modalidad)
     * @return Array de caracteres del texto introducido como contrase&ntilde;a
     * @throws AOCancelledOperationException Cuando el usuario cancela o cierra el di&aacute;logo */
    private final static char[] getPassword(String text, final Component c) throws AOCancelledOperationException {
        if (text == null) {
            text = "Introduzca la contrase\u00F1a";
        }
        final JPasswordField pwd = new JPasswordField(10);
        final JLabel lbText = new JLabel(text);
        lbText.setMinimumSize(new Dimension(lbText.getFontMetrics(lbText.getFont()).stringWidth(text), lbText.getSize().height));
        lbText.setLabelFor(pwd);
        final JPanel panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        panel.add(lbText);
        panel.add(pwd);
        final int action = JOptionPane.showConfirmDialog(c, panel, "Contrase\u00F1a", JOptionPane.OK_CANCEL_OPTION);
        if (!(action == JOptionPane.OK_OPTION)) {
            throw new AOCancelledOperationException("La insercion de contrasena ha sido cancelada por el usuario"); //$NON-NLS-1$
        }
        return pwd.getPassword();
    }

}
