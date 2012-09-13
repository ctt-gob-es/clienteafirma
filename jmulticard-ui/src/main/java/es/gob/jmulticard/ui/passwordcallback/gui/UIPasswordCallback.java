/*
 * Controlador Java de la Secretaria de Estado de Administraciones PÃºblicas
 * para el DNI electrÃ³nico.
 *
 * El Controlador Java para el DNI electrÃ³nico es un proveedor de seguridad de JCA/JCE
 * que permite el acceso y uso del DNI electrÃ³nico en aplicaciones Java de terceros
 * para la realizaciÃ³n de procesos de autenticaciÃ³n, firma electrÃ³nica y validaciÃ³n
 * de firma. Para ello, se implementan las funcionalidades KeyStore y Signature para
 * el acceso a los certificados y claves del DNI electrÃ³nico, asi como la realizaciÃ³n
 * de operaciones criptogrÃ¡ficas de firma con el DNI electrÃ³nico. El Controlador ha
 * sido diseÃ±ado para su funcionamiento independiente del sistema operativo final.
 *
 * Copyright (C) 2012 DirecciÃ³n General de ModernizaciÃ³n Administrativa, Procedimientos
 * e Impulso de la AdministraciÃ³n ElectrÃ³nica
 *
 * Este programa es software libre y utiliza un licenciamiento dual (LGPL 2.1+
 * o EUPL 1.1+), lo cual significa que los usuarios podrÃ¡n elegir bajo cual de las
 * licencias desean utilizar el cÃ³digo fuente. Su elecciÃ³n deberÃ¡ reflejarse
 * en las aplicaciones que integren o distribuyan el Controlador, ya que determinarÃ¡
 * su compatibilidad con otros componentes.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los tÃ©rminos de la
 * Lesser GNU General Public License publicada por la Free Software Foundation,
 * tanto en la versiÃ³n 2.1 de la Licencia, o en una versiÃ³n posterior.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los tÃ©rminos de la
 * European Union Public License publicada por la ComisiÃ³n Europea,
 * tanto en la versiÃ³n 1.1 de la Licencia, o en una versiÃ³n posterior.
 *
 * Deberia recibir una copia de la GNU Lesser General Public License, si aplica, junto
 * con este programa. Si no, consÃºltelo en <http://www.gnu.org/licenses/>.
 *
 * Deberia recibir una copia de la European Union Public License, si aplica, junto
 * con este programa. Si no, consÃºltelo en <http://joinup.ec.europa.eu/software/page/eupl>.
 *
 * Este programa es distribuido con la esperanza de que sea Ãºtil, pero
 * SIN NINGUNA GARANTÃ�A; incluso sin la garantia implicita de comercializaciÃ³n
 * o idoneidad para un propÃ³sito particular.
 */
package es.gob.jmulticard.ui.passwordcallback.gui;

import java.awt.Component;
import java.awt.Dimension;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;

import es.gob.jmulticard.ui.passwordcallback.CancelledOperationException;

/** <i>PasswordCallbak</i> que muestra un di&aacute;logo para solicitar una
 * contrase&ntilde;a. */
public class UIPasswordCallback extends PasswordCallback {

    private static final long serialVersionUID = 1719174318602363633L;

    /** Mensaje que se va a mostrar. */
    private String message = null;

    /** Componente padre sobre el que se mostrar&aacute; el di&aacute;logo para
     * la inserci&oacute;n de la contrase&ntilde;a. */
    private Component parent = null;

    /** T&iacute;tulo del di&aacute;logo. */
    private String title = null;

    /** Crea una <i>CallBack</i> para solicitar al usuario una contrase&ntilde;a
     * mediante un di&aacute;logo gr&aacute;fico. La contrase&ntilde;a no se
     * retiene ni almacena internamente en ning&uacute;n momento
     * @param prompt Texto del di&aacute;logo para solicitar la contrase&ntilde;a
     * @param parent Componente padre para la modalidad del di&aacute;logo
     * @param message Mensaje
     * @param title T&iacute;tulo del di&aacute;logo */
    public UIPasswordCallback(final String prompt, final Component parent, final String message, final String title) {
        super(prompt, false);
        this.parent = parent;
        if (prompt != null) {
            this.message = prompt;
        }
        else {
            this.message = message;
        }
        this.title = title;
    }

    @Override
    public char[] getPassword() {

    	final JPasswordField pwd = new JPasswordField(10);
        final JLabel lbText = new JLabel(this.message);
        lbText.setMinimumSize(new Dimension(lbText.getFontMetrics(lbText.getFont()).stringWidth(this.message), lbText.getSize().height));
        lbText.setLabelFor(pwd);
        final JPanel panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        panel.add(lbText);
        panel.add(pwd);

        final JOptionPane pane = new JOptionPane(panel, JOptionPane.QUESTION_MESSAGE, JOptionPane.OK_CANCEL_OPTION, new ImageIcon(this.getClass().getResource("/dnie_logo.png"))) { //$NON-NLS-1$
            private static final long serialVersionUID = -3012522768561175760L;
            @Override
            public void selectInitialValue() {
                pwd.requestFocusInWindow();
            }
        };
        pane.createDialog(this.parent, this.title).setVisible(true);

        final Object selectedValue = pane.getValue();
        if (selectedValue == null) {
            return new char[0];
        }
        if (((Integer) selectedValue).intValue() == JOptionPane.OK_OPTION) {
            return pwd.getPassword();
        }
        throw new CancelledOperationException(
            "La insercion de contrasena ha sido cancelada por el usuario" //$NON-NLS-1$
        );
    }
}
