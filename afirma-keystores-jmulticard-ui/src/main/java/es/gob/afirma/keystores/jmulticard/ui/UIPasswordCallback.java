/*
 * Controlador Java de la Secretaria de Estado de Administraciones Publicas
 * para el DNI electronico.
 *
 * El Controlador Java para el DNI electronico es un proveedor de seguridad de JCA/JCE
 * que permite el acceso y uso del DNI electronico en aplicaciones Java de terceros
 * para la realizacion de procesos de autenticacion, firma electronica y validacion
 * de firma. Para ello, se implementan las funcionalidades KeyStore y Signature para
 * el acceso a los certificados y claves del DNI electronico, asi como la realizacion
 * de operaciones criptograficas de firma con el DNI electronico. El Controlador ha
 * sido disenado para su funcionamiento independiente del sistema operativo final.
 *
 * Copyright (C) 2012 Direccion General de Modernizacion Administrativa, Procedimientos
 * e Impulso de la Administracion Electronica
 *
 * Este programa es software libre y utiliza un licenciamiento dual (LGPL 2.1+
 * o EUPL 1.1+), lo cual significa que los usuarios podran elegir bajo cual de las
 * licencias desean utilizar el codigo fuente. Su eleccion debera reflejarse
 * en las aplicaciones que integren o distribuyan el Controlador, ya que determinara
 * su compatibilidad con otros componentes.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la
 * Lesser GNU General Public License publicada por la Free Software Foundation,
 * tanto en la version 2.1 de la Licencia, o en una version posterior.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los terminos de la
 * European Union Public License publicada por la Comision Europea,
 * tanto en la version 1.1 de la Licencia, o en una version posterior.
 *
 * Deberia recibir una copia de la GNU Lesser General Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://www.gnu.org/licenses/>.
 *
 * Deberia recibir una copia de la European Union Public License, si aplica, junto
 * con este programa. Si no, consultelo en <http://joinup.ec.europa.eu/software/page/eupl>.
 *
 * Este programa es distribuido con la esperanza de que sea util, pero
 * SIN NINGUNA GARANTIA; incluso sin la garantia implicita de comercializacion
 * o idoneidad para un proposito particular.
 */
package es.gob.afirma.keystores.jmulticard.ui;

import java.awt.Component;
import java.awt.Dimension;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;

import es.gob.jmulticard.CancelledOperationException;

/** <i>PasswordCallbak</i> que muestra un di&aacute;logo para solicitar una
 * contrase&ntilde;a. */
public final class UIPasswordCallback extends PasswordCallback {

    private static final long serialVersionUID = 1719174318602363633L;

    private static final String DNI_LOGO = "/images/dnie_logo.png"; //$NON-NLS-1$

    /** Mensaje que se va a mostrar. */
    private transient final String message;

    /** Componente padre sobre el que se mostrar&aacute; el di&aacute;logo para
     * la inserci&oacute;n de la contrase&ntilde;a. */
    private transient final Component parent;

    /** T&iacute;tulo del di&aacute;logo. */
    private transient final String title;

    /** Crea una <i>CallBack</i> para solicitar al usuario una contrase&ntilde;a
     * mediante un di&aacute;logo gr&aacute;fico. La contrase&ntilde;a no se
     * retiene ni almacena internamente en ning&uacute;n momento.
     * @param prompt Texto del di&aacute;logo para solicitar la contrase&ntilde;a.
     * @param parentComponent Componente padre para la modalidad del di&aacute;logo.
     * @param dialogMessage Mensaje.
     * @param dialogTitle T&iacute;tulo del di&aacute;logo. */
    public UIPasswordCallback(final String prompt,
    		                  final Object parentComponent,
    		                  final String dialogMessage,
    		                  final String dialogTitle) {
        super(prompt, false);
        this.parent = parentComponent instanceof Component ? (Component) parentComponent : null;
        if (prompt != null) {
            this.message = prompt;
        }
        else {
            this.message = dialogMessage;
        }
        this.title = dialogTitle;
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

        final JOptionPane pane = new JOptionPane(
    		panel,
    		JOptionPane.QUESTION_MESSAGE,
    		JOptionPane.OK_CANCEL_OPTION,
    		new ImageIcon(this.getClass().getResource(DNI_LOGO))
		) {
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
