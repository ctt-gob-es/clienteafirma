package es.gob.afirma.ui.utils;

import java.awt.Component;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.JOptionPane;


/** <i>PasswordCallbak</i> que muestra un di&aacute;logo para solicitar una
 * contrase&ntilde;a. */
public class UIPasswordCallbackAccessibility extends PasswordCallback {

    private static final long serialVersionUID = 1719174318602363633L;

    /** Componente padre sobre el que se mostrar&aacute; el di&aacute;logo para
     * la inserci&oacute;n de la contrase&ntilde;a. */
    private Component parent = null;
    
    /**
     * Mensaje que se va a mostrar.
     */
    private String message = null;
    
    /**
     * Título del diálogo.
     */
    private String title = null;

    /** Crea una <i>CallBack</i> para solicitar al usuario una contrase&ntilde;a
     * mediante un di&aacute;logo gr&aacute;fico. La contrase&ntilde;a no se
     * retiene ni almacena internamente en ning&uacute;n momento
     * @param prompt
     *        Texto del di&aacute;logo para solicitar la contrase&ntilde;a
     * @param parent
     *        Componente padre para la modalidad del di&aacute;logo */
    public UIPasswordCallbackAccessibility(final String prompt, final Component parent, final String message, final String title) {
        super(prompt, false);
        this.parent = parent;
        if (prompt != null) {
        	this.message = prompt;
        } else {
        	this.message = message;
        }
        this.title = title;
    }

    @Override
    public char[] getPassword() {
    	
        return CustomDialog.showInputPasswordDialog(parent, true, null, false, message, title, JOptionPane.QUESTION_MESSAGE);
    }
}