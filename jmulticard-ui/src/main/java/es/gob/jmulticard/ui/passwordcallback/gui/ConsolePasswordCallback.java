/*
 * Controlador Java de la Secretaría de Estado de Administraciones Públicas
 * para el DNI electrónico.
 *
 * El Controlador Java para el DNI electrónico es un proveedor de seguridad de JCA/JCE 
 * que permite el acceso y uso del DNI electrónico en aplicaciones Java de terceros 
 * para la realización de procesos de autenticación, firma electrónica y validación 
 * de firma. Para ello, se implementan las funcionalidades KeyStore y Signature para 
 * el acceso a los certificados y claves del DNI electrónico, así como la realización 
 * de operaciones criptográficas de firma con el DNI electrónico. El Controlador ha 
 * sido diseñado para su funcionamiento independiente del sistema operativo final.
 * 
 * Copyright (C) 2012 Dirección General de Modernización Administrativa, Procedimientos 
 * e Impulso de la Administración Electrónica
 * 
 * Este programa es software libre y utiliza un licenciamiento dual (LGPL 2.1+
 * o EUPL 1.1+), lo cual significa que los usuarios podrán elegir bajo cual de las
 * licencias desean utilizar el código fuente. Su elección deberá reflejarse 
 * en las aplicaciones que integren o distribuyan el Controlador, ya que determinará
 * su compatibilidad con otros componentes.
 *
 * El Controlador puede ser redistribuido y/o modificado bajo los términos de la 
 * Lesser GNU General Public License publicada por la Free Software Foundation, 
 * tanto en la versión 2.1 de la Licencia, o en una versión posterior.
 * 
 * El Controlador puede ser redistribuido y/o modificado bajo los términos de la 
 * European Union Public License publicada por la Comisión Europea, 
 * tanto en la versión 1.1 de la Licencia, o en una versión posterior.
 * 
 * Debería recibir una copia de la GNU Lesser General Public License, si aplica, junto
 * con este programa. Si no, consúltelo en <http://www.gnu.org/licenses/>.
 * 
 * Debería recibir una copia de la European Union Public License, si aplica, junto
 * con este programa. Si no, consúltelo en <http://joinup.ec.europa.eu/software/page/eupl>.
 *
 * Este programa es distribuido con la esperanza de que sea útil, pero
 * SIN NINGUNA GARANTÍA; incluso sin la garantía implícita de comercialización
 * o idoneidad para un propósito particular.
 */
package es.gob.jmulticard.ui.passwordcallback.gui;

import java.io.Console;

import javax.security.auth.callback.PasswordCallback;

import es.gob.jmulticard.ui.passwordcallback.CancelledOperationException;
import es.gob.jmulticard.ui.passwordcallback.Messages;
import es.gob.jmulticard.ui.passwordcallback.NoConsoleException;

/** Solicitud de contrase&ntilde;a por consola. */
final class ConsolePasswordCallback extends PasswordCallback {

	private static final long serialVersionUID = -4044214967262414899L;

	/** Construye una <i>PasswordCallback</i> para solicitud de contrase&ntilde;a por consola
	 * @param p Texto de solicitud */
	ConsolePasswordCallback(final String p) {
		super(p, true);
		this.prompt = p;
	}

	private final String prompt;

	/**
	 * Elimina elementos de HTML que puedan venir en los mensajes de prompt
	 * @param input prompt
	 * @return prompt sin caracteres HTML
	 */
	private static String removeHTML(final String input) {
		//Extender en caso de que aparezcan mas caracteres HTML en las properties
	    return input.replace("<br/>", "").replaceAll("&nbsp;", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
	}

	/** {@inheritDoc} */
    @Override
    public char[] getPassword() {
        Console console = System.console();
        if (console == null) {
            throw new NoConsoleException("No hay consola para solicitar el PIN"); //$NON-NLS-1$
        }
        final char [] password = console.readPassword(removeHTML(this.prompt) + ":\n"); //$NON-NLS-1$
        if (password == null) {
            throw new CancelledOperationException(
              "Se ha cancelado la introduccion de PIN en consola" //$NON-NLS-1$
            );
        }
        if (password.length < 8 || password.length > 16) {
            console.printf(Messages.getString("ConsolePasswordCallback.1") + "\n"); //$NON-NLS-1$ //$NON-NLS-2$
            for (int i = 0; i < password.length; i++) {
            	password[i] = '\0';
            }
            return getPassword();
        }

        console.flush();
        console = null;

        return password;
    }

}
