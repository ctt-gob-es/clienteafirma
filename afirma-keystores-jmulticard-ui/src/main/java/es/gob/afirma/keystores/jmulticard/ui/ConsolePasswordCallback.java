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

import java.io.Console;
import java.util.Arrays;

import javax.security.auth.callback.PasswordCallback;

import es.gob.jmulticard.CancelledOperationException;

/** Solicitud de contrase&ntilde;a por consola. */
final class ConsolePasswordCallback extends PasswordCallback {

	private static final long serialVersionUID = -4044214967262414899L;

	/** Construye una <i>PasswordCallback</i> para solicitud de contrase&ntilde;a por consola
	 * @param p Texto de solicitud */
	ConsolePasswordCallback(final String p) {
		super(p, true);
		this.prompt = p;
	}

	private transient final String prompt;

	/** Elimina elementos de HTML que puedan venir en los mensajes de <i>prompt</i>.
	 * @param input <i>prompt</i>.
	 * @return <i>prompt</i> sin caracteres HTML. */
	private static String removeHTML(final String input) {
		//Extender en caso de que aparezcan mas caracteres HTML en las properties
	    return input.replace("<br/>", "").replace("&nbsp;", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
	}

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
            Arrays.fill(password, '\0');
            return getPassword();
        }

        console.flush();
        console = null;

        return password;
    }

}
