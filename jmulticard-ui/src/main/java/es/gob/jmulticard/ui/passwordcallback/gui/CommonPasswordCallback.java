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

import java.awt.Frame;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

import es.gob.jmulticard.ui.passwordcallback.Messages;
import es.gob.jmulticard.ui.passwordcallback.PasswordCallbackManager;

/** <i>PasswordCallback</i> que funciona en modo gr&aacute;fico pero revirtiendo a consola
 * en caso de un <code>java.awt.HeadLessException</code>.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CommonPasswordCallback extends PasswordCallback {

    private static boolean headless = false;

    static void setHeadLess(final boolean hl) {
        headless = hl;
    }

    static {
        AccessController.doPrivileged(new PrivilegedAction<Void>() {
            @Override
            public Void run() {
                setHeadLess(Boolean.getBoolean("java.awt.headless")); //$NON-NLS-1$
                return null;
            }
        });
    }

	private static final long serialVersionUID = 5514503307266079255L;

	private final String title;

	/** Construye un <i>PasswordCallback</i> que funciona en modo gr&aacute;fico pero revirtiendo a consola
     * en caso de un <code>java.awt.HeadLessException</code>.
	 * @param prompt Texto para la solicitud de la contrase&ntilde;a
	 * @param title T&iacute;tulo de la ventana gr&aacute;fica
	 * @param parent Componente padre para la modalidad. Si se indica <code>null</code>
	 *               se intenta hacer una modalidad a nivel de <i>toolkit</i> */
	private CommonPasswordCallback(final String prompt, final String title) {
		super(prompt, true);
		if (prompt == null) {
			throw new IllegalArgumentException("El texto de solicitud no puede ser nulo"); //$NON-NLS-1$
		}
		if (title == null) {
			this.title = prompt;
		}
		else {
			this.title = title;
		}
	}

	@Override
    public char[] getPassword() {
	    if (!headless) {
    		try {
    			UIPasswordCallbackAccessibility psc = new UIPasswordCallbackAccessibility(getPrompt(), PasswordCallbackManager.getDialogOwner(), getPrompt(), 'P', this.title);
    			final char[] pss = psc.getPassword();
    			psc.clearPassword();
    			psc = null;

    			return pss;
    		}
    		catch(final java.awt.HeadlessException e) {
    		    Logger.getLogger("es.gob.jmulticard").info("No hay entorno grafico, se revierte a consola: " + e); //$NON-NLS-1$ //$NON-NLS-2$
    		}
	    }
	    ConsolePasswordCallback cpc = new ConsolePasswordCallback(getPrompt());
	    final char[] pss = cpc.getPassword();
	    cpc.clearPassword();
	    cpc = null;

	    return pss;
    }

	/** Obtiene un di&aacute;logo de solicitd de PIN tras una introducci&oacute;n erronea.
	 * @param parent Componente padre para la modalidad
	 * @param retriesLeft Intentos restantes antes de bloquear el DNIe
	 * @return <i>PasswordCallback</i> de solicitd de PIN */
	public static PasswordCallback getDnieBadPinPasswordCallback(final int retriesLeft) {
		return new CommonPasswordCallback(
			Messages.getString("CommonPasswordCallback.0") + " " + Integer.toString(retriesLeft), //$NON-NLS-1$ //$NON-NLS-2$
			Messages.getString("CommonPasswordCallback.1") //$NON-NLS-1$
		);
	}

	/** Obtiene un di&aacute;logo de solicitd de PIN para realizar una lectura de certificados.
	 * @param parent Componente padre para la modalidad
	 * @return <i>PasswordCallback</i> de solicitd de PIN */
	public static PasswordCallback getDniePinForCertificateReadingPasswordCallback(final Frame parent) {
		return new CommonPasswordCallback(
			Messages.getString("CommonPasswordCallback.4"), //$NON-NLS-1$
			Messages.getString("CommonPasswordCallback.1") //$NON-NLS-1$
		);
	}

}
