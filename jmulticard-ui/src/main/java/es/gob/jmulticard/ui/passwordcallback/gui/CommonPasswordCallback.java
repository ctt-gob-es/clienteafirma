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
