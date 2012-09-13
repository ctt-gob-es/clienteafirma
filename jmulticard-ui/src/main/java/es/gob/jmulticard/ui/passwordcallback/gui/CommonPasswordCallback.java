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

import java.awt.Frame;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.JOptionPane;

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
    			UIPasswordCallback psc = new UIPasswordCallback(getPrompt(), PasswordCallbackManager.getDialogOwner(), getPrompt(), this.title);
    			final char[] pss = psc.getPassword();
    			psc.clearPassword();
    			psc = null;
    			if(pss.length < 8 || pss.length > 16) {
    				JOptionPane.showMessageDialog(PasswordCallbackManager.getDialogOwner(), Messages.getString("CommonPasswordCallback.5"), Messages.getString("CommonPasswordCallback.6"), JOptionPane.WARNING_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
    				return getPassword();
    			}
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
