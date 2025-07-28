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

import java.security.KeyStore.PasswordProtection;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

/** <i>PasswordCallback</i> que funciona en modo gr&aacute;fico pero revirtiendo a consola
 * en caso de un <code>java.awt.HeadLessException</code>.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CommonPasswordCallback extends PasswordCallback {

	private static final long serialVersionUID = 5514503307266079255L;

    private static boolean headless = false;

    /** T&iacute;tulo de la ventana gr&aacute;fica donde se vava pedir la contrase&ntilde;a. */
	private transient final String title;

	/** Indica si se est&aacute; o no pidiendo el PIN de un DNIe. */
	private transient final boolean isDnie;

	/** Indica si se debe dar al usuario la opci&oacute;n de recordar el PIN. */
	private transient final boolean allowUseCache;

	/** Indica el valor por defecto de la opci&oacute;n de guardar el PIN que se le
	 * presentar&aacute; al usuario en caso de que se le permita configurarlo. */
	private transient final boolean defaultUseCacheValue;

	/** Indica si el usuario configur&oacute; que desea recordar el PIN. */
	private transient boolean useCacheChecked;

    static {
        setHeadLess(Boolean.getBoolean("java.awt.headless")); //$NON-NLS-1$
    }

    static void setHeadLess(final boolean hl) {
        headless = hl;
    }

	/** Construye un <i>PasswordCallback</i> que funciona en modo gr&aacute;fico pero
	 * revirtiendo a consola en caso de un <code>java.awt.HeadLessException</code>.
	 * @param prompt Texto para la solicitud de la contrase&ntilde;a
	 * @param dialogTitle T&iacute;tulo de la ventana gr&aacute;fica.
	 * @param isDni Si es un DNIe. */
	public CommonPasswordCallback(final String prompt, final String dialogTitle, final boolean isDni) {
		this(prompt, dialogTitle, isDni, false, false);
	}

	/** Construye un <i>PasswordCallback</i> que funciona en modo gr&aacute;fico pero
	 * revirtiendo a consola en caso de un <code>java.awt.HeadLessException</code>.
	 * @param prompt Texto para la solicitud de la contrase&ntilde;a
	 * @param dialogTitle T&iacute;tulo de la ventana gr&aacute;fica.
	 * @param isDni Si es un DNIe.
	 * @param allowPinCache Si se permite el cach&acute; del PIN.
	 * @param defaultPinCacheValue Si por defecto debe usarse el valor del PIN en cach&eacute;. */
	public CommonPasswordCallback(final String prompt,
			                      final String dialogTitle,
			                      final boolean isDni,
			                      final boolean allowPinCache,
			                      final boolean defaultPinCacheValue) {
		super(prompt, true);
		if (prompt == null) {
			throw new IllegalArgumentException("El texto de solicitud no puede ser nulo"); //$NON-NLS-1$
		}
		if (dialogTitle == null) {
			this.title = prompt;
		}
		else {
			this.title = dialogTitle;
		}

		this.isDnie = isDni;
		this.allowUseCache = allowPinCache;
		this.defaultUseCacheValue = defaultPinCacheValue;
		this.useCacheChecked = this.defaultUseCacheValue;
	}

	/** Constructor gen&eacute;rico.
	 * @param pp PasswordProtection para solicitar la contrase&ntilde;a. */
	public CommonPasswordCallback(final PasswordProtection pp) {
		super("Por favor, introduzca el PIN de su tarjeta", false); //$NON-NLS-1$
		if (pp == null) {
			throw new IllegalArgumentException(
				"El PasswordProtection no puede ser nulo" //$NON-NLS-1$
			);
		}
		this.title = getPrompt();
		this.isDnie = false;
		this.allowUseCache = false;
		this.defaultUseCacheValue = false;
		this.useCacheChecked = this.defaultUseCacheValue;
	}

	@Override
    public char[] getPassword() {
	    if (!headless) {
    		try {
    			UIPasswordCallbackAccessibility psc;
    			if (this.isDnie) {
	    			psc = new UIPasswordCallbackAccessibility(
						getPrompt(),
						PasswordCallbackManager.getDialogOwner(),
						getPrompt(),
						'P',
						this.title,
						"/images/dnie.png", //$NON-NLS-1$
						this.allowUseCache,
						this.defaultUseCacheValue
					);
    			}
    			else {
    				psc = new UIPasswordCallbackAccessibility(
						getPrompt(),
						PasswordCallbackManager.getDialogOwner(),
						getPrompt(),
						'P',
						this.title,
						"/images/chipcard.png", //$NON-NLS-1$
						this.allowUseCache,
						this.defaultUseCacheValue
					);
    			}

    			final char[] pss = psc.getPassword();
    			this.useCacheChecked = psc.isUseCacheChecked();
    			psc.clearPassword();
    			psc = null;

    			return pss;
    		}
    		catch(final java.awt.HeadlessException e) {
    		    Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
		    		"No hay entorno grafico, se revierte a consola: " + e //$NON-NLS-1$
	    		);
    		}
	    }
	    ConsolePasswordCallback cpc = new ConsolePasswordCallback(getPrompt());
	    final char[] pss = cpc.getPassword();
	    cpc.clearPassword();
	    cpc = null;
	    return pss;
    }

	/** Indica si el usuario configur&oacute; que se recordase ka contrase&ntilde;a almacenada.
	 * @return {@code true} si el usuario seleccion&oacute; que se recordase la contrase&ntilde;a,
	 * {@code false} en caso contrario. En caso de que no se haya permitido al usuario
	 * seleccionar o no esta opci&oacute;n, se devolver&iacute;a el valor por defecto
	 * configurado. */
	public boolean isUseCacheChecked() {
		return this.useCacheChecked;
	}

}
