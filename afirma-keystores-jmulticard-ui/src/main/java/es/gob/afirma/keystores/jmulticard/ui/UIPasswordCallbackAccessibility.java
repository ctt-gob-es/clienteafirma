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

import javax.security.auth.callback.PasswordCallback;

/** <i>PasswordCallbak</i> que muestra un di&aacute;logo accesible para
 * solicitar una contrase&ntilde;a. */
public final class UIPasswordCallbackAccessibility extends PasswordCallback {

    private static final long serialVersionUID = 1719174318602363633L;

    /** Mensaje que se va a mostrar. */
    private transient final String message;

    /** Atajo para el campo de inserci&oacute;n de contrasenia. */
    private transient final int mnemonic;

    /** Componente padre sobre el que se mostrar&aacute; el di&aacute;logo para
     * la inserci&oacute;n de la contrase&ntilde;a. */
    private transient final Component parent;

    /** T&iacute;tulo del di&aacute;logo. */
    private transient final String title;

    /** Ruta hacia el fichero de icono del di&aacute;logo. */
    private transient final String iconPath;

    /** Si se permite o no <i>cachear</i> el PIN. */
    private transient final boolean allowUseCache;

    /** Si por defecto se debe mostrar o no la casilla "No volver a preguntar" para
     * <i>cachear</i> el PIN. */
    private transient final boolean defaultUseCache;

    /** Si se muestra o no la casilla "No volver a preguntar" para <i>cachear</i> el PIN. */
    private transient boolean useCache;

    /** Crea una <i>Callback</i> para solicitar al usuario una contrase&ntilde;a
     * mediante un di&aacute;logo gr&aacute;fico. La contrase&ntilde;a no se
     * retiene ni almacena internamente en ning&uacute;n momento.
     * @param prompt Texto del di&aacute;logo para solicitar la contrase&ntilde;a.
     * @param parentComponent Componente padre para la modalidad del di&aacute;logo.
     * @param dialogMessage Mensaje.
     * @param textFieldMnemonic Mnem&oacute;nico para el propio campo de texto.
     * @param dialogTitle T&iacute;tulo del di&aacute;logo.
     * @param iconFileName Ruta hacia el icono del di&aacute;logo.
     * @param allowDniCache Hace mostrarse la casilla para seleccionar el <i>cacheo</i> del PIN.
     * @param defaultDniCache Valor por defecto de la opci&oacute;n de <i>cacheo</i> de PIN. */
    public UIPasswordCallbackAccessibility(final String prompt,
    		                               final Component parentComponent,
    		                               final String dialogMessage,
    		                               final int textFieldMnemonic,
    		                               final String dialogTitle,
    		                               final String iconFileName,
    		                               final boolean allowDniCache,
    		                               final boolean defaultDniCache) {
        super(prompt, false);
        this.parent = parentComponent;
        if (prompt != null) {
            this.message = prompt;
        }
        else {
            this.message = dialogMessage;
        }
        this.mnemonic = textFieldMnemonic;
        this.title = dialogTitle;
        this.iconPath = iconFileName;
        this.allowUseCache = allowDniCache;
        this.defaultUseCache = defaultDniCache;
        this.useCache = false;
    }

    @Override
    public char[] getPassword() {
    	PasswordResult result = InputPasswordSmartcardDialog.showInputPasswordDialog(
			this.parent,
			true, // Modal
			this.message,
			this.mnemonic,
			this.title,
			this.iconPath,
			this.allowUseCache,
			this.defaultUseCache
    	);
    	this.useCache = result.isUseCache();
    	final char[] password = result.getPassword();
    	result.clear();
    	result = null;

    	return password;
    }

    /** Indica si est&aacute; establecido el uso de cach&eacute; en el PIN.
     * @return <code>true</code> si est&aacute; establecido el uso de cach&eacute; en el PIN,
     *         <code>false</code> en caso contrario. */
    public boolean isUseCacheChecked() {
    	return this.useCache;
    }
}
