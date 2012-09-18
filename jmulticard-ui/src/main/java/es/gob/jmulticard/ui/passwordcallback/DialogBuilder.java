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
package es.gob.jmulticard.ui.passwordcallback;

import java.awt.Component;
import java.io.Console;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.Locale;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.jmulticard.ui.passwordcallback.gui.CustomDialog;

/** Gestor de di&aacute;logos gr&aacute;ficos.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class DialogBuilder {
    
private static boolean headless = false;
    
    static void setHeadLess(boolean hl) {
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

    private DialogBuilder() {
        /* Constructor privado */
    }

    /** Muestra un di&aacute;logo para la confirmaci&oacute;n de una operaci&oacute;n con clave privada
     * @param parent Componente padre para la modalidad
     * @param digitalSignCert <code>true</code> si la operaci&oacute;n se hace con una clave destinada a
     *        firmas electr&oacute;nicas, <code>false</code> si la clave est&aacute;
     *        destinada a la autenticaci&oacute;n
     * @return <code>0</code> si el usuario acepta, <code>1</code> si rechaza hacer la operaci&oacute;n con
     *         clave privada */
    public static int showSignatureConfirmDialog(final Component parent, final boolean digitalSignCert) {
        if (!headless) {
            try {
                return CustomDialog.showConfirmDialog((parent != null) ? parent : PasswordCallbackManager.getDialogOwner(),
                                                      true,
                                                      (digitalSignCert) ? Messages.getString("DialogBuilder.2") : Messages.getString("DialogBuilder.3"), //$NON-NLS-1$ //$NON-NLS-2$
                                                      Messages.getString("DialogBuilder.1"), //$NON-NLS-1$
                                                      JOptionPane.YES_NO_OPTION,
                                                      JOptionPane.WARNING_MESSAGE);
            }
            catch (final java.awt.HeadlessException e) {
                Logger.getLogger("es.gob.jmulticard").info("No hay entorno grafico, se revierte a consola: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            }
        }
        final Console console = System.console();
        if (console == null) {
            throw new NoConsoleException("No hay consola para solicitar el PIN"); //$NON-NLS-1$
        }
        return getConsoleConfirm(console, digitalSignCert);
    }
    
    private static int getConsoleConfirm(final Console console, final boolean digitalSignCert) {
        console.printf(((digitalSignCert) ? Messages.getString("DialogBuilder.2") : Messages.getString("DialogBuilder.3")) + " " + Messages.getString("DialogBuilder.4") + "\n"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
        final String confirm = console.readLine().replace("\n", "").replace("\r", "").trim().toLowerCase(Locale.getDefault()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        if ("si".equals(confirm) //$NON-NLS-1$
                || "s".equals(confirm) //$NON-NLS-1$
                || "s\u00ED".equals(confirm)) { //$NON-NLS-1$
                return 0;
        }
        else if ("no".equals(confirm) || "n".equals(confirm)) { //$NON-NLS-1$ //$NON-NLS-2$
        	return 1;
        }
        else return getConsoleConfirm(console, digitalSignCert);
    }
    
}