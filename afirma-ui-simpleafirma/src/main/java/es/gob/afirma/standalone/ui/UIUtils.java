/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.standalone.ui;

import java.awt.Component;

import javax.swing.JOptionPane;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.util.windows.WinRegistryWrapper;
import es.gob.afirma.standalone.Messages;

/**
 * Utilidades generales para los interfaces gr&aacute;ficos.
 */
public final class UIUtils {

    /**
     * Muestra un di&aacute;logo de error de forma modal. Difiere del normal mostrado con <code>JOptionPane</code>
     * en que, siguiendo la gu&iacute;a de estilo de interfaces de Microsoft, el bot&oacute;n no es "OK", sino
     * cerrar
     * @param parent Componente padre para la modalidad
     * @param message Mensaje de error
     * @param title Titulo de la ventana de error
     * @param messageType Tipo de mensaje
     */
    public static void showErrorMessage(final Component parent, final Object message, final String title, final int messageType) {
        
        // Hay un error extrano por el que no llega el texto acotado por admiraciones
        String buttonTxt = Messages.getString(Messages.getString("UIUtils.0")); //$NON-NLS-1$
        if (buttonTxt.startsWith("!") && buttonTxt.endsWith("!")) { //$NON-NLS-1$ //$NON-NLS-2$
            buttonTxt = buttonTxt.substring(1, buttonTxt.length()-1);
        }
        
        JOptionPane.showOptionDialog(
                parent,
                message,
                title,
                JOptionPane.OK_OPTION,
                messageType,
                null,
                new String[] {
                              buttonTxt
                },
                buttonTxt
        );
    }
    
    static boolean hasAssociatedApplication(final String extension) {
        if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
            if (extension == null || "".equals(extension)) { //$NON-NLS-1$
                return false;
            }     
            final Object o = WinRegistryWrapper.get(WinRegistryWrapper.HKEY_CLASSES_ROOT, (extension.startsWith(".")) ? extension : ("." + extension), ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            if (o == null) {
                return false;
            }
            return (WinRegistryWrapper.get(WinRegistryWrapper.HKEY_CLASSES_ROOT, o.toString() + "\\shell\\open\\command", "") != null);  //$NON-NLS-1$//$NON-NLS-2$
        }
        return true;
    }
    
}
