package es.gob.afirma.standalone.ui;

import java.awt.Component;

import javax.swing.JOptionPane;

import es.gob.afirma.standalone.Messages;

/**
 * Utilidades generales para los interfaces gr&aacute;ficos.
 */
public class UIUtils {

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

        JOptionPane.showOptionDialog(
                parent,
                message,
                title,
                JOptionPane.OK_OPTION,
                messageType,
                null,
                new Object[] {
                    Messages.getString(Messages.getString("UIUtils.0")) //$NON-NLS-1$
                },
                Messages.getString(Messages.getString("UIUtils.0")) //$NON-NLS-1$
        );
    }
}
