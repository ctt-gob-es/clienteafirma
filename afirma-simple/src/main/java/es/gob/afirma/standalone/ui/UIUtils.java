/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.Component;

import javax.swing.JOptionPane;

import es.gob.afirma.standalone.SimpleAfirmaMessages;

/**
 * Utilidades generales para los interfaces gr&aacute;ficos.
 */
public final class UIUtils {

	private UIUtils() {
		// No permitimos la instanciacion
	}

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
        final String buttonTxt = SimpleAfirmaMessages.getString("UIUtils.0"); //$NON-NLS-1$
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

}
