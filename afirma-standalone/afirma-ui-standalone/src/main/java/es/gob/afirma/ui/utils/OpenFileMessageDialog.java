/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.utils;

import java.awt.Component;
import java.io.File;

import javax.swing.JOptionPane;

/** Clase con m&eacute;todos para mostrar un di&aacute;logo que pide al usuario
 * confirmaci&oacute;n para abrir un fichero. */
public final class OpenFileMessageDialog {

	private OpenFileMessageDialog() {
		// No permitimos la instanciacion
	}

    /** Muestra un di&aacute;logo que consulta al usuario si desea abrir un fichero.
     * @param parent Componente padre sobre el que se muestra el i&aacute;logo.
     * @param message Contenido del panel del di&aacute;logo.
     * @param title T&iacute;tulo del di&aacute;logo.
     * @param file Fichero que se desea abrir. */
    public static void show(final Component parent, final Object message, final String title, final File file) {
        show(parent, message, title, file, JOptionPane.INFORMATION_MESSAGE);
    }

    /** Muestra un di&aacute;logo que consulta al usuario si desea abrir un fichero.
     * @param parent Componente padre sobre el que se muestra el i&aacute;logo.
     * @param message Contenido del panel del di&aacute;logo.
     * @param title T&iacute;tulo del di&aacute;logo.
     * @param file Fichero que se desea abrir.
     * @param optionType Tipo de di&aacute;logo. */
    public static void show(final Component parent, final Object message, final String title, final File file, final int optionType) {
        if (file == null) {
            throw new IllegalArgumentException("No se ha indicado el fichero que desea abrir"); //$NON-NLS-1$
        }
        if (CustomDialog.showConfirmDialog(parent, true, (String) message, title, JOptionPane.OK_CANCEL_OPTION, optionType) == JOptionPane.OK_OPTION) {
            Utils.openFile(file.getAbsolutePath());
        }
    }
}
