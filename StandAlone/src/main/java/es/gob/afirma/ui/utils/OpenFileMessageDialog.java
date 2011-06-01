/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */
package es.gob.afirma.ui.utils;

import java.awt.Component;
import java.io.File;

import javax.swing.JOptionPane;

/**
 * Clase con m&eacute;todos para mostrar un di&aacute;logo que pide al usuario
 * confirmaci&oacute;n para abrir un fichero.
 */
public class OpenFileMessageDialog {

	/**
	 * Muestra un di&aacute;logo que consulta al usuario si desea abrir un fichero.
	 * @param parent Componente padre sobre el que se muestra el i&aacute;logo. 
	 * @param message Contenido del panel del di&aacute;logo.
	 * @param title T&iacute;tulo del di&aacute;logo.
	 * @param file Fichero que se desea abrir.
	 */
	public static void show(Component parent, Object message, String title, File file) {
		show(parent, message, title, file, JOptionPane.INFORMATION_MESSAGE);
	}
	
	/**
	 * Muestra un di&aacute;logo que consulta al usuario si desea abrir un fichero.
	 * @param parent Componente padre sobre el que se muestra el i&aacute;logo. 
	 * @param message Contenido del panel del di&aacute;logo.
	 * @param title T&iacute;tulo del di&aacute;logo.
	 * @param file Fichero que se desea abrir.
	 * @param optionType Tipo de di&aacute;logo.
	 */
	public static void show(Component parent, Object message, String title, File file, int optionType) {

		if(file == null)
			throw new NullPointerException("No se ha indicado el fichero que desea abrir");
		
		if(JOptionPane.showConfirmDialog(
				parent, message, title, JOptionPane.OK_CANCEL_OPTION, optionType) == JOptionPane.OK_OPTION)
			Utils.openFile(file.getAbsolutePath());
	}
}
