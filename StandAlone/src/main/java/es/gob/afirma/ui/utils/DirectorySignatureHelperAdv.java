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

import java.io.File;

import javax.swing.JDialog;

import es.gob.afirma.exceptions.AOUnsupportedSignFormatException;
import es.gob.afirma.misc.DirectorySignatureHelper;

/**
 * M&oacute;dulo para la ejecuci&oacute;n de firmas y multifirmas de ficheros. Durante el proceso
 * se muestra una barra de progreso que informa de la situaci&oacute;n.
 */
public class DirectorySignatureHelperAdv extends DirectorySignatureHelper {

	/** Componente padre sobre el que se mostrara el di&aacute;logo con la barra de progreso. */
	private JDialog parent = null;
  
	/** Dialogo con la barra de progreso. */
	private ProgressDialog progressDialog = null;
	
	/**
	 * Crea un instancia de la clase con una configuraci&oacute;n y un componente padre asignado.
	 * @param algorithm Algoritmo de firma electr&oacute;nica.
	 * @param format Formato de firma por defecto.
	 * @param mode Modo de firma.
	 * @param parent Componente padre.
	 * @throws AOUnsupportedSignFormatException 
	 */
	public DirectorySignatureHelperAdv(String algorithm, String format, String mode, JDialog parent) throws AOUnsupportedSignFormatException {
		super(algorithm, format, mode);
		this.parent = parent;
	}
	
	@Override
	protected void prepareOperation(File[] files) {
		progressDialog = new ProgressDialog(this.parent, files.length, Messages.getString("Wizard.multifirma.progress.titulo"));
		progressDialog.show();
	}
	
	@Override
	protected void disposeOperation() {
		progressDialog.close();
	}
	
	@Override
	protected void preProcessFile(File file) {
		progressDialog.processElement(file.getAbsolutePath());
	}
}
