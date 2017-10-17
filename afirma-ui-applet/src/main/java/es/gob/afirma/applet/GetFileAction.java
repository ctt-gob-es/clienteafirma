/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.awt.Component;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.security.PrivilegedExceptionAction;

import es.gob.afirma.applet.io.FileBean;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.ui.AOUIFactory;

/**
 * Acci&oacute;n para la recuperaci&oacute;n del nombre y contenido de uno o m&aacute;s ficheros.
 * @author Carlos Gamuci Mill&aacute;n
 */
final class GetFileAction implements PrivilegedExceptionAction<FileBean[]> {

	private File[] files;

    private String title;
    private String[] exts;
    private String desc;
    private boolean multiSel;
    private Component parent;

    /**
     * Crea la acci&oacute;n para la recuperaci&oacute;n del nombre y el contenido de un fichero
     * seleccionado por el usuario a trav&eacute;s de un di&aacute;logo modal.
     * @param title T&iacute;tulo del di&aacute;logo.
     * @param exts Extensiones de fichero aceptadas por defecto.
     * @param desc Descripci&oacute;n del tipo de fichero aceptado por defecto.
     * @param multiSel Habilita la selecci&oacute;n m&uacute;ltiple de ficheros.
     * @param parent Componente padre sobre el que se mostrar&aacute; el di&aacute;logo.
     */
    GetFileAction(final String title, final String[] exts, final String desc, final boolean multiSel, final Component parent) {
    	this.title = title;
        this.exts = exts != null ? exts.clone() : null;
        this.desc = desc;
        this.multiSel = multiSel;
        this.parent = parent;
    }

    /**
     * Crea la acci&oacute;n para la recuperaci&oacute;n del nombre y el contenido del fichero indicado.
     * @param path Ruta de fichero.
     */
    GetFileAction(final String path) {
    	this.files = new File[] { new File(path) };
    }

    /**
     * Muestra un di&aacute;logo modal para la carga de un fichero y recuperar el nombre
     * del mismo y su contenido.
     * @return Nombre del fichero, el car&aacute;cter "|" y el contenido del fichero (Texto o
     * BinarioB64 seg&uacute;n el indicador {@code asBase64}).
     * @throws es.gob.afirma.core.AOCancelledOperationException Cuando se cancela la operaci&oacute;n de selecci&oacute;n.
     * @throws IOException Cuando se produce un error al leer el fichero.
     */
	@Override
	public FileBean[] run() throws IOException {

		if (this.files == null) {
			this.files = AOUIFactory.getLoadFiles(this.title, null, null, this.exts, this.desc, false, this.multiSel, null, this.parent);
		}

		byte[] contentFic;
		FileInputStream is;
		final FileBean[] fileContents = new FileBean[this.files.length];
		for (int i = 0; i < this.files.length; i++) {
			is = new FileInputStream(this.files[i]);
			contentFic = AOUtil.getDataFromInputStream(is);
			is.close();
			fileContents[i] = new FileBean(this.files[i], contentFic);
		}

        return fileContents;
	}
}