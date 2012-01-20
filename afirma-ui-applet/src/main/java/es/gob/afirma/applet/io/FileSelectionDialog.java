/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet.io;

import java.awt.Component;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;

import javax.swing.JFileChooser;

import es.gob.afirma.core.AOCancelledOperationException;

/**
 * Di&aacute;logo para la selecci&oacute;n de un fichero y la devoluci&oacute;n de
 * un flujo de datos con su contenido.
 */
public final class FileSelectionDialog {

    private final String title;

    private final String[] exts;

    private final String desc;

    private final boolean multiSel;

    private final Component parent;

    /**
     * Muestra un di&aacute;logo modal para la selecci&oacute;n de ficheros.
     * @param title T&iacute;tulo del di&aacute;logo de selecci&oacute;n.
     * @param exts Extensiones de fichero aceptadas.
     * @param description Descripci&oacute;n del tipo de fichero aceptado por defecto.
     * @param multiSelection Habilita la selecci&oacute;n de m&uacute;ltiples ficheros.
     * @param parent Componente padre sobre el que se mostrar&aacute; el di&aacute;logo.
     */
    public FileSelectionDialog(final String title,
                               final String[] exts,
                               final String description,
                               final boolean multiSelection,
                               final Component parent) {
        this.title = title;
        this.exts = (exts != null ? exts.clone() : null);
        this.desc = description;
        this.multiSel = multiSelection;
        this.parent = parent;
    }

    /**
     * Devuelve el flujo de datos del contenido de los ficheros seleccionados en
     * caso de permitir la selecci&oacute;n m&uacute;ltiple de ficheros.
     * @return Flujos de datos de los ficheros seleccionados.
     * @throws AOCancelledOperationException Si el usuario cancela la operaci&oacute;n.
     * @throws IOException Si se produce alg&uacute;n error en la carga del fichero.
     */
    InputStream[] getFileContents() throws IOException {
    	final ArrayList<InputStream> contentsIs = new ArrayList<InputStream>();
    	for (final File file : this.selectFile().getSelectedFiles()) {
    		contentsIs.add(new FileInputStream(file));
    	}
    	return contentsIs.toArray(new InputStream[0]);
    }

    /**
     * Devuelve la ruta absoluta del fichero seleccionado en
     * caso de permitir la selecci&oacute;n m&uacute;ltiple de ficheros.
     * @return Rutas de los ficheros seleccionados.
     * @throws AOCancelledOperationException Si el usuario cancela la operaci&oacute;n.
     */
    public String[] getPaths() {
    	final ArrayList<String> paths = new ArrayList<String>();
    	for (final File file : this.selectFile().getSelectedFiles()) {
    		paths.add(file.getAbsolutePath());
    	}
    	return paths.toArray(new String[0]);
    }

    /**
     * Muestra un di&aacute;logo modal que permite la selecci&oacute;n de un fichero.
     * @return Di&aacute;logo con el fichero seleccionado.
     * @throws AOCancelledOperationException Si el usuario cancela la operaci&oacute;n.
     */
    private JFileChooser selectFile() {
    	final JFileChooser fc = new JFileChooser();
    	fc.setMultiSelectionEnabled(this.multiSel);
    	if (this.title != null) {
    		fc.setDialogTitle(this.title);
    	}
    	if (this.exts != null) {
    		fc.setFileFilter(new FileExtensionFilter(this.exts, this.desc));
    	}

    	final int result = fc.showOpenDialog(this.parent);
    	if (result != JFileChooser.APPROVE_OPTION) {
    		throw new AOCancelledOperationException("El usuario cancelo la seleccion del fichero"); //$NON-NLS-1$
    	}
    	return fc;
    }
}
