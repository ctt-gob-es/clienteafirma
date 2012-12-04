/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.miniapplet;

import java.awt.Component;
import java.io.File;
import java.io.IOException;
import java.security.PrivilegedExceptionAction;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.ui.AOUIFactory;

/**
 * Acci&oacute;n para almacenar un fichero en disco.
 * @author Carlos Gamuci Mill&aacute;n
 */
final class SaveFileAction implements PrivilegedExceptionAction<Void> {

	private final String title;
    private final byte[] data;
    private final String[] exts;
    private final String desc;
    private final File fileHint;
    private final Component parent;

    /**
     * Crea la acci&oacute;n para la carga de ficheros.
     * @param title T&iacute;tulo del di&aacute;logo de guardado.
     * @param data Datos que se desean guardar.
     * @param exts Extensiones permitidas para los datos.
     * @param description Descripci&oacute;n del tipo de fichero.
     * @param fileHint Fichero de salida propuesto.
     * @param parent Componente padre sobre el que se mostrar&aacute; el di&aacute;logo.
     */
    SaveFileAction(final String title, final byte[] data, final String[] exts,
    		final String description, final File fileHint, final Component parent) {
        this.title = title;
        this.data = data != null ? data.clone() : null;
        this.exts = exts != null ? exts.clone() : null;
        this.desc = description;
        this.fileHint = fileHint;
        this.parent = parent;
    }

    /**
     * Muestra un di&aacute;logo modal para el guardado de un fichero y lo salva en el directorio
     * y con el nombre indicado por el usuario.
     * @return {@code true} si el fichero se almacen&oacute; correctamente.
     * @throws AOCancelledOperationException Cuando se cancela la operaci&oacute;n.
     * @throws IOException Cuando se produce un error al almacenar el fichero.
     */
	@Override
	public Void run() throws IOException {
    	selectFileToSave();
    	return null;
	}

    /** Pregunta al usuario por un nombre de fichero para salvar datos en disco.
     * @return Nombre de fichero (con ruta) seleccionado por el usuario
     * @throws IOException Cuando se produzca un error durante la selecci&oacute;n del fichero.
     * @throws AOCancelledOperationException Cuando el usuario cancele la operaci&oacute;n.
     */
    private File selectFileToSave() throws IOException {
    	return AOUIFactory.getSaveDataToFile(
			this.data,
			this.title,
			this.fileHint,
			this.exts != null ? new FileExtensionFilter(this.exts, this.desc) : null,
			this.parent
		);
    }

}
