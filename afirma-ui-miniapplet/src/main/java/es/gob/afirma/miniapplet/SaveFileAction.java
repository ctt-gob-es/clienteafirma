/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.miniapplet;

import java.awt.Component;
import java.io.File;
import java.io.IOException;
import java.security.PrivilegedExceptionAction;
import java.util.Collections;

import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.ui.GenericFileFilter;

/**
 * Acci&oacute;n para almacenar un fichero en disco.
 * @author Carlos Gamuci Mill&aacute;n
 */
final class SaveFileAction implements PrivilegedExceptionAction<File> {

	private final String title;
    private final byte[] data;
    private final String[] exts;
    private final String desc;
    private final String filename;
    private final Component parent;

    /**
     * Crea la acci&oacute;n para la carga de ficheros.
     * @param title T&iacute;tulo del di&aacute;logo de guardado.
     * @param data Datos que se desean guardar.
     * @param exts Extensiones permitidas para los datos.
     * @param description Descripci&oacute;n del tipo de fichero.
     * @param filename Fichero de salida propuesto.
     * @param parent Componente padre sobre el que se mostrar&aacute; el di&aacute;logo.
     */
    SaveFileAction(final String title, final byte[] data, final String[] exts,
    		final String description, final String filename, final Component parent) {
        this.title = title;
        this.data = data != null ? data.clone() : null;
        this.exts = exts != null ? exts.clone() : null;
        this.desc = description;
        this.filename = filename;
        this.parent = parent;
    }

    /**
     * Muestra un di&aacute;logo modal para el guardado de un fichero y lo salva en el directorio
     * y con el nombre indicado por el usuario.
     * @return {@code true} si el fichero se almacen&oacute; correctamente.
     * @throws es.gob.afirma.core.AOCancelledOperationException Cuando se cancela la operaci&oacute;n.
     * @throws IOException Cuando se produce un error al almacenar el fichero.
     */
	@Override
	public File run() throws IOException {
    	return AOUIFactory.getSaveDataToFile(
			this.data,
			this.title,
			null,
			this.filename,
			Collections.singletonList(new GenericFileFilter(this.exts, this.desc)),
			this.parent
		);
	}
}
