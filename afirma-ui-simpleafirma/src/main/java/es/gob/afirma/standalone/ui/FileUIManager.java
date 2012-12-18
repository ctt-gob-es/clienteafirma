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

import java.awt.Frame;
import java.io.File;
import java.io.IOException;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.ui.AOUIFactory;

/**
 * Utilidades para el tratamiento de ficheros (apertura, guardado, etc.).
 */
public final class FileUIManager {

	private FileUIManager() {
		// No permitimos la instanciacion
	}

    /** Muestra un di&aacute;logo para el guardado de datos y los almacena en el
     * fichero seleccionado.
     * @param data Datos que se desean guardar.
     * @param exts Posibles extensiones que asignar al fichero.
     * @param currentDir Directorio actual. */
    static void saveFile(final Frame parent,
    		             final byte[] data,
    		             final File currentDir,
    		             final String[] exts,
    		             final String description,
    		             final String title) throws IOException {
    	try {
	    	AOUIFactory.getSaveDataToFile(
				data,
				null,
				title,
				null,
				exts,
				description,
				parent
			);
    	}
    	catch(final AOCancelledOperationException e) {
    		// Operacion cancelada por el usuario, no hacemos nada
    	}
    }

    /** Muestra un di&aacute;logo para la apertura de un fichero.
     * @param parent Componente padre, para la modalidad
     * @param exts Posibles extensiones que asignar al fichero.
     * @param actualDir Directorio actual.
     * @param title T&iacute;tulo del di&aacute;logo de apertura
     * @return Fichero seleccionado desde el di&aacute;logo o null si no se selecciona ninguno */
    public static File openFile(final Frame parent, final File actualDir, final String[] exts, final String title) {
    	try {
	    	return new File(AOUIFactory.getLoadFileName(
				title,
				actualDir != null ? actualDir.getAbsolutePath() : null,
				exts,
				null,
				false,
				parent
			)[0]);
    	}
    	catch(final AOCancelledOperationException e) {
    		return null;
    	}
    }

}
