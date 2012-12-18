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
import java.io.IOException;
import java.security.PrivilegedExceptionAction;

import es.gob.afirma.core.ui.AOUIFactory;

/**
 * Acci&oacute;n para la recuperaci&oacute;n del nombre de un fichero.
 * @author Carlos Gamuci Mill&aacute;n
 */
final class GetFilePathAction implements PrivilegedExceptionAction<String> {

    private final String title;
    private final String[] exts;
    private final String desc;
    private final Component parent;

    /**
     * Crea la acci&oacute;n para la recuperaci&oacute;n de la ruta de un fichero.
     * @param title T&iacute;tulo del di&aacute;logo.
     * @param exts Extensiones de fichero aceptadas por defecto.
     * @param description Descripci&opacute;n del tipo de fichero aceptado por defecto.
     * @param parent Componente padre sobre el que se mostrar&aacute; el di&aacute;logo.
     */
    GetFilePathAction(final String title, final String[] exts, final String description,
    		 final Component parent) {
        this.title = title;
        this.exts = exts != null ? exts.clone() : null;
        this.desc = description;
        this.parent = parent;
    }

    /**
     * Muestra un di&aacute;logo modal para la carga de un fichero y recuperar la ruta absoluta
     * del mismo.
     * @return Ruta absoluta del fichero.
     * @throws es.gob.afirma.core.AOCancelledOperationException Cuando se cancela la operaci&oacute;n de selecci&oacute;n.
     * @throws IOException Cuando se produce un error al leer el fichero.
     */
	@Override
	public String run() throws IOException {
		return AOUIFactory.getLoadFiles(this.title, null, null, this.exts, this.desc, false, false, this.parent)[0].getAbsolutePath();
	}
}
