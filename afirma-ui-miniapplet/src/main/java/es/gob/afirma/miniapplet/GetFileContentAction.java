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
import java.io.InputStream;
import java.security.PrivilegedExceptionAction;

import es.gob.afirma.core.misc.AOUtil;

/**
 * Acci&oacute;n para la recuperaci&oacute;n del contenido de un fichero seleccionado
 * por el usuario.
 * @author Carlos Gamuci Mill&aacute;n
 */
final class GetFileContentAction implements PrivilegedExceptionAction<byte[]>{

    private final String title;
    private final String[] exts;
    private final String desc;
    private final Component parent;
    
    /**
     * Crea la acci&oacute;n para la carga de ficheros.
     * @param title T&iacute;tulo del di&aacute;logo.
     * @param exts Extensiones de fichero aceptadas por defecto.
     * @param description Descripci&opacute;n del tipo de fichero aceptado por defecto.
     * @param parent Componente padre sobre el que se mostrar&aacute; el di&aacute;logo.
     */
    GetFileContentAction(final String title, final String[] exts, final String description,
    		 final Component parent) {
        this.title = title;
        this.exts = (exts != null ? exts.clone() : null);
        this.desc = description;
        this.parent = parent;
    }
    
    /**
     * Muestra al usuario un di&aacute;logo modal para la selecci&oacute;n de un fichero y devuelve
     * su contenido.
     * @return El contenido del fichero.
     * @throws AOCancelledOperationException Cuando se cancela la operacion de selecci&oacute;n.
     * @throws IOException Cuando se produce un error al leer el fichero.
     */
	public byte[] run() throws IOException {
        final FileSelectionDialog dialog = new FileSelectionDialog(this.title, this.exts, this.desc, false, this.parent);
        final InputStream is = dialog.getFileContent();
        try {
        	return AOUtil.getDataFromInputStream(is);
        } 
        finally {
        	if (is != null) {
        		try {
        			is.close();
        		} 
        		catch (final Exception e) {
        			/* Ignoramos este error */
        		}
        	}
        }
	}
}
