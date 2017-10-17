/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.io.File;

import javax.swing.filechooser.FileFilter;

/** Filtra los ficheros por extensi&oacute;n para los di&aacute;logos de
 * carga y guardado. Se declara como p&uacute;blico para que pueda ser usado
 * tambi&eacute;n por el interfaz de aplicaci&oacute;n de escritorio. No
 * usamos <code>FileNameExtensionFilter</code> directamente para
 * compatibilizar con Java 1.4
 * @version 0.3 */
final class ExtFilter extends FileFilter implements java.io.FileFilter {

    private final String[] extensions;
    private final String description;

    /** Construye un filtro para la selecci&oacute;n de ficheros en un di&aacute;logo de selecci6oacute;n de ficheros.
     * @param exts Extensiones de fichero permitidas
     * @param desc Descripci&oacute;n del tipo de fichero correspondiente a las extensiones */
    ExtFilter(final String[] exts, final String desc) {
        if (exts == null || exts.length < 1) {
            throw new IllegalArgumentException("No se puede crear un filtro vacio"); //$NON-NLS-1$
        }
        this.extensions = exts.clone();
        this.description = desc != null ? desc : AppletMessages.getString("ExtFilter.1"); //$NON-NLS-1$
    }

    /** {@inheritDoc} */
    @Override
    public boolean accept(final File f) {
        if (f.isDirectory()) {
            return true;
        }
        // getExtension() pasa la extension a minusculas, no hace falta
        // el "ignoreCase"
        final String extension = getExtension(f);
        for (final String extension2 : this.extensions) {
            if (extension2.equalsIgnoreCase(extension)) {
                return true;
            }
        }
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public String getDescription() {
        return this.description;
    }

    /** Devuelve la extensi&oacute;n de un fichero.
     * @param f
     *        Fichero del cual queremos conocer la extensi&oacute;n
     * @return Extensi&oacute;n del fichero o cadena vac&iacute;a si este no
     *         tiene extensi&oacute;n */
    private static String getExtension(final File f) {
        final String s = f.getName();
        final int i = s.lastIndexOf('.');
        if (i > 0 && i < s.length() - 1) {
            return s.substring(i + 1).toLowerCase();
        }
        return ""; //$NON-NLS-1$
    }

}
