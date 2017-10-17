/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet.io;

import java.io.File;

import javax.swing.filechooser.FileFilter;

/** Filtro de ficheros basado en extensiones de fichero para di&aacute;logos de selecci&oacute;n.
 * @author Carlos Gamuci */
class FileExtensionFilter extends FileFilter {

	private final String[] exts;

	private final String desc;

	/**
	 * Construye el filtro para un listado de extensiones y con una descripci&oacute;n (opcional)
	 * asignada.
	 * @param extensions Extensiones que se desean visualizar en el panel de selecci&oacute;n.
	 * @param description Descripci&oacute;n asignada al tipo de fichero que se busca.
	 */
	FileExtensionFilter(final String[] extensions, final String description) {
		this.exts = extensions == null ? null : extensions.clone();
		this.desc = description;
	}

	/** {@inheritDoc} */
	@Override
	public String getDescription() {
		if (this.desc != null) {
			return this.desc;
		}

		final StringBuilder buffer = new StringBuilder();
		for (int i = 0; i < this.exts.length; i++) {
			buffer.append(i == 0 ? "*." : ",*.").append(this.exts[i]); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return buffer.toString();
	}

	/** {@inheritDoc} */
	@Override
	public boolean accept(final File f) {
		if (f.isDirectory()) {
			return true;
		}
		for (final String ext : this.exts) {
			if (f.getName().endsWith("." + ext)) { //$NON-NLS-1$
				return true;
			}
		}
		return false;
	}
}
