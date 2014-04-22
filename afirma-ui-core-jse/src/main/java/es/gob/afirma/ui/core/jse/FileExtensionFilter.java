/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.ui.core.jse;

import java.io.File;

import javax.swing.filechooser.FileFilter;

/** Filtro de ficheros basado en extensiones de fichero para di&aacute;logos de selecci&oacute;n.
 * @author Carlos Gamuci */
class FileExtensionFilter extends FileFilter {

	private final String[] exts;

	private final String desc;

	/** Construye el filtro para un listado de extensiones y con una descripci&oacute;n (opcional)
	 * asignada.
	 * @param extensions Extensiones que se desean visualizar en el panel de selecci&oacute;n.
	 * @param description Descripci&oacute;n asignada al tipo de fichero que se busca. */
	FileExtensionFilter(final String[] extensions, final String description) {
		this.exts = extensions.clone();
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
