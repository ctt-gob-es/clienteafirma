package es.gob.afirma.miniapplet;

import java.io.File;

import javax.swing.filechooser.FileFilter;

/**
 * Filtro de ficheros basado en extensiones de fichero para di&aacute;logos de selecci&opacute;n.
 * @author Carlos Gamuci
 */
public class FileExtensionFilter extends FileFilter {

	private String[] exts;
	
	private String desc;
	
	/**
	 * Construye el filtro para un listado de extensiones y con una descripci&oacute;n (opcional)
	 * asignada. 
	 * @param extensions Extensiones que se desean visualizar en el panel de selecci&oacute;n.
	 * @param description Descripci&oacute;n asignada al tipo de fichero que se busca.
	 */
	public FileExtensionFilter(String[] extensions, final String description) {
		this.exts = extensions;
		this.desc = description;
	}
	
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
