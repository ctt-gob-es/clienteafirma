package es.gob.afirma.core.ui;

/** Filtro de ficheros por defecto */
public final class DefaultFileFilter{

	private final String[] exts;
	private final String description;

	/** Construye un filtro por defecto de fichero.
	 * @param fileExtensions Posibles extensiones del fichero.
	 * @param fileDescription Descripci&oacute;n del fichero. */
	public DefaultFileFilter(final String[] fileExtensions, final String fileDescription) {
		this.exts = fileExtensions != null ? fileExtensions.clone() : null;
		this.description = fileDescription;
	}

	/** Obtiene las posibles extensiones del fichero.
	 * @return Posibles extensiones del fichero. */
	public String[] getExtensions() {
		return this.exts != null ? this.exts.clone() : null;
	}

	/** Obtiene la descripci&oacute;n del fichero.
	 * @return Descripci&oacute;n del fichero. */
	public String getDescription() {
		return this.description;
	}

}
