package es.gob.afirma.core.ui;

import java.util.Arrays;
import java.util.Objects;

/** Filtro de ficheros gen&eacute;rico.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class GenericFileFilter {

	private final String[] exts;
	private final String description;

	/** Construye un filtro gen&eacute;rico de fichero.
	 * @param fileExtensions Posibles extensiones del fichero.
	 * @param fileDescription Descripci&oacute;n del fichero. */
	public GenericFileFilter(final String[] fileExtensions, final String fileDescription) {
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

	@Override
	public boolean equals(final Object obj) {
		if (obj instanceof GenericFileFilter) {
			final String[] objExts = ((GenericFileFilter) obj).getExtensions();
			final String objDesc = ((GenericFileFilter) obj).getDescription();
			return Arrays.equals(this.exts, objExts)
					&& Objects.equals(objDesc, this.description);
		}
		return super.equals(obj);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}
