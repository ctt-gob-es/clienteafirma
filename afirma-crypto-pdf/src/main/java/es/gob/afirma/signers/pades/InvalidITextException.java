package es.gob.afirma.signers.pades;

import es.gob.afirma.core.InvalidLibraryException;

/** Indica que hay un iText inv&aacute;lido en el CLASSPATH o en el BOOTCLASSPATH, a menudo
 * porque se ha instalado el JAR inapropiadamente como extensi&oacute;n del JRE.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class InvalidITextException extends InvalidLibraryException {

	private static final long serialVersionUID = -322997692480101275L;

	private final String exp;
	private final String fnd;

	/** Crea una instancia de la excepci&oacute;n.
	 * @param expected Versi&oacute;n esperada de iText
	 * @param found Versi&oacute;n encontrada (actual) de iText */
	public InvalidITextException(final String expected, final String found) {
		super("Se necesitaba iText version " + expected + ", pero se encontro la version " + found); //$NON-NLS-1$ //$NON-NLS-2$
		this.exp = expected;
		this.fnd = found;
	}

	/** Obtiene la versi&oacute;n esperada de iText.
	 * @return Versi&oacute;n esperada de iText */
	public String getExpectedVersion() {
		return this.exp;
	}

	/** Obtiene la versi&oacute;n encontrada (actual) de iText.
	 * @return Versi&oacute;n encontrada (actual) de iText */
	public String getFoundVersion() {
		return this.fnd;
	}

}
