package es.gob.afirma.signers.ooxml;

/** Versi&oacute;n del JRE no soportada.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class UnsupportedJreVersionException extends RuntimeException {

	private static final long serialVersionUID = -5356570249002228729L;

	UnsupportedJreVersionException() {
		super(
			"Version de Java no soportada. Se necesita Java 7 o superior, y se ha encontrado Java " + System.getProperty("java.version") //$NON-NLS-1$ //$NON-NLS-2$
		);
	}


}
