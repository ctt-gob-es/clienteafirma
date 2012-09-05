package es.gob.afirma.signers.pades;

import es.gob.afirma.core.AOFormatFileException;

/** Excepci&oacute;n para notificar que se ha proporcionado un fichero que no es un PDF o es un
 * PDF no soportado / inv&aacute;lido / corrupto.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class InvalidPdfException extends AOFormatFileException {

	private static final long serialVersionUID = 674827105543544636L;

	InvalidPdfException(final Exception e) {
		super("El fichero no es un PDF o es un PDF no soportado", e); //$NON-NLS-1$
	}

}
