package es.gob.afirma.signers.xades;

import es.gob.afirma.core.AOInvalidFormatException;

/** Excepci&oacute;n para notificar que la factura electr&oacute;nica ya est&aacute; firmada y por lo tanto no se pueden
 * a&ntilde;adir m&aacute;s firmas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class EFacturaAlreadySignedException extends AOInvalidFormatException {

	private static final long serialVersionUID = -2721087059930025508L;

	EFacturaAlreadySignedException() {
		super("La factura ya tiene una firma electronica y no admite firmas adicionales"); //$NON-NLS-1$
	}

}
