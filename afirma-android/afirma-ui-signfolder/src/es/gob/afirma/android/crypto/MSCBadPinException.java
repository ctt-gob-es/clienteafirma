package es.gob.afirma.android.crypto;

/**
 * Excepci&oacute;n que identifica un error en la inserci&oacute;n del PIN de
 * una tarjeta cript&oacute;grafica interna.
 * 
 * @author Carlos Gamuci
 */
public class MSCBadPinException extends Exception {

	/** Serial ID. */
	private static final long serialVersionUID = -6530167992401963061L;

	/**
	 * Crea una excepci&oacute;n asociada a la inserci&oacute;n de un PIN
	 * incorrecto para el uso de una tarjeta criptogr&aacute;fica interna.
	 * 
	 * @param msg
	 *            Mensaje.
	 * @param t
	 *            Excepci&oacute;n que caus&oacute; a esta.
	 */
	public MSCBadPinException(final String msg, final Throwable t) {
		super(msg, t);
	}

}
