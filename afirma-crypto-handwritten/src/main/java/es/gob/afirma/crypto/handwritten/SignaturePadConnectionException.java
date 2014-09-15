package es.gob.afirma.crypto.handwritten;


/** Error de conexi&oacute;n con un dispositivo de captura de firmas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class SignaturePadConnectionException extends SignaturePadException {

	private static final long serialVersionUID = -4128080513002225923L;

	/** Construye una excepci&oacute;n de conexi&oacute;n con un dispositivo de captura de firmas.
	 * @param t Excepci&oacute;n de origen. */
	public SignaturePadConnectionException(final Throwable t) {
		super(t);
	}

	/** Construye una excepci&oacute;n de conexi&oacute;n con un dispositivo de captura de firmas. */
	public SignaturePadConnectionException() {
		super();
	}

	/** Construye una excepci&oacute;n de conexi&oacute;n con un dispositivo de captura de firmas.
	 * @param msg Mensaje de la excepci&oacute;n. */
	public SignaturePadConnectionException(final String msg) {
		super(msg);
	}

	/** Construye una excepci&oacute;n de conexi&oacute;n con un dispositivo de captura de firmas.
	 * @param msg Mensaje de la excepci&oacute;n.
	 * @param t Excepci&oacute;n de origen. */
	public SignaturePadConnectionException(final String msg, final Throwable t) {
		super(msg, t);
	}

}
