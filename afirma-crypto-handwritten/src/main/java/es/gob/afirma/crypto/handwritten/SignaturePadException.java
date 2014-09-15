
package es.gob.afirma.crypto.handwritten;


/** Excepci&oacute;n gen&eacute;rica de un dispositivo de captura de firmas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class SignaturePadException extends Exception {

	/** Crea una excepci&oacute;n gen&eacute;rica de un dispositivo de captura de firmas.
	 * @param t Excepci&oacute;n de origen. */
	public SignaturePadException(final Throwable t) {
		super(t);
	}

	protected SignaturePadException() {
		super();
	}

	/** Crea una excepci&oacute;n gen&eacute;rica de un dispositivo de captura de firmas.
	 * @param msg Mensaje de la excepci&oacute;n.
	 * @param e Excepci&oacute;n de origen. */
	public SignaturePadException(final String msg, final Throwable e) {
		super(msg, e);
	}

	/** Crea una excepci&oacute;n gen&eacute;rica de un dispositivo de captura de firmas.
	 * @param msg Mensaje de la excepci&oacute;n. */
	public SignaturePadException(final String msg) {
		super(msg);
	}

	private static final long serialVersionUID = 7851491889542556618L;

}
