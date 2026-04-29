package es.gob.afirma.standalone.protocol;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ErrorCode;
import es.gob.afirma.standalone.SimpleErrorCode;

/**
 * Excepci&oacute;n que indica que se recibe un error desde el servidor intermedio, ya sea
 * porque se subio un error o porque fall&oacute; la llamada.
 */
class IntermediateServerErrorSendedException extends AOException {

	/** Serial Id. */
	private static final long serialVersionUID = 2647965667256824572L;

	/**
	 * Construye la excepci&oacute;n con un mensaje.
	 * @param msg Mensaje de error.
	 */
	IntermediateServerErrorSendedException(final String msg) {
		super(msg, SimpleErrorCode.Internal.ERROR_RECIVED_FROM_CLIENT);
	}

	/**
	 * Construye la excepci&oacute;n con un mensaje y una excepci&oacute;n controlada.
	 * Se establecer&aacute; internamente el mismo c&oacute;digo de error que el de la
	 * excepci&oacute;n recibida.
	 * @param msg Mensaje de error.
	 * @param e Excepci&oacute;n controlada.
	 */
	IntermediateServerErrorSendedException(final String msg, final AOException e) {
		super(msg, e.getErrorCode());
	}

	/**
	 * Construye la excepci&oacute;n con un mensaje, una excepci&oacute;n y un
	 * c&oacute;digo de error.
	 * @param msg Mensaje de error.
	 * @param e Excepci&oacute;n controlada.
	 * @param errorCode C&oacute;digo identificativo del error.
	 */
	IntermediateServerErrorSendedException(final String msg, final Throwable e, final ErrorCode errorCode) {
		super(msg, e, errorCode);
	}
}
