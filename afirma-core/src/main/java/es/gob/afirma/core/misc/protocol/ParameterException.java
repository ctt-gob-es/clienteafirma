package es.gob.afirma.core.misc.protocol;

/** Error en los par&aacute;metros de la URL recibida por la aplicaci&oacute;n. */
public class ParameterException extends Exception {

	private static final long serialVersionUID = 976364958815642808L;

	ParameterException(final String msg) {
		super(msg);
	}

	ParameterException(final String msg, final Throwable t) {
		super(msg, t);
	}
}