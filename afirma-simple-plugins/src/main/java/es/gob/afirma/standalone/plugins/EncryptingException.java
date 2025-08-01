package es.gob.afirma.standalone.plugins;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ErrorCode;

public class EncryptingException extends AOException {

	private static final long serialVersionUID = -2188855932237451920L;

	public EncryptingException(final String msg, final Throwable cause, final ErrorCode errorCode) {
		super(msg, cause, errorCode);
	}
}
