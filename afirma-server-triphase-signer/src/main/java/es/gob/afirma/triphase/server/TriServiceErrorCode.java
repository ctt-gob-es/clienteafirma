package es.gob.afirma.triphase.server;

import es.gob.afirma.core.ErrorCode;

public class TriServiceErrorCode {

	public static class Request {

		public static final ErrorCode CRYPTO_OPERATION_NOT_FOUND			= new ErrorCode("600115", "No se ha recibido el identificador de operacion criptografica para la operacion de firma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode UNSUPPORTED_CRYPTO_OPERATION			= new ErrorCode("600116", "El identificador de operacion criptografica no es valido"); //$NON-NLS-1$ //$NON-NLS-2$

	}

}
