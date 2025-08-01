package es.gob.afirma.triphase.signer;

import es.gob.afirma.core.ErrorCode;

public class TriphaseErrorCode {

	public static class Request {

		public static ErrorCode MALFORMED_PRESIGN						= new ErrorCode("600114", "La prefirma recibida esta mal formada"); //$NON-NLS-1$ //$NON-NLS-2$

	}

}
