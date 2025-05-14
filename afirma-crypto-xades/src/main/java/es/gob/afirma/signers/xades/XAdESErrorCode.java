package es.gob.afirma.signers.xades;

import es.gob.afirma.core.ErrorCode;

public class XAdESErrorCode {

	public static class Request {
		public static ErrorCode INVALID_ENCODING_URI 		= new ErrorCode("610001", "La URI de codificación proporcionada no tiene un formato válido"); //$NON-NLS-1$ //$NON-NLS-2$

	}
}
