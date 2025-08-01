package es.gob.afirma.signvalidation;

import es.gob.afirma.core.ErrorCode;

public class ValidationErrorCode {

	static class Functional {
		public static final ErrorCode INCOMPATIBLE_SIGNATURE			= new ErrorCode("507001", "El documento no esta firmado o la firma no es compatible"); //$NON-NLS-1$ //$NON-NLS-2$
	}

}
