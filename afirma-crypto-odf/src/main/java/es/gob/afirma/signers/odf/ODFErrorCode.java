package es.gob.afirma.signers.odf;

import es.gob.afirma.core.ErrorCode;

public class ODFErrorCode {

	public static class Internal {

		public static ErrorCode UNKWNON_ODF_SIGNING_ERROR	= new ErrorCode("213003", "Error desconocido en la generación de la firma ODF"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode INTERNAL_ODF_SIGNING_ERROR	= new ErrorCode("213004", "Error interno en la generación de la firma ODF"); //$NON-NLS-1$ //$NON-NLS-2$

	}

	public static class Functional {
		public static ErrorCode ODF_DOCUMENT_NEEDED 		= new ErrorCode("510003", "La firma requiere que los datos sean ODF"); //$NON-NLS-1$ //$NON-NLS-2$

	}
}
