package es.gob.afirma.signers.odf;

import es.gob.afirma.core.ErrorCode;

public class ODFErrorCode {

	public static class Internal {

		public static final ErrorCode UNKWNON_ODF_SIGNING_ERROR	= new ErrorCode("213003", "Error desconocido en la generacion de la firma ODF"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INTERNAL_ODF_SIGNING_ERROR	= new ErrorCode("213004", "Error interno en la generacion de la firma ODF"); //$NON-NLS-1$ //$NON-NLS-2$

	}

	public static class Functional {
		public static final ErrorCode ODF_DOCUMENT_NEEDED 		= new ErrorCode("510003", "La firma requiere que los datos sean un documento ODF"); //$NON-NLS-1$ //$NON-NLS-2$

	}
}
