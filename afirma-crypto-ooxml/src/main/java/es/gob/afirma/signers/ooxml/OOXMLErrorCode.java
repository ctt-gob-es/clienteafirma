package es.gob.afirma.signers.ooxml;

import es.gob.afirma.core.ErrorCode;

public class OOXMLErrorCode {

	public static class Internal {

		public static final ErrorCode UNKWNON_OOXML_SIGNING_ERROR		= new ErrorCode("213005", "Error desconocido en la generacion de la firma OOXML"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INTERNAL_OOXML_SIGNING_ERROR	= new ErrorCode("213006", "Error interno en la generacion de la firma OOXML"); //$NON-NLS-1$ //$NON-NLS-2$

	}

	public static class Functional {
		public static final ErrorCode INCOMPATIBLE_JRE			= new ErrorCode("500004", "JRE no compatible con la operacion"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode OOXML_DOCUMENT_NEEDED 		= new ErrorCode("510004", "La firma requiere que los datos sean OOXML"); //$NON-NLS-1$ //$NON-NLS-2$

	}
}
