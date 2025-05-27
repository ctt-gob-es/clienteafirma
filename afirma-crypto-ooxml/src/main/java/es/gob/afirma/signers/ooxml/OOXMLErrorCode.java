package es.gob.afirma.signers.ooxml;

import es.gob.afirma.core.ErrorCode;

public class OOXMLErrorCode {

	public static class Internal {

		public static ErrorCode UNKWNON_OOXML_SIGNING_ERROR		= new ErrorCode("213005", "Error desconocido en la generacion de la firma OOXML"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode INTERNAL_OOXML_SIGNING_ERROR	= new ErrorCode("213006", "Error interno en la generacion de la firma OOXML"); //$NON-NLS-1$ //$NON-NLS-2$

	}

	public static class Functional {
		public static ErrorCode OOXML_DOCUMENT_NEEDED 		= new ErrorCode("510004", "La firma requiere que los datos sean OOXML"); //$NON-NLS-1$ //$NON-NLS-2$

	}
}
