package es.gob.afirma.signers.xml;

import es.gob.afirma.core.ErrorCode;

public class XMLErrorCode {

	public static class Internal {

		public static ErrorCode UNKWNON_XML_SIGNING_ERROR	= new ErrorCode("213001", "Error desconocido en la generación de la firma XML"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode INTERNAL_XML_SIGNING_ERROR	= new ErrorCode("213002", "Error interno en la generación de la firma XML"); //$NON-NLS-1$ //$NON-NLS-2$

	}

	public static class Functional {
		public static ErrorCode XML_DOCUMENT_NEEDED 		= new ErrorCode("510002", "La firma requiere que los datos sean XML"); //$NON-NLS-1$ //$NON-NLS-2$

	}
}
