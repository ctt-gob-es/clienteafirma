package es.gob.afirma.signers.xades;

import es.gob.afirma.core.ErrorCode;

public class XAdESErrorCode {

	public static class Request {
		public static ErrorCode REFERENCE_HASH_NOT_FOUND 		= new ErrorCode("610201", "No se ha indicado la huella digital de una referencia a los datos firmados"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode INVALID_REFERENCE_HASH 			= new ErrorCode("610202", "Error al decodificar el hash de los datos referenciados"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode MANIFEST_REFERENCES_NOT_FOUND	= new ErrorCode("610203", "No se han proporcionado referencias para el manifest de firma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode INVALID_DATA_REFERENCE_URI		= new ErrorCode("610204", "La URI a los datos firmados no es valida"); //$NON-NLS-1$ //$NON-NLS-2$

	}
}
