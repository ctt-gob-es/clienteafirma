package es.gob.afirma.massive;

import es.gob.afirma.core.ErrorCode;

/**
 * C&oacute;digos de error expresos de las operaciones de firma masiva.
 */
public class MassiveErrorCode {

	/**
	 * Errores funcionales.
	 */
	public static final class Functional {
		public static ErrorCode INDIR_NOT_FOUND						= new ErrorCode("506001", "El directorio de entrada no existe"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode ACCESS_TO_DIR_IS_NOT_ALLOWED		= new ErrorCode("506002", "No se tiene acceso al directorio indicado"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/**
	 * Errores en la petici&oacute;n.
	 */
	public static final class Request {
		public static ErrorCode DECODING_HASH_ERROR					= new ErrorCode("600601", "Se ha proporcionado un hash no valido para la firma explicita"); //$NON-NLS-1$ //$NON-NLS-2$

	}
}
