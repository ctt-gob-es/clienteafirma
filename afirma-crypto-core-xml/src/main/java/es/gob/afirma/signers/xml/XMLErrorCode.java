package es.gob.afirma.signers.xml;

import es.gob.afirma.core.ErrorCode;

public class XMLErrorCode {

	public static class Internal {

		public static final ErrorCode UNKWNON_XML_SIGNING_ERROR				= new ErrorCode("213001", "Error desconocido en la generacion de la firma XML"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INTERNAL_XML_SIGNING_ERROR				= new ErrorCode("213002", "Error interno en la generacion de la firma XML"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode UNSUPPORTED_TRANSFORMATION_ALGORITHM	= new ErrorCode("213004", "El tipo de transformacion declarado no esta soportado"); //$NON-NLS-1$ //$NON-NLS-2$

	}

	public static class Communication {
		public static final ErrorCode DERREFERENCING_DATA_ERROR	= new ErrorCode("411001", "No se han podido cargar de una URI los datos para la firma XML"); //$NON-NLS-1$ //$NON-NLS-2$

	}

	public static class Functional {
		public static final ErrorCode XML_DOCUMENT_NEEDED 		= new ErrorCode("510002", "La firma requiere que los datos sean XML"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SIGNING_SIGNATURE 	= new ErrorCode("510007", "No se permite firmar una firma XML en el modo de firma trifasica"); //$NON-NLS-1$ //$NON-NLS-2$

	}

	public static class Request {
		public static final ErrorCode INVALID_ENCODING_URI 					= new ErrorCode("610001", "La URI del algoritmo de codificacion no es valida"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_REFERENCES_HASH_ALGORITHM_URI	= new ErrorCode("610002", "La URI del algoritmo de huella de las referencias no es valida"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_CANONICALIZATION_URI			= new ErrorCode("610003", "La URI del algoritmo de canonicalizacion no es valida"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_PRECALCULATED_DATA_HASH_ALGORITHM	= new ErrorCode("610004", "El algoritmo de huella de los datos precalculados no es valido"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_TRANSFORMATION					= new ErrorCode("610005", "La transformacion no es valida"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_REFERENCE_URI					= new ErrorCode("610006", "La URI a los datos firmados no es valida"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_NODE_SELECTOR_XPATH				= new ErrorCode("610007", "La ruta XPATH para la seleccion de nodo no es valida"); //$NON-NLS-1$ //$NON-NLS-2$
	}
}
