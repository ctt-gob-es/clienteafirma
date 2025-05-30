package es.gob.afirma.signers.pkcs7;

import es.gob.afirma.core.ErrorCode;

public class BinaryErrorCode {

	public static class Internal {

		public static final ErrorCode UNKWNON_BINARY_SIGNING_ERROR			= new ErrorCode("211001", "Error desconocido en la generacion de la firma binaria"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INTERNAL_BINARY_SIGNING_ERROR			= new ErrorCode("211002", "Error interno en la generacion de la firma binaria"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode GENERATING_TIMESTAMP_ERROR 				= new ErrorCode("211003", "Error generando sello de tiempo binario"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	public static class ThirdParty {
		public static final ErrorCode BOUNCYCASTLE_VERSION_NOT_SUPPORTED		= new ErrorCode("300001", "Se ha encontrado una version de Spongycastle no soportada"); //$NON-NLS-1$ //$NON-NLS-2$

	}

	public static class Functional {
		public static final ErrorCode SIGNATURE_DOESNT_CONTAIN_DATA 			= new ErrorCode("510001", "La firma no contiene los datos firmados necesarios para la operacion"); //$NON-NLS-1$ //$NON-NLS-2$

	}
}
