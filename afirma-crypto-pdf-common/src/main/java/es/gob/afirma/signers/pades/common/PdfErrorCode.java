package es.gob.afirma.signers.pades.common;

import es.gob.afirma.core.ErrorCode;

public class PdfErrorCode {

	public static class Internal {
		public static ErrorCode UNKWNON_PADES_SIGNING_ERROR				= new ErrorCode("212001", "Error desconocido en la generacion de la firma PAdES"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode INTERNAL_PADES_SIGNING_ERROR			= new ErrorCode("212002", "Error interno en la generacion de la firma PAdES"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode SIGNING_PDF_WITH_UNREGISTER_SIGNATURES	= new ErrorCode("212003", "Se esta intentando firmar una firma PDF con firmas sin registrar"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode SIGNING_PDF_WITH_BAD_PASSWORD			= new ErrorCode("212004", "La contrasena del PDF no era correcta"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode SIGNING_PDF_WITHOUT_PASSWORD			= new ErrorCode("212005", "El PDF necesita contrasena"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode SIGNING_PDF_WITH_MODIFIED_FORM			= new ErrorCode("212006", "Se esta intentando firmar un PDF con un formulario modificado despues de una firma anterior"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode SIGNING_PDF_WITH_CERTIFIED_SIGN			= new ErrorCode("212007", "Se esta intentando firmar un PDF certificado que no admite nuevas firmas"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode SIGNING_PDF_WITH_SUSPICION_OF_PSA		= new ErrorCode("212008", "Se esta intentando firmar un PDF sospechoso de haber sido modificado tras la firma"); //$NON-NLS-1$ //$NON-NLS-2$


	}

	public static class ThirdParty {
		public static ErrorCode ITEXT_VERSION_NOT_SUPPORTED = new ErrorCode("300002", "Se ha encontrado una version de iText no soportada"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	public static class Functional {
		public static ErrorCode PDF_DOCUMENT_NEEDED 		= new ErrorCode("510005", "La firma requiere que los datos sean PDF"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	public static class Request {
		public static ErrorCode INVALID_SIGNATURE_POSITION	= new ErrorCode("610301", "La posicion para la firma visible en la pagina no es valida"); //$NON-NLS-1$ //$NON-NLS-2$
		public static ErrorCode INVALID_SIGNATURE_PAGE		= new ErrorCode("610302", "La pagina para la firma visible no es valida"); //$NON-NLS-1$ //$NON-NLS-2$
	}

}
