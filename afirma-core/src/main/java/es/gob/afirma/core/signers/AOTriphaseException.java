package es.gob.afirma.core.signers;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ErrorCode;

/**
 * Excepci&oacute;n que se&ntilde;ala un error durante un proceso de firma trif&aacute;fica.
 */
public class AOTriphaseException extends AOException {

	/** Serial Id. */
	private static final long serialVersionUID = -6210469965589338895L;

	private String serverExceptionClassname = null;

	/**
	 * Crea la excepci&oacute;n con el mensaje asociado.
	 * @param msg Mensaje con el motivo del error.
	 */
	private AOTriphaseException(final String msg, final String serverExceptionClassname, final ErrorCode errorCode) {
		super(msg, errorCode);
		this.serverExceptionClassname = serverExceptionClassname;
	}

	/** Obtiene la excepci&oacute;n generada en el servidor cuando este ha sido la causa del problema.
	 * @return Nombre de la clase de la excepci&oacute;n generada en el servidor o <code>null</code>
	 *         si el error no est&aacute; relacionado con problemas en el servidor. */
	public String getServerExceptionClassname() {
		return this.serverExceptionClassname;
	}

	/**
	 * Obtiene la excepci&oacute;n que se&ntilde;ala un error durante un proceso de firma trif&aacute;fica
	 * a partir del mensaje recibido del servidor.
	 * @param msg Mensaje recibido del servidor.
	 * @param presign {@code true} indica que la operaci&oacute;n
	 * @return Excepci&oacute;n que se&ntilde;ala un error durante un proceso de firma trif&aacute;fica
	 *         a partir del mensaje recibido del servidor.
	 */
	public static AOTriphaseException parsePresignException(final String serverErrorCode, final String msg, final String exceptionClassname) {

		final ErrorCode errorCode;
		switch (serverErrorCode) {
		case "ERR-1": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_PRESIGN_ERROR_1;
			break;
		case "ERR-2": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_PRESIGN_ERROR_2;
			break;
		case "ERR-3": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_PRESIGN_ERROR_3;
			break;
		case "ERR-4": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_PRESIGN_ERROR_4;
			break;
		case "ERR-5": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_PRESIGN_ERROR_5;
			break;
		case "ERR-6": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_PRESIGN_ERROR_6;
			break;
		case "ERR-7": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_PRESIGN_ERROR_7;
			break;
		case "ERR-8": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_PRESIGN_ERROR_8;
			break;
		case "ERR-9": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_PRESIGN_ERROR_9;
			break;
		case "ERR-10": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_PRESIGN_ERROR_10;
			break;
		case "ERR-11": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_PRESIGN_ERROR_11;
			break;
		case "ERR-12": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_PRESIGN_ERROR_12;
			break;
		case "ERR-13": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_PRESIGN_ERROR_13;
			break;
		case "ERR-14": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_PRESIGN_ERROR_14;
			break;
		case "ERR-15": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_PRESIGN_ERROR_15;
			break;
		case "ERR-16": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_PRESIGN_ERROR_16;
			break;
		case "ERR-17": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_PRESIGN_ERROR_17;
			break;
		case "ERR-18": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_PRESIGN_ERROR_18;
			break;
		case "ERR-19": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_PRESIGN_ERROR_19;
			break;
		case "ERR-20": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_PRESIGN_ERROR_20;
			break;
		default:
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_UNKNOWN_PRESIGN_ERROR;
			break;
		}

		return new AOTriphaseException(msg, exceptionClassname, errorCode);
	}

	public static AOTriphaseException parsePostsignException(final String serverErrorCode, final String msg, final String exceptionClassname) {
		final ErrorCode errorCode;
		switch (serverErrorCode) {
		case "ERR-1": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_POSTSIGN_ERROR_1;
			break;
		case "ERR-2": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_POSTSIGN_ERROR_2;
			break;
		case "ERR-3": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_POSTSIGN_ERROR_3;
			break;
		case "ERR-4": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_POSTSIGN_ERROR_4;
			break;
		case "ERR-5": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_POSTSIGN_ERROR_5;
			break;
		case "ERR-6": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_POSTSIGN_ERROR_6;
			break;
		case "ERR-7": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_POSTSIGN_ERROR_7;
			break;
		case "ERR-8": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_POSTSIGN_ERROR_8;
			break;
		case "ERR-9": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_POSTSIGN_ERROR_9;
			break;
		case "ERR-10": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_POSTSIGN_ERROR_10;
			break;
		case "ERR-11": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_POSTSIGN_ERROR_11;
			break;
		case "ERR-12": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_POSTSIGN_ERROR_12;
			break;
		case "ERR-13": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_POSTSIGN_ERROR_13;
			break;
		case "ERR-14": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_POSTSIGN_ERROR_14;
			break;
		case "ERR-15": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_POSTSIGN_ERROR_15;
			break;
		case "ERR-16": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_POSTSIGN_ERROR_16;
			break;
		case "ERR-17": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_POSTSIGN_ERROR_17;
			break;
		case "ERR-18": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_POSTSIGN_ERROR_18;
			break;
		case "ERR-19": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_POSTSIGN_ERROR_19;
			break;
		case "ERR-20": //$NON-NLS-1$
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_POSTSIGN_ERROR_20;
			break;
		default:
			errorCode = ErrorCode.ThirdParty.TRI_SERVER_UNKNOWN_POSTSIGN_ERROR;
			break;
		}

		return new AOTriphaseException(msg, exceptionClassname, errorCode);
	}
}
