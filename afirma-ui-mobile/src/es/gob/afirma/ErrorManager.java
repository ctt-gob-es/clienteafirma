package es.gob.afirma;

import java.util.Dictionary;
import java.util.Hashtable;

final class ErrorManager {

	private static final String ERROR_NUMBER = "%#%"; //$NON-NLS-1$
	private static final String ERROR_MESSAGE = "%MSG%"; //$NON-NLS-1$
	private static final String ERROR_TEMPLATE = ERROR_NUMBER + ":=" + ERROR_MESSAGE; //$NON-NLS-1$

	private static final String GENERIC_ERROR = "Error generico"; //$NON-NLS-1$

	static final String ERROR_MISSING_OPERATION_NAME     = "ERR-00"; //$NON-NLS-1$
	static final String ERROR_UNSUPPORTED_OPERATION_NAME = "ERR-01"; //$NON-NLS-1$
	static final String ERROR_MISSING_DATA               = "ERR-02"; //$NON-NLS-1$
	static final String ERROR_BAD_XML                    = "ERR-03"; //$NON-NLS-1$
	static final String ERROR_BAD_CERTIFICATE            = "ERR-04"; //$NON-NLS-1$
	static final String ERROR_MISSING_DATA_ID            = "ERR-05"; //$NON-NLS-1$
	static final String ERROR_INVALID_DATA_ID            = "ERR-06"; //$NON-NLS-1$
	static final String ERROR_INVALID_DATA               = "ERR-07"; //$NON-NLS-1$
	static final String ERROR_MISSING_SERVLET      		 = "ERR-08"; //$NON-NLS-1$
	static final String ERROR_INVALID_SERVLET        	 = "ERR-09"; //$NON-NLS-1$
	static final String ERROR_NOT_SUPPORTED_FORMAT       = "ERR-10"; //$NON-NLS-1$
	static final String ERROR_CANCELLED_OPERATION        = "ERR-11"; //$NON-NLS-1$
	static final String ERROR_CODING_BASE64				 = "ERR-12"; //$NON-NLS-1$
	static final String ERROR_PKE       				 = "ERR-13"; //$NON-NLS-1$
	static final String ERROR_SIGNING       			 = "ERR-14"; //$NON-NLS-1$
	static final String ERROR_INVALID_CIPHER_KEY         = "ERR-15"; //$NON-NLS-1$
	static final String ERROR_CIPHERING			         = "ERR-16"; //$NON-NLS-1$
	static final String ERROR_NO_CERT_SELECTED			 = "ERR-17"; //$NON-NLS-1$
	static final String ERROR_PKE_ANDROID_4_1			 = "ERR-21"; //$NON-NLS-1$


	private static final Dictionary<String, String> ERRORS = new Hashtable<String, String>();
	static {
		ERRORS.put(ERROR_MISSING_OPERATION_NAME, "No se ha indicado codigo de operacion"); //$NON-NLS-1$
		ERRORS.put(ERROR_UNSUPPORTED_OPERATION_NAME, "Codigo de operacion no soportado"); //$NON-NLS-1$
		ERRORS.put(ERROR_MISSING_DATA, "No se han proporcionado los datos de la operacion"); //$NON-NLS-1$
		ERRORS.put(ERROR_BAD_XML, "Se ha recibido un XML mal formado"); //$NON-NLS-1$
		ERRORS.put(ERROR_BAD_CERTIFICATE, "Se ha recibido un certificado corrupto"); //$NON-NLS-1$
		ERRORS.put(ERROR_MISSING_DATA_ID, "No se ha proporcionado un identificador para los datos"); //$NON-NLS-1$
		ERRORS.put(ERROR_INVALID_DATA_ID, "El identificador para los datos es invalido"); //$NON-NLS-1$
		ERRORS.put(ERROR_INVALID_DATA, "Los datos solicitados o enviados son invalidos"); //$NON-NLS-1$
		ERRORS.put(ERROR_MISSING_SERVLET, "No se ha proporcionado el sevlet para la comunicacion de los datos"); //$NON-NLS-1$
		ERRORS.put(ERROR_INVALID_SERVLET, "La ruta del servlet es invalida"); //$NON-NLS-1$
		ERRORS.put(ERROR_NOT_SUPPORTED_FORMAT, "Se ha configurado un formato de firma no soportado"); //$NON-NLS-1$
		ERRORS.put(ERROR_CANCELLED_OPERATION, "Operaci\u00F3n cancelada"); //$NON-NLS-1$
		ERRORS.put(ERROR_CODING_BASE64, "Error en la codificaci\u00F3n del base 64"); //$NON-NLS-1$
		ERRORS.put(ERROR_PKE, "No se pudo recuperar la clave del certificado"); //$NON-NLS-1$
		ERRORS.put(ERROR_SIGNING, "Ocurri\u00F3 un error en la operaci\u00F3n de firma"); //$NON-NLS-1$
		ERRORS.put(ERROR_INVALID_CIPHER_KEY, "La clave de cifrado proporcionada no es valida"); //$NON-NLS-1$
		ERRORS.put(ERROR_CIPHERING, "Error durante el proceso de cifrado de los datos"); //$NON-NLS-1$
		ERRORS.put(ERROR_NO_CERT_SELECTED, "No se seleccion\u00F3 ning\u00FAn certificado de firma"); //$NON-NLS-1$
		ERRORS.put(ERROR_PKE_ANDROID_4_1, "Android 4.1 y 4.1.1 no permiten que los nombres de certificados contengan caracteres especiales (espacios, guiones...). Modifique el alias de sus certificados al importarlos para evitar este error."); //$NON-NLS-1$
	}

	private ErrorManager() {
		// No instanciable
	}

	static String genError(final String number, final String msg) {
		return
			ERROR_TEMPLATE.replace(ERROR_NUMBER, number).replace(
				ERROR_MESSAGE,
				msg != null ? msg : ERRORS.get(number) != null ? ERRORS.get(number) : GENERIC_ERROR
			);
	}

	/** Recupera el mensaje asociado a un c&oacute;digo de error.
	 * @param code C&oacute;digo de error.
	 * @return Mensaje. */
	static String getErrorMessage(final String code) {
		return ERRORS.get(code);
	}
}
