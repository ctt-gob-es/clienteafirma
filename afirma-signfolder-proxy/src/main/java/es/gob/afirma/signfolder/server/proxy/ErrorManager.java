package es.gob.afirma.signfolder.server.proxy;

import java.util.Dictionary;
import java.util.Hashtable;

final class ErrorManager {

	private static final String ERROR_NUMBER = "%#%"; //$NON-NLS-1$
	private static final String ERROR_MESSAGE = "%MSG%"; //$NON-NLS-1$
	private static final String ERROR_TEMPLATE = "<err cd=\"" + ERROR_NUMBER + "\">" + ERROR_MESSAGE + "</err>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

	private static final String GENERIC_ERROR = "Error generico"; //$NON-NLS-1$

	static final String ERROR_MISSING_OPERATION_NAME     = "ERR-00"; //$NON-NLS-1$
	static final String ERROR_UNSUPPORTED_OPERATION_NAME = "ERR-01"; //$NON-NLS-1$
	static final String ERROR_MISSING_DATA               = "ERR-02"; //$NON-NLS-1$
	static final String ERROR_BAD_XML                    = "ERR-03"; //$NON-NLS-1$
	static final String ERROR_BAD_CERTIFICATE            = "ERR-04"; //$NON-NLS-1$
	static final String ERROR_MISSING_DATA_ID            = "ERR-05"; //$NON-NLS-1$
	static final String ERROR_INVALID_DATA_ID            = "ERR-06"; //$NON-NLS-1$
	static final String ERROR_INVALID_DATA               = "ERR-07"; //$NON-NLS-1$
	static final String ERROR_COMMUNICATING_PORTAFIRMAS  = "ERR-08"; //$NON-NLS-1$
	static final String ERROR_COMMUNICATING_SERVICE 	 = "ERR-09"; //$NON-NLS-1$
	static final String ERROR_UNKNOWN_ERROR				 = "ERR-10"; //$NON-NLS-1$
	static final String ERROR_AUTHENTICATING_REQUEST	 = "ERR-11"; //$NON-NLS-1$


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
		ERRORS.put(ERROR_COMMUNICATING_PORTAFIRMAS, "Ocurrio un error en la comunicacion con el Port@firmas"); //$NON-NLS-1$
		ERRORS.put(ERROR_COMMUNICATING_SERVICE, "Ocurrio un error al contactar con un webservice externo"); //$NON-NLS-1$
		ERRORS.put(ERROR_UNKNOWN_ERROR, "Error desconocido en las peticiones al portafirmas"); //$NON-NLS-1$
		ERRORS.put(ERROR_AUTHENTICATING_REQUEST, "Error en la autenticacion de la peticion"); //$NON-NLS-1$
	}

	private ErrorManager() {
		// No instanciable
	}

	static String genError(final String number) {
		return genError(number, null);
	}

	static String genError(final String number, final String msg) {
		return
				ERROR_TEMPLATE.replace(ERROR_NUMBER, number).replace(
						ERROR_MESSAGE,
						msg != null ? msg : ERRORS.get(number) != null ? ERRORS.get(number) : GENERIC_ERROR
						);
	}
}
