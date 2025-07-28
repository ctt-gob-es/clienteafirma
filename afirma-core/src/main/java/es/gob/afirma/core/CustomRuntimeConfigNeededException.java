package es.gob.afirma.core;

import java.util.Properties;

/**
 * Identifica que no se puede completar una operaci&oacute;n sin configuraci&oacute;n adicional que se podr&iacute;
 * proporcionar en tiempo de ejecuci&oacute;n (probablemente la podr&iacute;a proporcionar un usuario) y se
 * proporciona la l&oacute;gica necesaria para la configuraci&oacute;n. La configuraci&oacute;n puede ser un dato
 * o una confirmaci&oacute;n y una aplicaci&oacute;n que tenga la capacidad deber&iacute; proporcionar los medios
 * necesarios para obtener esta confirmaci&oacute;n.
 */
public abstract class CustomRuntimeConfigNeededException extends RuntimeConfigNeededException {

	/** Serial Id. */
	private static final long serialVersionUID = -42502407809095471L;

	/**
	 * Construye la excepcion con la informaci&oacute;n necesaria para poder solicitar la
	 * configuraci&oacute;n que se necesite.
	 * @param msg Descripci&oacute;n del error.
	 * @param requestType Tipo de configuraci&oacute;n necesaria.
	 * @param requestorText Mensaje o c&oacute;digo de mensaje para la solicitud de la informaci&oacute;n.
	 * @param param Identificador del par&aacute;metro que se configurar&aacute; con la informaci&oacute;n.
	 * proporcionada.
	 */
	public CustomRuntimeConfigNeededException(final String msg, final RequestType requestType, final String requestorText, final String param) {
		super(msg, requestType, requestorText, param);
	}

	/**
	 * Construye la excepcion con la informaci&oacute;n necesaria para poder solicitar la
	 * configuraci&oacute;n que se necesite.
	 * @param msg Descripci&oacute;n del error.
	 * @param requestType Tipo de configuraci&oacute;n necesaria.
	 * @param requestorText Mensaje o c&oacute;digo de mensaje para la solicitud de la informaci&oacute;n.
	 * @param param Identificador del par&aacute;metro que se configurar&aacute; con la informaci&oacute;n.
	 * proporcionada.
	 * @param cause Origen del problema.
	 */
	public CustomRuntimeConfigNeededException(final String msg, final RequestType requestType, final String requestorText, final String param, final Throwable cause) {
		super(msg, requestType, requestorText, param, cause);
	}

	/**
	 * Modifica la configuraci&oacute;n de la operaci&oacute;n seg&uacute;n la respuesta del usuario.
	 * @param extraParams Configuraci&oacute;n de la operaci&oacute;n de firma.
	 */
	public abstract void prepareOperationWithConfirmation(Properties extraParams);
}
