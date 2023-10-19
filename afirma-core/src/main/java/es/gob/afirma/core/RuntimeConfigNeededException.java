package es.gob.afirma.core;

/**
 * Identifica que no se puede completar una operaci&oacute;n sin configuraci&oacute;n adicional que se podr&iacute;
 * proporcionar en tiempo de ejecuci&oacute;n (probablemente la podr&iacute;a proporcionar un usuario). La
 * configuraci&oacute;n puede ser un dato o una confirmaci&oacute;n y una aplicaci&oacute;n que tenga la capacidad
 * deber&iacute; proporcionar los medios necesarios para obtener esta confirmaci&oacute;n.
 */
public class RuntimeConfigNeededException extends AOException {

	/** Serial Id. */
	private static final long serialVersionUID = 6782826789825877195L;

	private final RequestType requestType;

	private final String requestorText;

	private final String param;

	/**
	 * Construye la excepcion con la informaci&oacute;n necesaria para poder solicitar la
	 * configuraci&oacute;n que se necesite.
	 * @param msg Descripci&oacute;n del error.
	 * @param requestType Tipo de configuraci&oacute;n necesaria.
	 * @param requestorText Mensaje o c&oacute;digo de mensaje para la solicitud de la informaci&oacute;n.
	 * @param param Identificador del par&aacute;metro que se configurar&aacute; con la informaci&oacute;n.
	 * proporcionada.
	 */
	public RuntimeConfigNeededException(final String msg, final RequestType requestType, final String requestorText, final String param) {
		super(msg);
		this.requestType = requestType;
		this.requestorText = requestorText;
		this.param = param;
	}

	/**
	 * Construye la excepcion con la informaci&oacute;n necesaria para poder solicitar la
	 * configuraci&oacute;n que se necesite.
	 * @param msg Descripci&oacute;n del error.
	 * @param requestType Tipo de configuraci&oacute;n necesaria.
	 * @param requestorText Mensaje o c&oacute;digo de mensaje para la solicitud de la informaci&oacute;n.
	 * @param param Identificador del par&aacute;metro que se configurar&aacute; con la informaci&oacute;n.
	 * proporcionada.
	 * @param e Error que origin&oacute; la excepci&oacute;n.
	 */
	public RuntimeConfigNeededException(final String msg, final RequestType requestType, final String requestorText, final String param, final Throwable e) {
		super(msg, e);
		this.requestType = requestType;
		this.requestorText = requestorText;
		this.param = param;
	}

	/**
	 * Obtiene el tipo de configuraci&oacute;n necesaria.
	 * @return Tipo de configuraci&oacute;n necesaria.
	 */
	public RequestType getRequestType() {
		return this.requestType;
	}

	/**
	 * Obtiene el mensaje o c&oacute;digo de mensaje para la solicitud de la informaci&oacute;n.
	 * @return Mensaje o c&oacute;digo de mensaje.
	 */
	public String getRequestorText() {
		return this.requestorText;
	}

	/**
	 * Obtiene el identificador del par&aacute;metro de configuraci&oacute;n.
	 * @return Identificador del par&aacute;metro que se configurar&aacute; con la informaci&oacute;n.
	 */
	public String getParam() {
		return this.param;
	}

	/**
	 * Tipos de configuraci&oacute;n necesaria.
	 */
	public static enum RequestType {
		/** Requiere confirmaci&oacute;n. */
		CONFIRM,
		/** Requiere que se proporcione una contrase&tilde;a. */
		PASSWORD
	}
}
