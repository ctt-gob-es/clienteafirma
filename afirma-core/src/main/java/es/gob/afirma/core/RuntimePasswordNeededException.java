package es.gob.afirma.core;

import java.util.Properties;

/**
 * Excepci&oacute;n para notificar de que se requiere una contrase&ntilde;a del usuario.
 * @author carlos.gamuci
 */
public abstract class RuntimePasswordNeededException extends RuntimeConfigNeededException {

	/** Serial Id. */
	private static final long serialVersionUID = 8852303435406903975L;

	/**
	 * Crea la excepci&oacute;n.
	 * @param msg Mensaje descriptivo del problema.
	 * @param requestorText Identificador de la cadena de texto que debe
	 * usarse para pedir los datos al usuario.
	 */
	public RuntimePasswordNeededException(final String msg, final String requestorText) {
		super(msg, RequestType.PASSWORD, requestorText, null);
	}

	/**
	 * Crea la excepci&oacute;n.
	 * @param msg Mensaje descriptivo del problema.
	 * @param requestorText Identificador de la cadena de texto que debe
	 * usarse para pedir los datos al usuario.
	 * @param cause Causa del error.
	 */
	public RuntimePasswordNeededException(final String msg, final String requestorText, final Throwable cause) {
		super(msg, RequestType.PASSWORD, requestorText, null, cause);
	}

	/**
	 * Crea la excepci&oacute;n.
	 * @param msg Mensaje descriptivo del problema.
	 * @param requestorText Identificador de la cadena de texto que debe
	 * usarse para pedir los datos al usuario.
	 * @param param Nombre del par&aacute;metro que deber&iacute;a establecerse en la configuraci&oacute;n
	 * con la contrase&ntilde;a que se solicite.
	 * @param cause Causa del error.
	 */
	public RuntimePasswordNeededException(final String msg, final String requestorText, final String param, final Throwable cause) {
		super(msg, RequestType.PASSWORD, requestorText, param, cause);
	}

	/**
	 * Establece la configuraci&oacute;n adecuada para el uso de la nueva contrase&ntilde;a.
	 * @param config Configuraci&oacute;n a la que agregar&aacute; la contrase&ntilde;a.
	 * @param password Contrase&ntilde;a que se debe agregar a la configuraci&oacute;n.
	 */
	public abstract void configure(Properties config, char[] password);
}
