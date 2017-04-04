package es.gob.afirma.standalone.configurator;

/** Cuando no se puede generar el certificado.
 * Com&uacute;nmente por un error de configuraci&oacute;n. */
public final class ConfigurationException extends Exception {

	/** Serial Id. */
	private static final long serialVersionUID = 2072955063471096921L;

	/** Error de configuraci&oacute;n para la generaci&oacute;n del certificado.
	 * @param msg Descripci&oacute;n del error. */
	public ConfigurationException(final String msg) {
		super(msg);
	}

	/** Error de configuraci&oacute;n para la generaci&oacute;n del certificado.
	 * @param msg Descripci&oacute;n del error.
	 * @param cause Motivo que origin&oacute; el error. */
	public ConfigurationException(final String msg, final Throwable cause) {
		super(msg, cause);
	}
}
