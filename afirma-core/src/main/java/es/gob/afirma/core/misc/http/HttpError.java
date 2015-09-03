package es.gob.afirma.core.misc.http;

import java.io.IOException;

/** Error de conexi&oacute;n HTTP.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class HttpError extends IOException {

	private static final long serialVersionUID = 8997766820804553378L;

	private final int responseCode;
	private final String responseDescription;

	/** Crea una excepci&oacute;n de error de conexi&oacute;n HTTP.
	 * @param resCode C&oacute;digo HTTP de respuesta. */
	HttpError(final int resCode) {
		super("Error en conexion HTTP con codigo de respuesta " + resCode); //$NON-NLS-1$
		this.responseCode = resCode;
		this.responseDescription = null;
	}

	/** Crea una excepci&oacute;n de error de conexi&oacute;n HTTP.
	 * @param resCode C&oacute;digo HTTP de respuesta.
	 * @param resDescription Descripci&oacute;n del error. */
	HttpError(final int resCode, final String resDescription) {
		super("Error en conexion HTTP con codigo de respuesta " + resCode + " y descripcion: " + resDescription); //$NON-NLS-1$ //$NON-NLS-2$
		this.responseCode = resCode;
		this.responseDescription = resDescription;
	}

	/** Obtiene el c&oacute;digo HTTP de respuesta.
	 * @return C&oacute;digo HTTP de respuesta. */
	public int getResponseCode() {
		return this.responseCode;
	}

	/** Obtiene la descripci&oacute;n del error HTTP.
	 * @return Descripci&oacute;n del error HTTP. */
	public String getResponseDescription() {
		return this.responseDescription;
	}

}
