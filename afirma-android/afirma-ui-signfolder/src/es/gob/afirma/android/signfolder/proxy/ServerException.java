package es.gob.afirma.android.signfolder.proxy;

/** Error al llamar a los servicios del Portafirmas. */
public final class ServerException extends RuntimeException {

	/** Serial Id. */
	private static final long serialVersionUID = -4715921829977071413L;

	/** Crea una excepci&oacute;n de error al llamar a los servicios del Portafirmas.
	 * @param msg Mensaje de error. */
	public ServerException(final String msg) {
		super(msg);
	}

	/** Crea una excepci&oacute;n de error al llamar a los servicios del Portafirmas.
	 * @param msg Mensaje de error.
	 * @param cause Causa del error. */
	public ServerException(final String msg, final Throwable cause) {
		super(msg, cause);
	}
}
