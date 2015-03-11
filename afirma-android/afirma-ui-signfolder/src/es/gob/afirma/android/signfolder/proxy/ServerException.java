package es.gob.afirma.android.signfolder.proxy;

/**
 * Se utiliza cuando se obtiene un error al llamar a los servicios del Portafirmas.
 */
public class ServerException extends RuntimeException {

	/** Serial Id. */
	private static final long serialVersionUID = -4715921829977071413L;

	public ServerException(final String msg) {
		super(msg);
	}

	public ServerException(final String msg, final Throwable cause) {
		super(msg, cause);
	}
}
