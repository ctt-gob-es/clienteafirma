package es.gob.afirma.plugin.hash.command;

/**
 * Excepci&oacute;n que identifica un error en la operativa del plugin al ejecutarlo
 * desde consola.
 */
public class HashCommandException extends Exception {

	/** Serial Id. */
	private static final long serialVersionUID = -2884038243941239958L;

	/**
	 * Constreuye la excepci&oacute;n con el error de consola y su causa.
	 * @param msg Mensaje que mostrar en consola.
	 * @param cause Causa del error.
	 */
	public HashCommandException(final String msg, final Throwable cause) {
		super(msg, cause);
	}
}
