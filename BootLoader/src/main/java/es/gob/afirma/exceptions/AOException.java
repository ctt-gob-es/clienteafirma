package es.gob.afirma.exceptions;

/**
 * Excepci&oacute;n gen&eacute;rica.
 * @version 1.0
 */
public final class AOException extends Exception {

	private static final long serialVersionUID = -662191654860389176L;

	/**
	 * Contruye una excepci&oacute;n gen&eacute;rica con mensaje.
	 * @param msg Mensaje de la excepci&oacute;n
	 */
	public AOException(final String msg) {
		super(msg);
	}
	
	/**
	 * Contruye una excepci&oacute;n gen&eacute;rica con mensaje y define su causa.
	 * @param msg Descripcion del error.
	 * @param e Causa del error.
	 */
	public AOException(final String msg, final Exception e) {
		super(msg, e);
	}
}
