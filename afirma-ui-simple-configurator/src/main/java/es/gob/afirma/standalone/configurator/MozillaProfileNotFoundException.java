package es.gob.afirma.standalone.configurator;

/**
 * Excepci&oacute;n que indica que no ha sido posible encontrar directorios de perfil
 * de Firefox en el sistema.
 */
public final class MozillaProfileNotFoundException extends Exception {

	private static final long serialVersionUID = 1L;

	/**
	 * Construye la excepci&oacute;n.
	 */
	public MozillaProfileNotFoundException() {
		super();
	}

	/**
	 * Construye la excepci&oacute;n indicando un mensaje de error.
	 * @param msg Mensaje de error.
	 */
	public MozillaProfileNotFoundException(final String msg) {
		super(msg);
	}
}
