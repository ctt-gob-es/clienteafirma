package es.gob.afirma.standalone.plugins.manager;

/**
 * Excepci&oacute;n que se&ntilde;ala que un JAR no est&aacute; firmado o
 * no completamente firmado.
 */
public class JarNoSignedException extends Exception {

	/** Serial Id. */
	private static final long serialVersionUID = 7937166390999870039L;

	/**
	 * Construye la excepci&oacute;n.
	 */
	public JarNoSignedException() {
		super();
	}

	/**
	 * Construye la excepci&oacute;n con un mensaje descriptivo.
	 * @param msg Mensaje descriptivo.
	 */
	public JarNoSignedException(final String msg) {
		super(msg);
	}
}
