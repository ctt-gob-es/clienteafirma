package es.gob.afirma.miniapplet;

import java.security.PrivilegedAction;

/**
 * Acci&oacute;n privilegiada para la recuperaci&oacute;n de una propiedad del sistema.
 * @author Carlos Gamuci Mill&aacute;n
 */
final class GetPropertyAction implements PrivilegedAction<String> {

	private final String property;

	/**
	 * Crea la opci&oacute;n para recuperar la propiedad indicada.
	 * @param property Propiedad del sistema que se desea recuperar.
	 */
	public GetPropertyAction(final String property) {
		this.property = property;
	}

	/**
	 * Recupera la propiedad del sistema indicada.
	 */
	@Override
	public String run() {
		return System.getProperty(this.property);
	}
}
