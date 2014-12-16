package es.gob.afirma.core.misc.protocol;

/** Error que indica que se necesita una versi&oacute;n m&aacute;s actualizada del aplicativo.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class ParameterNeedsUpdatedVersionException extends ParameterException {

	private static final long serialVersionUID = 7936191422727825394L;

	ParameterNeedsUpdatedVersionException() {
		super("Se necesita actualizar la aplicacion"); //$NON-NLS-1$
	}

}
