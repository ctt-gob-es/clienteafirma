package es.gob.afirma.core.misc.protocol;

/** Error que indica que se ha solicitado en los par&aacute;metros un acceso local prohibido.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class ParameterLocalAccessRequestedException extends ParameterException {

	private static final long serialVersionUID = -6979789543878872249L;

	ParameterLocalAccessRequestedException(final String msg) {
		super(msg);
	}

	ParameterLocalAccessRequestedException(final String msg, final Throwable cause) {
		super(msg, cause);
	}
}
