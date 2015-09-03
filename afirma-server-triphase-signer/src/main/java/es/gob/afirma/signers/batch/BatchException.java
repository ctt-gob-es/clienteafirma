package es.gob.afirma.signers.batch;

/** Error en el proceso de firma por lotes.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class BatchException extends Exception {

	private static final long serialVersionUID = 1L;

	BatchException(final String msg, final Throwable e) {
		super(msg, e);
	}

}
