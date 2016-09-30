package es.gob.afirma.android.signfolder.proxy;

/**
 * L&iacute;nea de firma.
 */
public class SignLine {

	private final String signer;

	private final boolean done;

	/**
	 * Crea la l&iacute;nea de firma indicando el firmante. Por defecto, se considera que
	 * la operaci&oacute;n de firma/visto bueno en cuesti&oacute;n a&uacute;n no se ha
	 * ejecutado.
	 * @param signer Firmante.
	 */
	public SignLine(final String signer) {
		this.signer = signer;
		this.done = false;
	}

	/**
	 * Crea la l&iacute;nea de firma indicando el firmante y el estado de la firma/visto
	 * bueno en cuestion.
	 * @param signer Firmante.
	 * @param done Estado de la operaci&oacute;n de firma.
	 */
	public SignLine(final String signer, final boolean done) {
		this.signer = signer;
		this.done = done;
	}

	/**
	 * Recupera el firmante de la operaci&oacute;n.
	 * @return Firmante.
	 */
	public String getSigner() {
		return this.signer;
	}

	/**
	 * Indica si la operaci&oacute;n ya se ha realizado.
	 * @return {@code true} si la operaci&oacute;n ya se ha ejecutado,
	 * {@code false} en caso contrario.
	 */
	public boolean isDone() {
		return this.done;
	}
}
