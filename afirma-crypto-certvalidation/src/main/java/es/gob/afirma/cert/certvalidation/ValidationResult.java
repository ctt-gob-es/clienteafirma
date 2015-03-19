package es.gob.afirma.cert.certvalidation;

/** Resultado de la validaci&oacute;n de un certificado X.509.
 * Clase cedida por <a href="http://www.yohago.com/">YoHago</a>.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public enum ValidationResult {

	/** V&aacute;lido. */
	VALID(0),
	/** No compatible X.509 o corrupto. */
	CORRUPT(1),
	/** No se soporta la CA de expedici&oacute;n. */
	CA_NOT_SUPPORTED(2),
	/** Aun no v&aacute;lido. */
	NOT_YET_VALID(3),
	/** Caducado. */
	EXPIRED(4),
	/** Revocado. */
	REVOKED(5),
	/** Desconocido. */
	UNKNOWN(6),
	/** Error interno o del servidor. */
	SERVER_ERROR(7);

	private static final int CODE_VALID = 0;
	private static final int CODE_CORRUPT = 1;
	private static final int CODE_CA_NOT_SUPPORTED = 2;
	private static final int CODE_NOT_YET_VALID = 3;
	private static final int CODE_EXPIRED = 4;
	private static final int CODE_REVOKED = 5;
	private static final int CODE_UNKNOWN = 6;
	private static final int CODE_SERVER_ERROR = 7;

	private final int resultCode;
	private ValidationResult(final int code) {
		if (code < CODE_VALID || code > CODE_SERVER_ERROR) {
			throw new IllegalArgumentException(
				"El codigo de resultado debe estar comprendido entre 0 y 7: " + code //$NON-NLS-1$
			);
		}
		this.resultCode = code;
	}

	/** Obtiene la representaci&oacute;n JSON del resultado de la validaci&oacute;n.
	 * @return Representaci&oacute;n JSON del resultado de la validaci&oacute;n */
	public String toJsonString() {
		return new StringBuilder()
			.append("{\n") //$NON-NLS-1$
			.append("  \"result\": \"").append(isValid() ? "OK" : "KO").append("\",\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  \"reason\": \"").append(toString()).append("\"\n") //$NON-NLS-1$ //$NON-NLS-2$
			.append("}") //$NON-NLS-1$
		.toString();
	}

	/** Indica si el resultado corresponde a un certificado X.509v3 v&aacute;lido.
	 * @return <code>true</code> si corresponde a un certificado X.509v3 v&aacute;lido, <code>false</code>
	 *         en caso contrario */
	public boolean isValid() {
		return this.resultCode == CODE_VALID;
	}

	@Override
	public String toString() {
		switch(this.resultCode) {
			case CODE_VALID:
				return "Valido"; //$NON-NLS-1$
			case CODE_CORRUPT:
				return "No compatible X.509 o corrupto"; //$NON-NLS-1$
			case CODE_CA_NOT_SUPPORTED:
				return "No es de una CA soportada"; //$NON-NLS-1$
			case CODE_NOT_YET_VALID:
				return "Aun no valido"; //$NON-NLS-1$
			case CODE_EXPIRED:
				return "Caducado"; //$NON-NLS-1$
			case CODE_REVOKED:
				return "Revocado"; //$NON-NLS-1$
			case CODE_UNKNOWN:
				return "Desconocido"; //$NON-NLS-1$
			case CODE_SERVER_ERROR:
				return "Error interno o del servidor"; //$NON-NLS-1$
			default:
				throw new IllegalStateException(
					"El codigo de resultado debe estar comprendido entre 0 y 7: " + this.resultCode //$NON-NLS-1$
				);
		}
	}
}