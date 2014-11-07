package es.gob.afirma.android.signfolder.proxy;

/** Informaci&oacute;n de un documento de una solicitud de firma.
 * @author Carlos Gamuci */
public final class SignRequestDocument {

	static final String CRYPTO_OPERATION_SIGN = "sign"; //$NON-NLS-1$
	static final String CRYPTO_OPERATION_COSIGN = "cosign"; //$NON-NLS-1$
	static final String CRYPTO_OPERATION_COUNTERSIGN = "countersign"; //$NON-NLS-1$

	/** Identificador del documento */
	private final String id;

	/** Operaci&oacute;n que se debe realizar sobre el documento (sign, cosign o countersign). */
	private final String cryptoOperation;

	/** Nombre del documento */
	private final String name;

	/** Tama&ntilde;o del documento */
	private final int size;

	/** MimeType del documento */
	private final String mimeType;

	/** Formato de firma a aplicar */
	private final String signFormat;

	/** Formato de firma a aplicar */
	private final String messageDigestAlgorithm;

	/** Par&aacute;metros de firma conforme a las especificaciones de los extraParams de @firma. */
	private final String params;

	/** Crea un documento englobado en una petici&oacute;n de firma.
	 * @param id Identificador del documento.
	 * @param name Nombre.
	 * @param size Tama&ntilde;o.
	 * @param mimeType MimeType.
	 * @param signFormat Formato de firma a aplicar.
	 * @param messageDigestAlgorithm Algoritmo de huella digital que debe usarse en la firma del documento.
	 * @param params Par&aacute;metros de configuraci&oacute;n de la firma.
	 */
	public SignRequestDocument(final String id, final String name, final int size, final String mimeType, final String signFormat, final String messageDigestAlgorithm, final String params) {
		this(id, name, size, mimeType, signFormat, messageDigestAlgorithm, params, CRYPTO_OPERATION_SIGN);
	}

	/** Crea un documento englobado en una petici&oacute;n de firma/multifirma.
	 * @param id Identificador del documento.
	 * @param name Nombre.
	 * @param size Tama&ntilde;o.
	 * @param mimeType MimeType.
	 * @param signFormat Formato de firma a aplicar.
	 * @param messageDigestAlgorithm Algoritmo de huella digital que debe usarse en la firma del documento.
	 * @param params Par&aacute;metros de configuraci&oacute;n de la firma.
	 * @param operationId Identificador de la operaci&oacute;n que se debe realizar sobre le documento.
	 */
	private SignRequestDocument(final String id, final String name, final int size, final String mimeType, final String signFormat, final String messageDigestAlgorithm, final String params, final String cryptoOperation) {
		this.id = id;
		this.name = name;
		this.size = size;
		this.mimeType = mimeType;
		this.signFormat = signFormat;
		this.messageDigestAlgorithm = messageDigestAlgorithm;
		this.params = params;
		this.cryptoOperation = cryptoOperation;
	}

	/** Recupera el identificador del documento.
	 * @return Identificador. */
	public String getId() {
		return this.id;
	}

	/** Recupera la operaci&oacute;n que debe realizarse sobre el documento (firma, cofirma, contrafirma de hojas o contrafirma de arbol).
	 * @return Identificador del tipo de operaci&oacute;n. */
	public String getCryptoOperation() {
		return this.cryptoOperation;
	}

	/** Recupera el nombre del documento.
	 * @return Nombre del documento. */
	public String getName() {
		return this.name;
	}

	/** Recupera el tama&ntilde;o del documento.
	 * @return Tama&ntilde;o del documento. */
	public int getSize() {
		return this.size;
	}

	/** Recupera el mimetype del documento.
	 * @return Mimetype del documento. */
	public String getMimeType() {
		return this.mimeType;
	}

	/** Recupera el formato de firma que se le debe aplicar al documento.
	 * @return Formato de firma que se le debe aplicar al documento. */
	public String getSignFormat() {
		return this.signFormat;
	}

	/** Recupera el algoritmo de huella digital asociado al algoritmo de firma que se desea utilizar.
	 * @return Algoritmo de huella digial que se usara en la firma. */
	public String getMessageDigestAlgorithm() {
		return this.messageDigestAlgorithm;
	}

	/** Recupera los par&aacute;metros de configuraci&oacute;n para la firma
	 * conforme al formato de extraParams de @firma.
	 * @return Par&aacute;metros de configuraci&oacute;n de la firma. */
	public String getParams() {
		return this.params;
	}

	@Override
	public String toString() {
		return getName();
	}
}
