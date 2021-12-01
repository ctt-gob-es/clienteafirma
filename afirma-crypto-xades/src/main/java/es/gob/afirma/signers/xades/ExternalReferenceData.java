package es.gob.afirma.signers.xades;

/**
 * Datos de una referencia externa de la firma.
 */
public class ExternalReferenceData {

	private final String uri;

	private final byte[] messageDigest;

	private String id;

	private String mimeType;

	private String encoding;

	private String oid;

	/**
	 * Construye el conjunto de datos imprescindibles de una referencia externa.
	 * @param uri URI de referencia a los datos.
	 * @param md Huella digital (hash) de los datos.
	 */
	public ExternalReferenceData(final String uri, final byte[] md) {
		this.uri = uri;
		this.messageDigest = md != null ? md.clone() : null;

	}

	/**
	 * Recupera la URI que identifica el destino de la referencia.
	 * @return URI.
	 */
	public String getUri() {
		return this.uri;
	}

	/**
	 * Recupera la huella digital de los datos referenciados.
	 * @return Huella digital/hash.
	 */
	public byte[] getMessageDigest() {
		return this.messageDigest != null ? this.messageDigest.clone() : null;
	}

	/**
	 * Recupera el identificador asignado a la referencia.
	 * @return Identificador de la referencia.
	 */
	public String getId() {
		return this.id;
	}

	/**
	 * Establece el identificador de la referencia.
	 * @param id Identificador de la referencia.
	 */
	public void setId(final String id) {
		this.id = id;
	}

	/**
	 * Recupera el mimetype que define el tipo de los datos referenciados.
	 * @return MimeType de los datos.
	 */
	public String getMimeType() {
		return this.mimeType;
	}

	/**
	 * Establece el mimetype que define el tipo de los datos referenciados.
	 * @param mimeType MimeType de los datos.
	 */
	public void setMimeType(final String mimeType) {
		this.mimeType = mimeType;
	}

	/**
	 * Recupera la codificaci&oacute;n de texto de los datos referenciados.
	 * @return Codificaci&oacute;n de los datos.
	 */
	public String getEncoding() {
		return this.encoding;
	}

	/**
	 * Establece la codificaci&oacute;n de los que define el tipo de los datos referenciados.
	 * @param encoding Codificaci&oacute;n de texto.
	 */
	public void setEncoding(final String encoding) {
		this.encoding = encoding;
	}

	/**
	 * Recupera el mimetype que define el tipo de los datos referenciados.
	 * @return Identificador del tipo de dato.
	 */
	public String getOid() {
		return this.oid;
	}

	/**
	 * Establece el mimetype que define el tipo de los datos referenciados.
	 * @param oid Identificador del tipo de dato.
	 */
	public void setOid(final String oid) {
		this.oid = oid;
	}
}
