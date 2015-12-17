package es.gob.afirma.signfolder.server.proxy;

/** Informaci&oacute;n de un documento de una solicitud de firma.
 * @author Carlos Gamuci */
public class SignRequestDocument {

	/** Identificador del documento */
	private final String id;

	/** Nombre del documento */
	private final String name;

	/** Tama&ntilde;o del documento */
	private final Integer size;

	/** MimeType del documento */
	private final String mimeType;

	/** Operacion a realizar sobre el fichero */
	private final String operation;

	/** Formato de firma a aplicar */
	private final String signFormat;

	/** Formato de firma a aplicar */
	private final String messageDigestAlgorithm;

	/** Par&aacute;metros de configuraci&oacute;n de firma*/
	private final String params;

	/** Crea un documento englobado en una petici&oacute;n de firma.
	 * @param id Identificador del documento.
	 * @param name Nombre.
	 * @param size Tama&ntilde;o del documento.
	 * @param mimeType MimeType.
	 * @param operation Tipo de operacion que debe realizarse sobre el fichero.
	 * @param signFormat Formato de firma a aplicar.
	 * @param messageDigestAlgorithm Algoritmo de huella digital que se utilizar&aacute; en la operaci&oacute;n de firma.
	 * @param params Par&aacute;metros de configuraci&oacute;n de firma.
	 */
	public SignRequestDocument(final String id, final String name, final Integer size, final String mimeType, final String operation, final String signFormat, final String messageDigestAlgorithm, final String params) {
		this.id = id;
		this.name = name;
		this.size = size;
		this.mimeType = mimeType;
		this.operation = operation;
		this.signFormat = signFormat;
		this.messageDigestAlgorithm = messageDigestAlgorithm;
		this.params = params;
	}

	/** Recupera el identificador del documento.
	 * @return Identificador. */
	public String getId() {
		return this.id;
	}

	/** Recupera el nombre del documento.
	 * @return Nombre del documento. */
	public String getName() {
		return this.name;
	}

	/** Recupera el tama&ntilde;o del documento.
	 * @return Tama&ntilde;o del documento. */
	public Integer getSize() {
		return this.size;
	}

	/** Recupera el mimetype del documento.
	 * @return Mimetype del documento. */
	public String getMimeType() {
		return this.mimeType;
	}

	/** Recupera el tipo de operacion que debe realizarse sobre el documento
	 * (firma, cofirma, contrafirma de nodos de hojas o visto bueno).
	 * @return Tipo de operaci&oacute;n. */
	public String getOperation() {
		return this.operation;
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

	/** Recupera los par&aacute;metros de configuraci&oacute;n de la firma (pol&iacute;tica de firma,
	 * modo, atributos adicionales,...).
	 * @return Parametros de firma. */
	public String getParams() {
		return this.params;
	}
}
