package es.gob.afirma.signfolder.proxy;


/** Informaci&oacute;n de un documento de una solicitud de firma.
 * @author Carlos Gamuci */
public class SignRequestDocument {

	/** Identificador del documento */
	private final String id;

	/** Nombre del documento */
	private final String name;

	/** MimeType del documento */
	private final String mimeType;

	/** Formato de firma a aplicar */
	private final String signFormat;

	/** Crea un documento englobado en una petici&oacute;n de firma.
	 * @param id Identificador del documento.
	 * @param name Nombre.
	 * @param mimeType MimeType.
	 * @param signFormat Formato de firma a aplicar. */
	public SignRequestDocument(final String id, final String name, final String mimeType, final String signFormat) {
		this.id = id;
		this.name = name;
		this.mimeType = mimeType;
		this.signFormat = signFormat;
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
}
