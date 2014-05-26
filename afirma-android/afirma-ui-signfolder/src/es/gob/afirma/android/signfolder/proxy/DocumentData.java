package es.gob.afirma.android.signfolder.proxy;

/**
 * Datos identificados de un fichero para su descarga o previsualizaci&oacute;n.
 */
public class DocumentData {

	private final String docId;

	private final String filename;

	private final String mimetype;

	private final String dataB64;

	/**
	 * Datos de un documento.
	 * @param id Identificador del documento.
	 * @param filename Nombre de fichero.
	 * @param mimetype MimeType de los datos.
	 * @param dataB64 Contenido del fichero en base 64.
	 */
	public DocumentData(final String id, final String filename, final String mimetype, final String dataB64) {
		this.docId = id;
		this.filename = filename;
		this.mimetype = mimetype;
		this.dataB64 = dataB64;
	}

	/**
	 * Recupera el identificador del documento.
	 * @return Identificador del documento.
	 */
	public String getDocId() {
		return this.docId;
	}

	/**
	 * Recupera el nombre del fichero.
	 * @return Nombre del fichero.
	 */
	public String getFilename() {
		return this.filename;
	}

	/**
	 * Recupera el MimeType de los datos.
	 * @return MimeType de los datos.
	 */
	public String getMimetype() {
		return this.mimetype;
	}

	/**
	 * Recupera el contenido del fichero en base 64.
	 * @return Contenido del fichero en base 64.
	 */
	public String getDataB64() {
		return this.dataB64;
	}
}
