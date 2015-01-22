package es.gob.afirma.signfolder.server.proxy;

import java.io.InputStream;

/**
 * Datos identificados de un fichero para su descarga o previsualizaci&oacute;n.
 */
public class DocumentData {

	private final String docId;

	private final String filename;

	private final String mimetype;

	private InputStream dataIs;

	private String algorithm;

	private String format;

	/**
	 * Datos de un documento.
	 * @param id Identificador del documento.
	 * @param filename Nombre de fichero.
	 * @param mimetype MimeType de los datos.
	 */
	public DocumentData(final String id, final String filename, final String mimetype) {
		this.docId = id;
		this.filename = filename;
		this.mimetype = mimetype;
		this.dataIs = null;
		this.algorithm = null;
		this.format = null;
	}

	/**
	 * Datos de un documento.
	 * @param id Identificador del documento.
	 * @param filename Nombre de fichero.
	 * @param mimetype MimeType de los datos.
	 * @param dataIs Flujo de entrada de los datos.
	 */
	public DocumentData(final String id, final String filename, final String mimetype, final InputStream dataIs) {
		this.docId = id;
		this.filename = filename;
		this.mimetype = mimetype;
		this.dataIs = dataIs;
		this.algorithm = null;
		this.format = null;
	}

	/**
	 * Datos de un documento.
	 * @param id Identificador del documento.
	 * @param filename Nombre de fichero.
	 * @param mimetype MimeType de los datos.
	 * @param dataIs Flujo de entrada de los datos.
	 * @param algorithm Algoritmo que se debe usar para firmarlo.
	 * @param format Formato que se debe usar para firmarlo.
	 */
	public DocumentData(final String id, final String filename, final String mimetype, final InputStream dataIs, final String algorithm, final String format) {
		this.docId = id;
		this.filename = filename;
		this.mimetype = mimetype;
		this.dataIs = dataIs;
		this.algorithm = algorithm;
		this.format = format;
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
	 * Recupera el flujo de entrada de los datos.
	 * @return Flujo de entrada de los datos.
	 */
	public InputStream getDataIs() {
		return this.dataIs;
	}

	/**
	 * Recupera el algoritmo de firma.
	 * @return Algoritmo de firma.
	 */
	public String getAlgorithm() {
		return this.algorithm;
	}

	/**
	 * Recupera el formato de firma.
	 * @return Formato de firma.
	 */
	public String getFormat() {
		return this.format;
	}

	/**
	 * Establece el flujo entrada de los datos.
	 * @param dataIs Flujo de entrada de los datos.
	 */
	public void setDataIs(InputStream dataIs) {
		this.dataIs = dataIs;
	}

	/**
	 * Establece el algoritmo de firma.
	 * @param algorithm Algoritmo de firma.
	 */
	public void setAlgorithm(String algorithm) {
		this.algorithm = algorithm;
	}

	/**
	 * Establece el formato de firma.
	 * @param format Formato de firma.
	 */
	public void setFormat(String format) {
		this.format = format;
	}
}
