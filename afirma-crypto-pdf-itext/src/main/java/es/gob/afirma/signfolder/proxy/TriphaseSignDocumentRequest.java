package es.gob.afirma.signfolder.proxy;

/**
 * Petici&oacute;n de prefirma de un documento.
 * @author Carlos Gamuci Mill&aacute;n
 */
public class TriphaseSignDocumentRequest {

	/** Identificador del documento. */
	private final String id;

	/** Formato de firma electr&oacute;nica con el que se desea prefirmar. */
	private final String signatureFormat;

	/** Propiedades de configuracion de la firma codificadas en base 64. */
	private final String params;

	/** Metadatos de configuracion de la prefirma codificados en base 64. */
	private final String metadata;

	/** Resultado de la prefirma en base 64. */
	private final String preSignature;

	/**
	 * Construye un objeto petici&oacute;n de prefirma de un documento.
	 * @param docId Identificador del documento.
	 * @param signatureFormat Formato de firma electr&oacute;nica que se desea para
	 * la prefirma del documento.
	 */
	public TriphaseSignDocumentRequest(final String docId, final String signatureFormat) {
		this.id = docId;
		this.signatureFormat = signatureFormat;
		this.params = null;
		this.metadata = null;
		this.preSignature = null;
	}

	/**
	 * Construye un objeto petici&oacute;n de prefirma de un documento.
	 * @param docId Identificador del documento.
	 * @param signatureFormat Formato de firma electr&oacute;nica que se desea para
	 * la prefirma del documento.
	 * @param params Propiedades de configuraci&oacute;n de la prefirma codificadas en base 64.
	 * @param meta Datos de configuraci&oacute;n de la prefirma codificados en base 64.
	 * @param presignature Resultado de la operaci&oacute;n de prefirma codificado en base 64.
	 */
	public TriphaseSignDocumentRequest(final String docId, final String signatureFormat, final String params,
			final String meta, final String presignature) {
		this.id = docId;
		this.signatureFormat = signatureFormat;
		this.params = params;
		this.metadata = meta;
		this.preSignature = presignature;
	}

	/**
	 * Recupera el identificador del documento.
	 * @return Identificador del documento.
	 */
	public String getId() {
		return this.id;
	}

	/**
	 * Recupera el formato de firma en el que se desea prefirmar.
	 * @return Formato de firma.
	 */
	public String getSignatureFormat() {
		return this.signatureFormat;
	}

	/**
	 * Recupera las propiedades de configuraci&oacute;n de la operaci&oacute;n de prefirma.
	 * @return Propiedades de configuraci&oacute;n codificadas en base 64 o {@code null} si
	 * no hay par&aacute;metros establecidos.
	 */
	public String getParams() {
		return this.params;
	}

	/**
	 * Recupera los metadatos generados de la operaci&oacute;n de prefirma.
	 * @return Par&aacute;metros de configuraci&oacute;n codificados en base 64 o {@code null} si
	 * no hay metadatos establecidos.
	 */
	public String getMetadata() {
		return this.metadata;
	}

	/**
	 * Recupera la prefirma generada.
	 * @return Prefirma codificada en base 64 o {@code null} si se ha establecido la prefirma.
	 */
	public String getPreSignature() {
		return this.preSignature;
	}
}
