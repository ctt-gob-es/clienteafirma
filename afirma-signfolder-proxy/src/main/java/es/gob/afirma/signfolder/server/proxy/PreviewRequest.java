package es.gob.afirma.signfolder.server.proxy;

/**
 * Petici&oacute;n para la previsualizaci&oacute;n de un documento.
 */
public class PreviewRequest {

	private final String docId;
	private final byte[] certEncoded;

	/**
	 * Construye la petici&oacute;n para la previsualizaci&oacute;n del documento.
	 * @param docId Identificador del documento.
	 * @param certEncoded Certificado codificado con el que realizar la autenticaci&oacute;n de la petici&oacute;n.
	 */
	public PreviewRequest(final byte[] certEncoded, final String docId) {
		this.certEncoded = certEncoded;
		this.docId = docId;
	}

	/**
	 * Recupera el identificador del documento.
	 * @return Identificador.
	 */
	public String getDocId() {
		return this.docId;
	}

	/**
	 * Recupera el certificado con el que se har&aacute; la autenticaci&oacute;n.
	 * @return Certificado codificado.
	 */
	public byte[] getCertEncoded() {
		return this.certEncoded;
	}
}
