package es.gob.afirma.signfolder.server.proxy;

/**
 * Petici&oacute;n del detalle de una solicitud de firma.
 */
public class DetailRequest {

	private final byte[] certEncoded;
	private final String id;

	/**
	 * Construye una petici&oacute;n de una solicitud de firma.
	 * @param certEncoded Certificado codificado para autenticar la petici&oacute;n.
	 * @param id Identificador de la petici&oacute;n.
	 */
	public DetailRequest(final byte[] certEncoded, final String id) {
		this.certEncoded = certEncoded;
		this.id = id;
	}

	/**
	 * Recupera el certificado para la autenticaci&oacute;n de la petici&oacute;n.
	 * @return Certificado codificado.
	 */
	public byte[] getCertEncoded() {
		return this.certEncoded;
	}

	/**
	 * Recupera el identificador de la petici&oacute;n solicitada.
	 * @return Identificador.
	 */
	public String getRequestId() {
		return this.id;
	}
}
