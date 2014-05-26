package es.gob.afirma.signfolder.server.proxy;

import java.util.ArrayList;
import java.util.List;

/**
 * Solicitud para el rechazo de peticiones.
 */
public final class RejectRequest extends ArrayList<String> {

	/** Serial ID. */
	private static final long serialVersionUID = 1L;

	private final byte[] certEncoded;

	/**
	 * Crea la solicitud de rechazo de una lista de peticiones de firma.
	 * @param certEncoded Certificado codificado para la autenticaci&oacute;n
	 * @param ids Identificadores de las peticiones a rechazar.
	 */
	public RejectRequest(final byte[] certEncoded, final List<String> ids) {
		this.certEncoded = certEncoded;
		this.addAll(ids);
	}

	/**
	 * Recupera el certificado codificado para la autenticaci&oacute;n de la petici&oacute;n.
	 * @return Certificado codificado.
	 */
	public byte[] getCertEncoded() {
		return this.certEncoded;
	}
}
