package es.gob.afirma.signfolder.server.proxy;

import java.util.ArrayList;

public class ApproveRequestList extends ArrayList<ApproveRequest> {

	/** Default SerialId */
	private static final long serialVersionUID = 1L;
	
	/** Certificado de firma. */
	private final byte[] certEncoded;
	
	/**
	 * Construye el objeto con los datos necesario para la solicitud del visto bueno
	 * de un listado de peticiones. 
	 * @param certEncoded Certificado para la autenticaci&oacute;n de la solicitud.
	 */
	public ApproveRequestList(final byte[] certEncoded) {
		this.certEncoded = certEncoded;
	}
	
	/**
	 * Recupera el certificado para la autenticaci&oacute;n de la petici&oacute;n.
	 * @return Certificado.
	 */
	public byte[] getCertEncoded() {
		return this.certEncoded;
	}
}
