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

	private final String rejectReason;

	/** Usamos un espacio como valor por defecto igual que el portafirmas web. */
	private static final String DEFAULT_REASON = " "; //$NON-NLS-1$

	/**
	 * Crea la solicitud de rechazo de una lista de peticiones de firma.
	 * @param certEncoded Certificado codificado para la autenticaci&oacute;n
	 * @param ids Identificadores de las peticiones a rechazar.
	 * @param rejectReason Motivo del rechazo. Si no se indica, se establece el valor por defecto.
	 */
	public RejectRequest(final byte[] certEncoded, final List<String> ids, final String rejectReason) {
		this.certEncoded = certEncoded;
		this.addAll(ids);
		// El servicio no permite valores nulos ni vacios como motivo de rechazo
		this.rejectReason = rejectReason != null && rejectReason.length() != 0 ? rejectReason : DEFAULT_REASON;
	}

	/**
	 * Recupera el certificado codificado para la autenticaci&oacute;n de la petici&oacute;n.
	 * @return Certificado codificado.
	 */
	public byte[] getCertEncoded() {
		return this.certEncoded;
	}

	/**
	 * Recupera el motivo de rechazo de la petici&oacute;n.
	 * @return Motivo del rechazo.
	 */
	public String getRejectReason() {
		return this.rejectReason;
	}
}
