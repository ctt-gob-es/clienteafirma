package es.gob.afirma.signfolder.proxy;

/**
 * Petici&oacute;n de firma trif&aacute;sica para m&uacute;ltiples documentos.
 * @author Carlos Gamuci Mill&aacute;n
 */
public class TriphaseRequestBean {

	/** Certificado de firma codificado en base 64. */
	private final String certEncoded;

	/** Listado de peticiones para las que se desea la firma trif&aacute;sica. */
	private final TriphaseRequest[] triphaseRequests;

	/** Construye el listado de peticiones de firma trif&aacute;sica.
	 * @param certEncoded Certificado de firma codificado en base 64.
	 * @param triphaseRequests Listado de firmas solicitadas.
	 */
	public TriphaseRequestBean(final String certEncoded, final TriphaseRequest[] triphaseRequests) {
		this.certEncoded = certEncoded;
		this.triphaseRequests = triphaseRequests;
	}

	/**
	 * Recupera el certificado de firma codificado en base 64.
	 * @return Certificado de firma codificado en base 64.
	 */
	public String getCertEncoded() {
		return this.certEncoded;
	}

	/**
	 * Recupera las peticiones de firma trif&aacute;sica realizadas.
	 * @return Listado de peticiones.
	 */
	public TriphaseRequest[] getTriphaseRequests() {
		return this.triphaseRequests;
	}
}
