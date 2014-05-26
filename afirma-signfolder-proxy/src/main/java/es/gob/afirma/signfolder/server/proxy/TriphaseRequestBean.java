package es.gob.afirma.signfolder.server.proxy;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;

/** Petici&oacute;n de firma trif&aacute;sica para m&uacute;ltiples documentos.
 * @author Carlos Gamuci Mill&aacute;n */
public final class TriphaseRequestBean extends ArrayList<TriphaseRequest> {

	/** Serial Id. */
	private static final long serialVersionUID = 1L;

	/** Certificado de firma. */
	private final X509Certificate cert;

	/** Construye el listado de peticiones de firma trif&aacute;sica.
	 * @param certEncoded Certificado de firma codificado en Base64.
	 * @param triphaseRequests Listado de firmas solicitadas.
	 * @throws IOException Cuando el certificado no se indica correctamente en Base 64.
	 * @throws CertificateException Cuando no se indica un certificado valido
	 */
	TriphaseRequestBean(final byte[] certEncoded,
			            final List<TriphaseRequest> triphaseRequests) throws CertificateException,
	                                                                     IOException {

		this.cert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
			new ByteArrayInputStream(certEncoded));
		
		if (triphaseRequests != null) {
			this.addAll(triphaseRequests);
		}
	}

	/** Construye el listado de peticiones de firma trif&aacute;sica.
	 * @param cert Certificado.
	 * @param triphaseRequests Listado de firmas solicitadas.
	 */
	TriphaseRequestBean(final X509Certificate cert, final List<TriphaseRequest> triphaseRequests) {
		this.cert = cert;
		if (triphaseRequests != null) {
			this.addAll(triphaseRequests);
		}
	}

	/** Recupera el certificado de firma.
	 * @return Certificado de firma. */
	public X509Certificate getCertificate() {
		return this.cert;
	}
}
