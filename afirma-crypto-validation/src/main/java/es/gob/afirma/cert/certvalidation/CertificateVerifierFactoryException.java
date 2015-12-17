package es.gob.afirma.cert.certvalidation;

/** No se conocen mecanismos de validacion para los certificados del emisor indicado.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CertificateVerifierFactoryException extends Exception {

	private static final long serialVersionUID = -8812915740009885947L;

	CertificateVerifierFactoryException(final String desc) {
		super(desc);
	}

	CertificateVerifierFactoryException(final String desc, final Exception e) {
		super(desc, e);
	}

}
