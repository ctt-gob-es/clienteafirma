package es.gob.afirma.keystores.filters;

import java.security.cert.X509Certificate;

import es.gob.afirma.keystores.filters.rfc.KeyUsageFilter;

public final class CipherCertificateFilter extends CertificateFilter {

	private final KeyUsageFilter keyUsageFilter;

	public CipherCertificateFilter() {
		this.keyUsageFilter = new KeyUsageFilter(AUTHENTICATION_CERT_USAGE);
	}

	/** {@inheritDoc} */
	@Override
	public boolean matches(final X509Certificate cert) {
		return this.keyUsageFilter.matches(cert);
	}

	/**
	 * Usos de clave permitidos en los certificados para la autenticaci&oacute;n de usuarios.
	 */
    private static final Boolean[] AUTHENTICATION_CERT_USAGE = {
    	Boolean.TRUE, // digitalSignature
        null, // nonRepudiation
        Boolean.TRUE, // keyEncipherment
        null, // dataEncipherment
        null, // keyAgreement
        null, // keyCertSign
        null, // cRLSign
        null, // encipherOnly
        null  // decipherOnly
    };
}
