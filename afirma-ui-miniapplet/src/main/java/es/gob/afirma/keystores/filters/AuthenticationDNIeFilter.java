package es.gob.afirma.keystores.filters;

import java.security.cert.X509Certificate;

import es.gob.afirma.keystores.filters.rfc.KeyUsageFilter;
import es.gob.afirma.keystores.filters.rfc.RFC2254CertificateFilter;

/**
 * Filtro de certificados que s&oacute;lo admite el certificado de autenticaci&oacute;n del DNIe.
 * @author Carlos Gamuci Mill&aacute;n.
 */
public final class AuthenticationDNIeFilter extends CertificateFilter {

	private final RFC2254CertificateFilter rfc2254Filter;
	private final KeyUsageFilter keyUsageFilter;
	
	/**
	 * Contruye el filtro gen&eacute;rico para la selecci&oacute;n de los certificados de
	 * autenticaci&oacute;n del DNIe.
	 */
	public AuthenticationDNIeFilter() {
		this.rfc2254Filter = new RFC2254CertificateFilter(
			null,
			"(&(cn=AC DNIE *)(ou=DNIE)(o=DIRECCION GENERAL DE LA POLICIA)(c=ES))" //$NON-NLS-1$
		);
		this.keyUsageFilter = new KeyUsageFilter(AUTHENTICATION_CERT_USAGE);
	}
	
	@Override
	public boolean matches(final X509Certificate cert) {
		return this.keyUsageFilter.matches(cert) && this.rfc2254Filter.matches(cert);
	}
	
	/**
	 * Usos de clave permitidos en los certificados para la autenticaci&oacute;n de usuarios.  
	 */
    private static final Boolean[] AUTHENTICATION_CERT_USAGE = {
    	Boolean.TRUE, // digitalSignature
        null, // nonRepudiation
        null, // keyEncipherment
        null, // dataEncipherment
        null, // keyAgreement
        null, // keyCertSign
        null, // cRLSign
        null, // encipherOnly
        null  // decipherOnly
    };
}
