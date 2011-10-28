package es.gob.afirma.keystores.filters;

import java.security.cert.X509Certificate;

import es.gob.afirma.keystores.filters.rfc.KeyUsageFilter;
import es.gob.afirma.keystores.filters.rfc.RFC2254CertificateFilter;

/**
 * Filtro de certificados que s&oacute;lo admite el certificado de firma del DNIe.
 * @author Carlos Gamuci Mill&aacute;n.
 */
public class DNIeFilter implements CertificateFilter {

	private RFC2254CertificateFilter rfc2254Filter = null;
	private KeyUsageFilter keyUsageFilter = null;
	
	/**
	 * Contruye el filtro gen&eacute;rico para la selecci&oacute;n de los certificados de firma
	 * del DNIe.
	 */
	public DNIeFilter() {
		
		this.rfc2254Filter = new RFC2254CertificateFilter(
				"(cn=*\28FIRMA\29)", //$NON-NLS-1$
				"(&(cn=AC DNIE 00*)(ou=DNIE)(o=DIRECCION GENERAL DE LA POLICIA)(c=ES))"); //$NON-NLS-1$
		
		this.keyUsageFilter = new KeyUsageFilter(KeyUsageFilter.SIGN_CERT_USAGE);
	}
	
	@Override
	public boolean matches(X509Certificate cert) {
		return this.keyUsageFilter.matches(cert) && this.rfc2254Filter.matches(cert);
	}
}
