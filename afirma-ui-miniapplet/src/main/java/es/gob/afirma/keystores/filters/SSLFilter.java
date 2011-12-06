package es.gob.afirma.keystores.filters;

import java.math.BigInteger;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import javax.naming.InvalidNameException;
import javax.naming.ldap.LdapName;
import javax.naming.ldap.Rdn;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.keystores.main.filters.CertificateFilter;

/**
 * Filtro de certificados que selecciona los certificados con un n&uacute;mero de
 * serie concreto (esto deber&iacute; devolver com&uacute;nmente un &uacute;nico certificado).s&oacute;lo admite el certificado de firma del DNIe.
 * @author Carlos Gamuci Mill&aacute;n.
 */
public final class SSLFilter extends CertificateFilter {
	
	private static final String[] SUBJECT_SN_PREFIX = new String[] {
		"serialnumber=", //$NON-NLS-1$
		"SERIALNUMBER=", //$NON-NLS-1$
		"2.5.4.5=" //$NON-NLS-1$
	};
	
	private final String serialNumber;
	private final AuthenticationDNIeFilter authenticationDnieCertFilter;
	private final SignatureDNIeFilter signatureDnieCertFilter;
	
	/**
	 * Contruye el filtro a partir del n&uacute;mero de serie del certificado que
	 * deseamos obtener.
	 * @param serialNumber N&uacute;mero de serie del certificado
	 */
	public SSLFilter(final String serialNumber) {
		this.serialNumber = this.prepareSerialNumber(serialNumber);
		this.authenticationDnieCertFilter = new AuthenticationDNIeFilter();
		this.signatureDnieCertFilter = new SignatureDNIeFilter();
	}
	
	@Override
    public boolean matches(final X509Certificate cert) {
		return this.prepareSerialNumber(this.getCertificateSN(cert)).equalsIgnoreCase(this.serialNumber);
	}
	
	@Override
    public X509Certificate[] matches(final X509Certificate[] certs) {
		
		final Set<X509Certificate> filteredCerts = new HashSet<X509Certificate>();
		for (final X509Certificate cert : certs) {
			try {
				if (this.matches(cert)) {
					if (this.isAuthenticationDnieCert(cert)) {
						for (final X509Certificate cert2 : certs) {
							if (this.isSignatureDnieCert(cert2) && this.getSubjectSN(cert2) != null &&
									this.getSubjectSN(cert2).equalsIgnoreCase(this.getSubjectSN(cert)) &&
									this.getExpiredDate(cert2).equals(this.getExpiredDate(cert))) {
								filteredCerts.add(cert2);
								break;
							}
						}
					} 
					else {
						filteredCerts.add(cert);
					}
				}
			} catch (final Exception e) {
				Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
						"Error en la verificacion del certificado '" + //$NON-NLS-1$
						cert.getSerialNumber() + "': " + e);  //$NON-NLS-1$
			}
		}
		return filteredCerts.toArray(new X509Certificate[filteredCerts.size()]);
	}
	
	private boolean isAuthenticationDnieCert(final X509Certificate cert) {
		return this.authenticationDnieCertFilter.matches(cert);
	}
	
	private boolean isSignatureDnieCert(final X509Certificate cert) {
		return this.signatureDnieCertFilter.matches(cert);
	}
	
	/**
	 * Recupera el n&uacute;mero de serie del subject de un certificado en formato hexadecimal.
	 * Los ceros ('0') a la izquierda del n&uacute;mero de serie se eliminan durante el
	 * proceso. Si el certificado no tiene n&uacute;mero de serie, devolver&aacute;
	 * {@code null}.
	 * @param cert Certificado.
	 * @return Numero de serie del subject en hexadecimal.
	 */
	private String getSubjectSN(final X509Certificate cert) {
		final String principal = cert.getSubjectX500Principal().getName();
    	final List<Rdn> rdns;
		try {
			rdns = new LdapName(principal).getRdns();
		} 
		catch (final InvalidNameException e) {
			return null;
		}
		if (rdns != null && !rdns.isEmpty()) {
			for (final Rdn rdn : rdns) {
				final String rdnText = rdn.toString();
				for (final String rdnPrefix : SUBJECT_SN_PREFIX) {
					if (rdnText.startsWith(rdnPrefix)) {
						return rdnText.substring(rdnPrefix.length()).replace("#", ""); //$NON-NLS-1$ //$NON-NLS-2$
					}
				}
			}
		}
		return null;
	}
	
	/**
	 * Recupera la fecha de expiraci&oacute;n del certificado en formato "yyyy-MM-dd".
	 * @param cert Certificado.
	 * @return Fecha de caducidad.
	 */
	private String getExpiredDate(final X509Certificate cert) {
		return new SimpleDateFormat("yyyy-MM-dd").format(cert.getNotAfter()); //$NON-NLS-1$
	}
	
	/**
	 * Recupera el n&uacute;mero de serie de un certificado en formato hexadecimal.
	 * Los ceros ('0') a la izquierda del n&uacute;mero de serie se eliminan durante el
	 * proceso. Si el certificado no tiene n&uacute;mero de serie, devolver&aacute;
	 * {@code null}.
	 * @param cert Certificado.
	 * @return Numero de serie en hexadecimal.
	 */
	private String getCertificateSN(final X509Certificate cert) {
    	return this.bigIntegerToHex(cert.getSerialNumber());
	}
	
	private String bigIntegerToHex(final BigInteger bi) {
		return AOUtil.hexify(bi.toByteArray(), ""); //$NON-NLS-1$
	}
	
	/**
	 * Prepara una numero de serie en hexadecimal para que tenga un formato
	 * @param serialNumber Numero de serie en hexadecimal.
	 * @return Numero de serie preparado.
	 */
	private String prepareSerialNumber(final String sn) {
		final String preparedSn = sn.trim().replace(" ", "").replace("#", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		int n = 0;
		while(n < preparedSn.length()) {
			if (preparedSn.charAt(n) != '0') {
				break;
			}
			n++;
		}
		return preparedSn.substring(n);
	}
}
