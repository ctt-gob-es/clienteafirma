/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.miniapplet.keystores.filters;

import java.math.BigInteger;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.naming.InvalidNameException;
import javax.naming.ldap.LdapName;
import javax.naming.ldap.Rdn;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.filters.CertificateFilter;

/**
 * Filtro que selecciona los certificados con un n&uacute;mero de serie concreto. 
 * Un filtrado de este tipo devuelve com&uacute;nmente un &uacute;nico certificado.
 * Si se indica el n&uacute;mero de serie del certificado del DNIe, se devuelve el
 * certificado de firma.
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
	 * Contruye el filtro a partir del n&uacute;mero de serie en hexadecimal del certificado
	 * que deseamos obtener.
	 * @param serialNumber N&uacute;mero de serie del certificado en hexadecimal.
	 */
	public SSLFilter(final String serialNumber) {
		this.serialNumber = this.prepareSerialNumber(serialNumber);
		this.authenticationDnieCertFilter = new AuthenticationDNIeFilter();
		this.signatureDnieCertFilter = new SignatureDNIeFilter();
	}
	
	/** {@inheritDoc} */
	@Override
    public boolean matches(final X509Certificate cert) {
		return this.prepareSerialNumber(this.getCertificateSN(cert)).equalsIgnoreCase(this.serialNumber);
	}
	
	/** {@inheritDoc} */
	@Override
	public String[] matches(String[] aliases, AOKeyStoreManager ksm) {

		X509Certificate[] certs = new X509Certificate[aliases.length];
		for (int i = 0; i < aliases.length; i++) {
			certs[i] = ksm.getCertificate(aliases[i]);
		}
		
		X509Certificate cert;
		X509Certificate cert2;
		final List<String> filteredCerts = new ArrayList<String>();
		for (int i = 0; i < aliases.length; i++) {
			cert = ksm.getCertificate(aliases[i]);
			try {
				if (this.matches(cert)) {
					if (this.isAuthenticationDnieCert(cert)) {
						for (int j = 0; j < aliases.length; j++) {
							if (i != j) {
								cert2 = ksm.getCertificate(aliases[j]);				
								if (this.isSignatureDnieCert(cert2) && this.getSubjectSN(cert2) != null &&
										this.getSubjectSN(cert2).equalsIgnoreCase(this.getSubjectSN(cert)) &&
										this.getExpiredDate(cert2).equals(this.getExpiredDate(cert))) {
									filteredCerts.add(aliases[j]);
									break;
								}
							}
						}
					}
					else {
						filteredCerts.add(aliases[i]);
					}
				}
			} catch (final Exception e) {
				Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
						"Error en la verificacion del certificado '" + //$NON-NLS-1$
						cert.getSerialNumber() + "': " + e);  //$NON-NLS-1$
			}
		}
		return filteredCerts.toArray(new String[filteredCerts.size()]);
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
	 * @return N&uacute;mero de serie del subject en hexadecimal.
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
	 * Prepara un n&uacute;mero de serie en hexadecimal para que tenga
	 * un formato concreto.
	 * @param serialNumber N&uacute;mero de serie en hexadecimal.
	 * @return N&uacute;mero de serie preparado.
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
