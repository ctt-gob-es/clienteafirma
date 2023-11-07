/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.filters;

import java.security.cert.X509Certificate;

import es.gob.afirma.keystores.CertificateFilter;

/**
 * Filtro que muestra &uacute;nicamente los certificados preparados para firma. Esto no se
 * realiza mediante KeyUsage, sino que se muestran todos los certificados con clave privada
 * disponibles en el almac&eacute;n y retirar aquellos certificados que se conoce que no
 * son espec&iacute;ficos para firma, como el certificado de autenticaci&oacute;n del DNIe,
 * por ejemplo.
 * @author Carlos Gamuci Mill&aacute;n.
 */
public final class SigningCertificateFilter extends CertificateFilter {

	private final AuthenticationDNIeFilter authenticationDnieCertFilter;

	/**
	 * Contruye el filtro de certificados de firma.
	 */
	public SigningCertificateFilter() {
		this.authenticationDnieCertFilter = new AuthenticationDNIeFilter();
	}

	/** {@inheritDoc} */
	@Override
    public boolean matches(final X509Certificate cert) {
		return !this.authenticationDnieCertFilter.matches(cert);
	}
}
