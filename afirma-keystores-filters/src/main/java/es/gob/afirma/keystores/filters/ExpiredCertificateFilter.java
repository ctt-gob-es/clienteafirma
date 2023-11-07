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
import java.util.Date;

import es.gob.afirma.keystores.CertificateFilter;

/**
 * Filtro para determinar aquellos que est&aacute;n caducados.
 * @author Carlos Gamuci Mill&aacute;n.
 */
public final class ExpiredCertificateFilter extends CertificateFilter {

	private boolean showExpired = false;

	/**
	 * Inicializa el filtro indicando no se muestren los certificados
	 * caducados.
	 */
	public ExpiredCertificateFilter() {
		this.showExpired = false;
	}

	/**
	 * Inicializa el filtro indicando si se deben mostrar o no los certificados
	 * caducados.
	 * @param showExpired {@code true} para mostrar los certificados caducados,
	 * {@code false} en caso contrario.
	 */
	public ExpiredCertificateFilter(final boolean showExpired) {
		this.showExpired = showExpired;
	}

	/** {@inheritDoc} */
	@Override
    public boolean matches(final X509Certificate cert) {
		if (!this.showExpired) {
			try {
				cert.checkValidity(new Date());
			}
			catch (final Exception e) {
				return false;
			}
		}
		return true;
	}
}
