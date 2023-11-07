/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import java.security.cert.X509Certificate;

import es.gob.afirma.core.keystores.KeyStoreManager;

/** Filtro que engloba m&uacute;ltiples filtros de certificados, de tal forma que
 * s&oacute;lo pasar&acute;n por el filtro aquellos que cumplan con todos los filtros
 * individualmente. */
public final class MultipleCertificateFilter extends CertificateFilter {

	/** Listado de filtros que se desean aplicar sobre los certificados. */
	private final CertificateFilter[] filters;

	/** Crea un filtro m&uacute;ltiple a partir de un listado de filtros.
	 * @param filters Listado de filtros. */
	public MultipleCertificateFilter(final CertificateFilter[] filters) {
		if (filters == null) {
			throw new IllegalArgumentException("Listado nulo de filtros de certificados"); //$NON-NLS-1$
		}
		this.filters = filters.clone();
	}

	@Override
	public boolean matches(final X509Certificate cert) {
		for (final CertificateFilter filter : this.filters) {
			if (!filter.matches(cert)) {
				return false;
			}
		}
		return true;
	}

    @Override
	public String[] matches(final String[] aliases, final KeyStoreManager ksm) {
    	String[] filteredAliases = aliases.clone();
    	for (final CertificateFilter filter : this.filters) {
    		filteredAliases = filter.matches(filteredAliases, ksm);
		}
		return filteredAliases;
    }
}
