/* Copyright (C) 2014 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@seap.minhap.es
 */

package es.gob.afirma.keystores.filters;

import java.security.cert.X509Certificate;
import java.util.Date;

import es.gob.afirma.keystores.filters.CertificateFilter;

/**
 * Filtro para determinar aquellos que est&aacute;n caducados.
 * @author Carlos Gamuci Mill&aacute;n.
 */
public final class ExpiredCertificateFilter extends CertificateFilter {

	/** {@inheritDoc} */
	@Override
    public boolean matches(final X509Certificate cert) {
		try {
			cert.checkValidity(new Date());
		}
		catch (final Exception e) {
			return false;
		}
		return true;
	}
}
