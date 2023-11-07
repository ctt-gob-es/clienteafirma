/* Copyright (C) 2023 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.filters;

import java.security.cert.X509Certificate;

/**
 * Filtro de certificados que excluye el certificado de autenticaci&oacute;n del DNIe.
 */
public final class SkipAuthDNIeFilter extends AuthenticationDNIeFilter {

	/** {@inheritDoc} */
	@Override
	public boolean matches(final X509Certificate cert) {
		return !super.matches(cert);
	}
}
