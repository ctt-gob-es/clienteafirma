/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.filters;

import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.keystores.CertificateFilter;

/**
 * Filtro para especificar un certificado concreto.
 * @author Carlos Gamuci Mill&aacute;n.
 */
public final class EncodedCertificateFilter extends CertificateFilter {

	private final String certEncoded;

	/**
	 * Construye el filtro indic&aacute;ndole cual es el &uacute;nico certificado que debe aceptar.
	 * @param certEncoded Certificado codificado en base 64.
	 */
	public EncodedCertificateFilter(final String certEncoded) {
		if (certEncoded == null) {
			throw new NullPointerException("No se ha indicado el certificado que se desea obtener"); //$NON-NLS-1$
		}
		this.certEncoded = certEncoded;
	}

	/** {@inheritDoc} */
	@Override
    public boolean matches(final X509Certificate cert) {
		try {
			return this.certEncoded.equals(Base64.encode(cert.getEncoded()));
		} catch (final CertificateEncodingException e) {
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
					"No se ha podido codificar el certificado en base 64. Se ignorara: " + e  //$NON-NLS-1$
				);
			return false;
		}
	}
}
