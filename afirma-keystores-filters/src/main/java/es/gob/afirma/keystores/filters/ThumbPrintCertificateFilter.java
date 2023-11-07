/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.filters;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.keystores.CertificateFilter;

/** Filtro para identificar un certificado a partir de su <i>thumbprint</i> (tambi&eacute;n
 * conocido como <i>fingerprint</i>) codificado en hexadecimal. El thumbprint de un certificado
 * es la huella digital de su codificaci&oacute;n.
 * @author Carlos Gamuci Mill&aacute;n. */
public final class ThumbPrintCertificateFilter extends CertificateFilter {

	private final String digestAlgorithm;
	private final String thumbprint;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Crea un filtro para identificar un certificado a partir de su <i>thumbprint</i>.
	 * @param digestAlgorithm Algoritmo de huella a usar
	 * @param thumbprint Huella del certificado, obtenida con el algoritmo indicado */
	public ThumbPrintCertificateFilter(final String digestAlgorithm, final String thumbprint) {
		if (digestAlgorithm == null || thumbprint == null) {
			throw new IllegalArgumentException("Se debe indicar tanto el algoritmo como la huella digital del certificado"); //$NON-NLS-1$
		}
		this.digestAlgorithm = digestAlgorithm;
		this.thumbprint = thumbprint.replace(" ", ""); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** {@inheritDoc} */
	@Override
    public boolean matches(final X509Certificate cert) {
		try {
			return this.thumbprint.equalsIgnoreCase(
					AOUtil.hexify(MessageDigest.getInstance(this.digestAlgorithm).digest(cert.getEncoded()), "")); //$NON-NLS-1$
		}
		catch (final NoSuchAlgorithmException e) {
			LOGGER.warning("Algoritmo de huella digital no reconocido: " + e); //$NON-NLS-1$
			return false;
		}
		catch (final CertificateEncodingException e) {
			LOGGER.warning("No se ha podido obtener la codificacion del certificado: " + e); //$NON-NLS-1$
			return false;
		}
		catch (final Exception e) {
			LOGGER.warning("No se ha podido filtrar el certificado: " + e); //$NON-NLS-1$
			return false;
		}
	}
}
