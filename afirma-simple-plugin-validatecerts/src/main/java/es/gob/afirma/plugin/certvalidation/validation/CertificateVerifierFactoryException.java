/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.plugin.certvalidation.validation;

/** No se conocen mecanismos de validacion para los certificados del emisor indicado.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CertificateVerifierFactoryException extends Exception {

	private static final long serialVersionUID = -8812915740009885947L;

	CertificateVerifierFactoryException(final String desc) {
		super(desc);
	}

	CertificateVerifierFactoryException(final String desc, final Exception e) {
		super(desc, e);
	}

}
