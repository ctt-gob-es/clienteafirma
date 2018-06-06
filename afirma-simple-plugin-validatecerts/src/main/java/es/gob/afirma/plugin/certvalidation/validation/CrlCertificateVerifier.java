/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.plugin.certvalidation.validation;

import java.security.cert.X509Certificate;

/** Validador de certificados X.509v3 por verificaci&oacute;n de listas de revocaci&oacute;n
 * y de periodo de validez contra el reloj del sistema.
 * Clase cedida por <a href="http://www.yohago.com/">YoHago</a>.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CrlCertificateVerifier extends CertificateVerifier {

	@Override
	public ValidationResult verifyRevocation(final X509Certificate cert) {
		return CrlHelper.verifyCertificateCRLs(
			cert,
			this.getIssuerCert() != null ? this.getIssuerCert().getPublicKey() : null,
			null
		);
	}
}