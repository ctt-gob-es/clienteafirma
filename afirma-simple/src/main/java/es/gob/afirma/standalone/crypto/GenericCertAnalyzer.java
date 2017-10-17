/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.crypto;

import java.security.cert.X509Certificate;

/** Analizador gen&eacute;rico de certificados.
 * @author Carlos Gamuci Mill&aacute;n */
public final class GenericCertAnalyzer extends CertAnalyzer {

    @Override
    public boolean isValidCert(final X509Certificate cert) {
        return true;
    }

    @Override
    public CertificateInfo analyzeCert(final X509Certificate cert) {
    	return new CertificateInfo(
    		cert,
			null,
			null,
			null
		);
    }
}
