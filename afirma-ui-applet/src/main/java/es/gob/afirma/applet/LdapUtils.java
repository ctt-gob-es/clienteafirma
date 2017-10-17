/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.io.IOException;
import java.security.InvalidAlgorithmParameterException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertStore;
import java.security.cert.CertStoreException;
import java.security.cert.Certificate;
import java.security.cert.LDAPCertStoreParameters;
import java.security.cert.X509CertSelector;
import java.security.cert.X509Certificate;

/**
 * Funciones para el manejo de certificados en servidores LDAP.
 */
final class LdapUtils {

	private LdapUtils() {
		// No permitimos la instanciacion
	}

    /**
     * Recupera un certificado de un LDAP.
     * @param server Servidor LDAP.
     * @param port Puerto del servidor.
     * @param principal Principal del certificado a recuperar.
     * @return Certificado recuperado.
     * @throws IOException Si el formato del principal no es correcto.
     * @throws NoSuchAlgorithmException Si no se puede obtener el certificado a partir del <i>Principal</i>
     * @throws InvalidAlgorithmParameterException Si falla la inicalizaci&oacute;n del <code>CertStore</code>
     * @throws CertStoreException Si el sistema no soporta los <code>CertStore</code> de tipo LDAP
     */
    static X509Certificate getCertificate(final String server, final int port, final String principal) throws IOException, CertStoreException, InvalidAlgorithmParameterException, NoSuchAlgorithmException {
        if (server == null || server.length() == 0) {
			throw new IllegalArgumentException("El servidor no puede ser nulo ni vacio"); //$NON-NLS-1$
		}
        if (principal == null || principal.length() == 0) {
			throw new IllegalArgumentException("El Principal del Certificado no puede ser nulo ni vacio"); //$NON-NLS-1$
		}

        final X509CertSelector certSelector = new X509CertSelector();
        certSelector.setSubject(principal);

        return (X509Certificate) CertStore.getInstance("LDAP", new LDAPCertStoreParameters(server, port)) //$NON-NLS-1$
                .getCertificates(certSelector).toArray(new Certificate[0])[0]; }
}
