/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.security.cert.CertStore;
import java.security.cert.Certificate;
import java.security.cert.LDAPCertStoreParameters;
import java.security.cert.X509CertSelector;
import java.security.cert.X509Certificate;

/**
 * Funciones para el manejo de certificados en servidores LDAP.
 */
public final class LdapUtils {

	private LdapUtils() {
		// No permitimos la instanciacion
	}

    /**
     * Recupera un certificado de un LDAP.
     * @param server Servidor LDAP.
     * @param port Puerto del servidor.
     * @param principal Principal del certificado a recuperar.
     * @return Certificado recuperado.
     * @throws Exception Cuando ocurre alg&uacute;n error durante la recuperaci&oacute;n.
     */
    public static X509Certificate getCertificate(final String server, final int port, final String principal) throws Exception {
        if (server == null || server.length() == 0) {
			throw new NullPointerException("El servidor no puede ser nulo ni vacio"); //$NON-NLS-1$
		}
        if (principal == null || principal.length() == 0) {
			throw new NullPointerException("El Principal del Certificado no puede ser nulo ni vacio"); //$NON-NLS-1$
		}

        final X509CertSelector certSelector = new X509CertSelector();
        certSelector.setSubject(principal);

        return (X509Certificate) CertStore.getInstance("LDAP", new LDAPCertStoreParameters(server, port)) //$NON-NLS-1$
                .getCertificates(certSelector).toArray(new Certificate[0])[0]; }
}
