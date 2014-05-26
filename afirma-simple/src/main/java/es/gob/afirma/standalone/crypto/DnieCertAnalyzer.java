/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.standalone.crypto;

import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import javax.naming.ldap.LdapName;
import javax.naming.ldap.Rdn;
import javax.swing.ImageIcon;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;


final class DnieCertAnalyzer extends CertAnalyzer {

	@Override
	public boolean isValidCert(final X509Certificate cert) {
		if (cert == null) {
            return false;
        }
		if (AOUtil.getCN(cert.getIssuerX500Principal().toString()).startsWith("AC DNIE") || AOUtil.getCN(cert.getIssuerX500Principal().toString()).startsWith("AC RAIZ DNIE")) { //$NON-NLS-1$ //$NON-NLS-2$
		    return true;
		}
		return false;
	}

	@Override
	public CertificateInfo analyzeCert(final X509Certificate cert) {
	    String titular = null;
	    try {
	        final LdapName dniSubject = new LdapName(cert.getSubjectX500Principal().toString());
	        String nombre = null;
	        String apellido = null;
	        for(final Rdn rdn : dniSubject.getRdns()) {
	            if (rdn.getType().equalsIgnoreCase("GIVENNAME")) { //$NON-NLS-1$
                    nombre = rdn.getValue().toString();
                }
                else if (rdn.getType().equalsIgnoreCase("SURNAME")) { //$NON-NLS-1$
                    apellido = rdn.getValue().toString();
                }
	        }
	        if (nombre != null || apellido != null) {
	            if (nombre != null) {
                    titular = nombre;
                }
	            if (apellido != null) {
	                if (titular != null) {
                        titular = titular + " " + apellido; //$NON-NLS-1$
                    }
                    else {
                        titular = apellido;
                    }
	            }
	        }
	    }
	    catch(final Exception e) {
	        Logger.getLogger("es.gob.afirma").warning("No se ha podido obtener el nombre del titular del DNIe: " + e); //$NON-NLS-1$ //$NON-NLS-2$
	    }
		return new CertificateInfo(
			cert,
			SimpleAfirmaMessages.getString("DnieCertAnalyzer.2") + (titular != null ? " " + SimpleAfirmaMessages.getString("DnieCertAnalyzer.0") + " " + titular : ""),  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
			new ImageIcon(this.getClass().getResource("/resources/dnie_cert_ico.png")),  //$NON-NLS-1$
			SimpleAfirmaMessages.getString("DnieCertAnalyzer.4") //$NON-NLS-1$
		);
	}

}
