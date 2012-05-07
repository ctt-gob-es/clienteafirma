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

import java.awt.Component;
import java.net.URL;
import java.util.logging.Logger;

import javax.naming.ldap.LdapName;
import javax.swing.SwingWorker;

import es.gob.afirma.util.AOCertVerifier;

final class DniOcspLdapWorker extends SwingWorker<Void, String> {

	private final AOCertVerifier verifier;
	private final Component comp;

	DniOcspLdapWorker(final AOCertVerifier cv, final Component c) {
		this.verifier = cv;
		this.comp = c;
	}

	@Override
	protected Void doInBackground() {
		if (this.verifier == null) {
			return null;
		}
		try {
			this.verifier.addRootCertificatesFromLdap(
				"ldap.dnie.es",  //$NON-NLS-1$
				new LdapName("CN=AC RAIZ DNIE,OU=DNIE,O=DIRECCION GENERAL DE LA POLICIA,C=ES") //$NON-NLS-1$
			);
            this.verifier.addRootCertificatesFromLdap(
                "ldap.dnie.es",  //$NON-NLS-1$
                new LdapName("CN=AC DNIE 001,OU=DNIE,O=DIRECCION GENERAL DE LA POLICIA,C=ES") //$NON-NLS-1$
            );
            this.verifier.addRootCertificatesFromLdap(
                "ldap.dnie.es",  //$NON-NLS-1$
                new LdapName("CN=AC DNIE 002,OU=DNIE,O=DIRECCION GENERAL DE LA POLICIA,C=ES") //$NON-NLS-1$
            );
            this.verifier.addRootCertificatesFromLdap(
                "ldap.dnie.es",  //$NON-NLS-1$
                new LdapName("CN=AC DNIE 003,OU=DNIE,O=DIRECCION GENERAL DE LA POLICIA,C=ES") //$NON-NLS-1$
            );

            // Anadimos los certificados raiz de las VA desde LDAP

            this.verifier.addRootCertificatesFromLdap(
                "ldap.dnie.es",  //$NON-NLS-1$
                new LdapName("CN=AV DNIE FNMT,OU=FNMT, OU=DNIE,O=DIRECCION GENERAL DE LA POLICIA,C=ES") //$NON-NLS-1$
            );

            // Habilitamos el OCSP (DNIe SHA-1)

			AOCertVerifier.enableOCSP(
				new URL("http://ocsp.dnielectronico.es:80"),  //$NON-NLS-1$
				null,
				null,
				null
			);

			// Habilitamos el componente

			if (this.comp != null) {
				this.comp.setEnabled(true);
			}

		}
		catch(final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("No se ha podido habilitar la validacion OCSP: " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}

		return null;
	}

}
