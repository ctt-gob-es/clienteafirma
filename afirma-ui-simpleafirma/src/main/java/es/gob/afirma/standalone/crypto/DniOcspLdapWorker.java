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
	protected Void doInBackground() throws Exception {
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
