package es.gob.afirma.standalone.crypto;

import java.net.URL;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import javax.naming.ldap.LdapName;
import javax.naming.ldap.Rdn;
import javax.swing.ImageIcon;

import es.atosorigin.AOCertVerifier;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.standalone.Messages;


class DnieCertAnalyzer extends CertAnalyzer {

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
	    System.out.println();
		return new CertificateInfo(cert, Messages.getString("DnieCertAnalyzer.2") + ((titular != null) ? (" " + Messages.getString("DnieCertAnalyzer.0") + " " + titular) : ""), getDNIeCertVerifier(), new ImageIcon(this.getClass().getResource("/resources/dnie_cert_ico.png")), Messages.getString("DnieCertAnalyzer.4")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
	}

	/**
	 * Obtiene un validador de certificados DNIe v&iacute;a OCSP.
	 * @return Validador de certificados de DNIe
	 */
	private AOCertVerifier getDNIeCertVerifier() {

		// Instanciamos la clase verificadora
		final AOCertVerifier v = new AOCertVerifier();

		// Indicamos que verifique la validez temporal del certificado
		v.setCheckValidity(true);

		// Anadimos los cerificados CA desde LDAP
		try {
			v.addRootCertificatesFromLdap(
				"ldap.dnie.es",  //$NON-NLS-1$
				new LdapName("CN=AC RAIZ DNIE,OU=DNIE,O=DIRECCION GENERAL DE LA POLICIA,C=ES") //$NON-NLS-1$
			);
		}
		catch(final Exception e) {
			Logger.getLogger("es.gob.afirma").warning("No se ha podido anadir el certificado CA raiz del DNIe: " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}
		try {
            v.addRootCertificatesFromLdap(
                "ldap.dnie.es",  //$NON-NLS-1$
                new LdapName("CN=AC DNIE 001,OU=DNIE,O=DIRECCION GENERAL DE LA POLICIA,C=ES") //$NON-NLS-1$
            );
        }
        catch(final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido anadir el certificado CA 001 del DNIe: " + e); //$NON-NLS-1$ //$NON-NLS-2$
        }
        try {
            v.addRootCertificatesFromLdap(
                "ldap.dnie.es",  //$NON-NLS-1$
                new LdapName("CN=AC DNIE 002,OU=DNIE,O=DIRECCION GENERAL DE LA POLICIA,C=ES") //$NON-NLS-1$
            );
        }
        catch(final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido anadir el certificado CA 002 del DNIe: " + e); //$NON-NLS-1$ //$NON-NLS-2$
        }
        try {
            v.addRootCertificatesFromLdap(
                "ldap.dnie.es",  //$NON-NLS-1$
                new LdapName("CN=AC DNIE 003,OU=DNIE,O=DIRECCION GENERAL DE LA POLICIA,C=ES") //$NON-NLS-1$
            );
        }
        catch(final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido anadir el certificado CA 003 del DNIe: " + e); //$NON-NLS-1$ //$NON-NLS-2$
        }

		// Anadimos los certificados raiz de las VA desde LDAP
		try {
            v.addRootCertificatesFromLdap(
                "ldap.dnie.es",  //$NON-NLS-1$
                new LdapName("CN=AV DNIE FNMT,OU=FNMT, OU=DNIE,O=DIRECCION GENERAL DE LA POLICIA,C=ES") //$NON-NLS-1$
            );
        }
        catch(final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido anadir el certificado VA FNMT raiz del DNIe: " + e); //$NON-NLS-1$ //$NON-NLS-2$
        }

		// Habilitamos el OCSP (DNIe SHA-1)
		try {
			v.enableOCSP(
				new URL("http://ocsp.dnielectronico.es:80"),  //$NON-NLS-1$
				null,
				null,
				null
			);
		}
		catch(final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("No se ha podido habilitar la validacion OCSP, la validacion puede ser incompleta: " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}

		return v;
	}

}
