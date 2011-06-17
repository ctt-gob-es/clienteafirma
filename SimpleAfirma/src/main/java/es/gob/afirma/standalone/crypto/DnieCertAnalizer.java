package es.gob.afirma.standalone.crypto;

import java.net.URL;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import javax.naming.ldap.LdapName;

import es.atosorigin.AOCertVerifier;


class DnieCertAnalizer extends CertAnalyzer {

	@Override
	public boolean isValidCert(final X509Certificate cert) {
		if (cert == null) return false;
		return false;
	}

	@Override
	public CertificateInfo analizeCert(final X509Certificate cert) {
		// TODO Auto-generated method stub
		return null;
	}
	
	/**
	 * Obtiene un validador de certificados DNIe v&iacute;a OCSP.
	 * @return Validador de certificados de DNIe
	 */
	private AOCertVerifier getDNIeCertVerifier() {
		
		// Instanciamos la clase verificadora
		AOCertVerifier v = new AOCertVerifier();
		 		
		// Indicamos que verifique la validez temporal del certificado
		v.setCheckValidity(true);
		
		// Anadimos un cerificado raiz desde LDAP (DNIe CA)
		try {
			v.addRootCertificatesFromLdap(
				"ldap.dnie.es", 
				new LdapName("CN=AC RAIZ DNIE,OU=DNIE,O=DIRECCION GENERAL DE LA POLICIA,C=ES")
			);
		}
		catch(final Exception e) {
			Logger.getLogger("es.gob.afirma").warning("No se ha podido anadir el certificado CA raiz del DNIe: " + e);
		}
		
		// Anadimos un par de certificados raiz desde disco duro (DNIe VA)
		//v.addRootCertificate(new FileInputStream(new File("c:\\AVDNIEFNMTSHA1.cer")));
		//v.addRootCertificate(new FileInputStream(new File("c:\\AVDNIEFNMTSHA2.cer")));
		
		// Habilitamos el OCSP (DNIe SHA-1)
		try {
			v.enableOCSP(
				new URL("http://ocsp.dnielectronico.es:80"), 
				new LdapName("CN=AV DNIE FNMT,OU=FNMT,OU=DNIE,O=DIRECCION GENERAL DE LA POLICIA,C=ES"),
				new LdapName("CN=AC DNIE 001,OU=DNIE,O=DIRECCION GENERAL DE LA POLICIA,C=ES"),
				"34:23:43:b8:af:dd:e6:fd:4a:16:97:3f:bc:90:cc:b3"
			);
		}
		catch(final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("No se ha podido habilitar la validacion OCSP, la validacion puede ser incompleta: " + e);
		}
		
		return v;
	}

}
