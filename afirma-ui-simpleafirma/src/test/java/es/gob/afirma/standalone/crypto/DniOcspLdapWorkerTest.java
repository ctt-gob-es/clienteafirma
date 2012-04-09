package es.gob.afirma.standalone.crypto;

import java.net.URL;

import javax.naming.ldap.LdapName;

import junit.framework.Assert;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.util.AOCertVerifier;

/**
 * Prueba la conexi&oacute;n con el LDAP de la polic&iacute;a.
 * @author Carlos Gamuci
 *
 */
public class DniOcspLdapWorkerTest {

	/**
	 * Prueba a habilitar el servicio de validaci&oacute;n de certificados del DNIe
	 * conectando con el LDAP de la Direcci&oacute;n General de la Polic&iacute;a.
	 */
	@Test
	@Ignore
	public void pruebaHabilitarServicioParaElChequeoDeCertificadosDelDNIe() {

		// Instanciamos la clase verificadora
		final AOCertVerifier verifier = new AOCertVerifier();

		// Indicamos que verifique la validez temporal del certificado
		verifier.setCheckValidity(true);

		try {
			verifier.addRootCertificatesFromLdap(
				"ldap.dnie.es",  //$NON-NLS-1$
				new LdapName("CN=AC RAIZ DNIE,OU=DNIE,O=DIRECCION GENERAL DE LA POLICIA,C=ES") //$NON-NLS-1$
			);
            verifier.addRootCertificatesFromLdap(
                "ldap.dnie.es",  //$NON-NLS-1$
                new LdapName("CN=AC DNIE 001,OU=DNIE,O=DIRECCION GENERAL DE LA POLICIA,C=ES") //$NON-NLS-1$
            );
            verifier.addRootCertificatesFromLdap(
                "ldap.dnie.es",  //$NON-NLS-1$
                new LdapName("CN=AC DNIE 002,OU=DNIE,O=DIRECCION GENERAL DE LA POLICIA,C=ES") //$NON-NLS-1$
            );
            verifier.addRootCertificatesFromLdap(
                "ldap.dnie.es",  //$NON-NLS-1$
                new LdapName("CN=AC DNIE 003,OU=DNIE,O=DIRECCION GENERAL DE LA POLICIA,C=ES") //$NON-NLS-1$
            );

            // Anadimos los certificados raiz de las VA desde LDAP

            verifier.addRootCertificatesFromLdap(
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
		} catch (final Exception e) {
			Assert.fail("Ocurrio un error al conectar con el LDAP de la policia: " + e.toString()); //$NON-NLS-1$
		}
	}
}
