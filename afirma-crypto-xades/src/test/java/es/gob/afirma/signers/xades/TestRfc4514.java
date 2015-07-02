package es.gob.afirma.signers.xades;

import org.junit.Test;

/** Pruebas de codificaci&oacute;n de nombres X.500 acordes a la RFC 4514.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestRfc4514 {

	/** Prueba de adecuaci&oacute;n de la codificaci&oacute;n de nombres X.500 acordes a la RFC 4514. */
	@SuppressWarnings("static-method")
	@Test
	public void testEscapeHelper() {
		System.out.println(
			EscapeHelper.escapeLdapName("O=Atos, OU=Consultoria, CN=\"Tomas, el grande\"") //$NON-NLS-1$
		);
		System.out.println(
			EscapeHelper.escapeLdapName("CN=COLEGIO DE REGISTRADORES DE LA PROPIEDAD MERCANTILES Y BIENES MUEBLES DE ESPA\u00F1A,2.5.4.5=#1309513238363330313247,O=COLEGIO DE REGISTRADORES DE LA PROPIEDAD MERCANTILES Y BIENES MUEBLES DE ESPA\u00F1A,L=MADRID,C=ES") //$NON-NLS-1$
		);
	}

}
