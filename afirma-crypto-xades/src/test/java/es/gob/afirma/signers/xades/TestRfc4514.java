package es.gob.afirma.signers.xades;

import org.junit.Test;

/** Pruebas de codificaci&oacute;n de nombres X.500 acordes a la RFC 4514.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestRfc4514 {

	@Test
	public void testEscapeHelper() {
		System.out.println(
			EscapeHelper.escapeLdapName("O=Atos, OU=Consultoria, CN=Tomas")
		);
	}

}
