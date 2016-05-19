package es.gob.afirma.keystores.mozilla.shared;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.InputStreamReader;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.BoundedBufferedReader;

/** Pruebas de la configuraci&oacute;n especial de NSS compartido.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestNssSharedDb {

	/** prueba del an&aacute;lisis de "pkcs11.txt".
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testRawPkcs11Txt() throws Exception {

		final byte[] pkcs11Txt = AOUtil.getDataFromInputStream(
			TestNssSharedDb.class.getResourceAsStream("/pkcs11.txt") //$NON-NLS-1$
		);

		final BufferedReader br = new BoundedBufferedReader(
			new InputStreamReader(
				new ByteArrayInputStream(pkcs11Txt)
			),
			512, // Maximo 512 lineas
			4096 // Maximo 4KB por linea
		);
	    String line;
	    while ((line = br.readLine()) != null) {
	       System.out.println(AOUtil.getRDNvalueFromLdapName("library", line.replace(" ", ","))); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	       System.out.println(
    		   line.substring(
				   line.indexOf("name=\"") + "name=\"".length(),  //$NON-NLS-1$ //$NON-NLS-2$
				   line.indexOf(
					   '"', 
					   line.indexOf("name=\"") + "name=\"".length() //$NON-NLS-1$ //$NON-NLS-2$
				   )
			   )
		   );
	    }
	    br.close();
	}


}
