package es.gob.afirma.keystores.mozilla.shared;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.keystores.mozilla.AOSecMod.ModuleName;

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

	/** Prueba de lectura de <i>pkcs11.txt</i>.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testRawPkcs11txtFunction() throws Exception {
		final byte[] pkcs11Txt = AOUtil
				.getDataFromInputStream(TestNssSharedDb.class
						.getResourceAsStream("/pkcs11.txt") //$NON-NLS-1$
				);
		Reader reader = null;
		try {
			reader = new InputStreamReader(new ByteArrayInputStream(pkcs11Txt));
			final List<ModuleName> modules = Pkcs11Txt.getModules(reader);
			Assert.assertEquals(3, modules.size());
			Assert.assertEquals(
					"DataKey SignaSURE 3600 (EXTERNAL, dkck32.dll, slot 0)", //$NON-NLS-1$
					modules.get(0).toString());
			Assert.assertEquals(
					"Netscape Software Fortezza (EXTERNAL, swft32.dll, slot 0)", //$NON-NLS-1$
					modules.get(1).toString());
			Assert.assertEquals(
					"Litronic Netsign (EXTERNAL, core32.dll, slot 0)", modules //$NON-NLS-1$
							.get(2).toString());

		} finally {
			if (reader != null) {
				reader.close();
			}
		}
	}

	/** Prueba de lectura de <i>pkcs11.txt</i>.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testRawPkcs11txtTest2() throws Exception {
		final byte[] pkcs11Txt = AOUtil
				.getDataFromInputStream(TestNssSharedDb.class
						.getResourceAsStream("/pkcs11-test2.txt") //$NON-NLS-1$
				);
		Reader reader = null;
		try {
			reader = new InputStreamReader(new ByteArrayInputStream(pkcs11Txt));
			final List<ModuleName> modules = Pkcs11Txt.getModules(reader);
			Assert.assertEquals(1, modules.size());
			Assert.assertEquals(
					"Mozilla Root Certs (EXTERNAL, /usr/lib/x86_64-linux-gnu/nss/libnssckbi.so, slot 0)", //$NON-NLS-1$
					modules.get(0).toString());

		} finally {
			if (reader != null) {
				reader.close();
			}
		}
	}


}
