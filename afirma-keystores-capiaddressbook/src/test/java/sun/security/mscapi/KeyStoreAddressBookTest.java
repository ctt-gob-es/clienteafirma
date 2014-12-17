
package sun.security.mscapi;

import java.security.KeyStore;
import java.security.Security;
import java.util.Enumeration;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.keystores.capiaddressbook.MSCAPIAddressBook;

/** Clase de pruebas de acceso a la libreta de direcciones.
 * @author Carlos Gamuci. */
public class KeyStoreAddressBookTest {

	/** Prueba de acceso a la libreta a traves del proveedor MSCAPIAddressBook.
	 * @throws Exception Cuando no se localiza el proveedor, ocurren un problema
	 * en su carga o al listar los certificados. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita CAPI
	public void pruebaProveedorDeAccesoLibretaDirecciones() throws Exception {

		if (System.getProperty("os.name").contains("indows")) { //$NON-NLS-1$ //$NON-NLS-2$

			Security.addProvider(new MSCAPIAddressBook());

			final KeyStore ks = KeyStore.getInstance("Windows-ADDRESSBOOK"); //$NON-NLS-1$
			ks.load(null, null);

			final Enumeration<String> aliases = ks.aliases();
			while(aliases.hasMoreElements()) {
				final String alias = aliases.nextElement();
				Assert.assertNotNull(alias);
				System.out.println(alias);
			}
		}
	}
}
