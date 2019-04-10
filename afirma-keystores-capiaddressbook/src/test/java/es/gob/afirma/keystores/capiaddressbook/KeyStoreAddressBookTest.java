
package es.gob.afirma.keystores.capiaddressbook;

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

	/** Prueba de acceso al almac&eacute;n CAPI CA.
	 * @throws Exception Cuando no se localiza el proveedor, ocurren un problema
	 * en su carga o al listar los certificados. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita CAPI
	public void pruebaProveedorDeAccesoCA() throws Exception {

		if (System.getProperty("os.name").contains("indows")) { //$NON-NLS-1$ //$NON-NLS-2$

			Security.addProvider(new MSCAPIAddressBook());

			KeyStore ks = KeyStore.getInstance("Windows-CA"); //$NON-NLS-1$
			ks.load(null, null);

			Enumeration<String> aliases = ks.aliases();
			while(aliases.hasMoreElements()) {
				final String alias = aliases.nextElement();
				Assert.assertNotNull(alias);
				System.out.println(alias);
			}

			System.out.println();
			System.out.println();

			// Comparamos con el ROOT de SUNMSCAPI
			ks = KeyStore.getInstance("Windows-ROOT"); //$NON-NLS-1$
			ks.load(null, null);

			aliases = ks.aliases();
			while(aliases.hasMoreElements()) {
				final String alias = aliases.nextElement();
				Assert.assertNotNull(alias);
				System.out.println(alias);
			}

		}
	}
}
