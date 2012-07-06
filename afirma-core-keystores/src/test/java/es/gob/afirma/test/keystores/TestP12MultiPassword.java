package es.gob.afirma.test.keystores;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Test;

import es.gob.afirma.keystores.dnie.pkcs12.Pkcs12KeyStoreManager;
import es.gob.afirma.keystores.main.callbacks.CachePasswordCallback;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;

/** Pruebas de almac&eacute;n PKCS#12 con distintas conttrase&ntilde;as
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestP12MultiPassword {

	/** Prueba un almac&eacute;n PKCS#12 con distintas conttrase&ntilde;as.
	 * @throws Exception SI ocurrel cualquier problema */
	@SuppressWarnings("static-method")
	@Test
	public void testPkcs12StoreWithMultiplePasswords() throws Exception {
		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

		final AOKeyStoreManager ksm = new Pkcs12KeyStoreManager();
		ksm.init(null, ClassLoader.getSystemResourceAsStream("multi_almacen.p12"), new CachePasswordCallback("1111".toCharArray()), null);  //$NON-NLS-1$//$NON-NLS-2$

		for (int i=0;i<ksm.getAliases().length;i++) {
			if (i==2 || i==4) {
				continue;
			}
			System.out.println("Certificado: " + ksm.getAliases()[i]); //$NON-NLS-1$
			System.out.println(ksm.getKeyEntry(ksm.getAliases()[i], new CachePasswordCallback("12341234".toCharArray())).getClass().getName()); //$NON-NLS-1$
		}
	}

}
