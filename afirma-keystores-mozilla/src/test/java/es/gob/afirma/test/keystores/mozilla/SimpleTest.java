package es.gob.afirma.test.keystores.mozilla;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.keystores.main.common.AOKeyStore;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerFactory;

/** Pruebas simples de almacenes Mozilla NSS. */
public final class SimpleTest {

    /** Inicio de las pruebas desde consola sin JUnit.
     * @param args */
    public static void main(final String[] args) {
		try {
			new SimpleTest().testKeyStoreManagerCreation();
		}
		catch (final Exception e) {
		    System.err.println(e.toString());
		}
    }

    /** Prueba de la obtenci&oacute;n de almac&eacute;n y alias con Mozilla NSS.
     * @throws Exception */
    @SuppressWarnings("static-method")
    @Test
    @Ignore
    public void testKeyStoreManagerCreation() throws Exception {
    	final AOKeyStoreManager ksm = AOKeyStoreManagerFactory
		 .getAOKeyStoreManager(AOKeyStore.MOZ_UNI, null,
			"TEST-KEYSTORE",  //$NON-NLS-1$
			null, // PasswordCallback
			null // Parent
		);
    	final String[] aliases = ksm.getAliases();
    	for (final String alias : aliases) {
    		System.out.println(alias);
    	}
    }
}