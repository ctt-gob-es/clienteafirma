package es.gob.afirma.keystores.mozilla;

import java.io.File;

import org.junit.Test;

import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;

/** Pruebas de directorios a medida para Mozilla Firefox.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public class TestCustomDirs {

	/** Pruebas de directorio de perfiles a medida.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testCustomProfilesDir() throws Exception {

		System.setProperty(
			"es.gob.afirma.keystores.mozilla.UseEnvironmentVariables", //$NON-NLS-1$
			"true" //$NON-NLS-1$
		);

		System.setProperty(
			"AFIRMA_PROFILES_INI", //$NON-NLS-1$
			new File(TestCustomDirs.class.getResource("/profiles.ini").toURI()).getAbsolutePath() //$NON-NLS-1$
		);

		System.out.println(System.getProperty("AFIRMA_PROFILES_INI")); //$NON-NLS-1$
		System.out.println(System.getProperty("es.gob.afirma.keystores.mozilla.UseEnvironmentVariables")); //$NON-NLS-1$
		System.out.println();

    	final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
    	    AOKeyStore.MOZ_UNI, // Store
    	    null, // Lib
			"TEST-KEYSTORE", // Description //$NON-NLS-1$
			null, // PasswordCallback
			null // Parent
		);
    	final String[] aliases = ksm.getAliases();
    	for (final String alias : aliases) {
    		System.out.println(alias);
    	}

	}

}
