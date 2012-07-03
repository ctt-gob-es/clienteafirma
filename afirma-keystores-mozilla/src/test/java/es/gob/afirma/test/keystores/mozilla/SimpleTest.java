package es.gob.afirma.test.keystores.mozilla;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.keystores.dnie.DnieUnifiedKeyStoreManager;
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

    /** Prueba de la obtenci&oacute;n de almac&eacute;n y alias con Mozilla NSS agreg&aacute;ndolo con el controlador DNIe 100& Java.
     * @throws Exception */
    @SuppressWarnings("static-method")
    @Test
    public void testKeyStoreManagerCreationWithDnieAggregation() throws Exception {
    	AOKeyStoreManager ksm = AOKeyStoreManagerFactory
		 .getAOKeyStoreManager(AOKeyStore.MOZ_UNI, null,
			"TEST-KEYSTORE",  //$NON-NLS-1$
			null, // PasswordCallback
			null // Parent
		);
    	ksm = new DnieUnifiedKeyStoreManager(ksm, null);
    	final String[] aliases = ksm.getAliases();
    	for (final String alias : aliases) {
    		System.out.println(alias);
    	}
    	//System.out.println(ksm.getCertificate("ANF Usuario Activo"));
    	//System.out.println(ksm.getCertificate("CertFirmaDigital"));
    	//System.out.println(ksm.getKeyEntry("CertAutenticacion", new UIPasswordCallback("PIN", null)));
    	System.out.println(ksm.getCertificateChain("CertAutenticacion")[2]); //$NON-NLS-1$
    	System.out.println(ksm.getCertificateChain("ANF Usuario Activo")[0]); //$NON-NLS-1$
    }
}