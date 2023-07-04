/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.test.keystores;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.Provider;
import java.security.Security;
import java.security.Signature;
import java.security.cert.X509Certificate;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.KeyStoreUtilities;
import es.gob.afirma.keystores.KeyStoreUtilities.PasswordCallbackHandler;

/** Pruebas espec&iacute;ficas para el almac&eacute;n DNIe.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestDnie {

	private static final String SIGN_ALIAS = "CertFirmaDigital";  //$NON-NLS-1$
	private static final String DNIE_P11_64 = "c:\\windows\\system32\\DNIe_P11_x64.dll"; //$NON-NLS-1$
	//private static final String DNIE_P11_32 = "c:\\windows\\system32\\DNIe_P11.dll"; //$NON-NLS-1$

    /** Prueba de carga y uso del almac&eacute;n DNIe 100% Java.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita un DNIe
    public void testDnieJava() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

        final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
    		AOKeyStore.DNIEJAVA,
    		null,
    		"Afirma-DNIe", //$NON-NLS-1$
    		AOKeyStore.DNIEJAVA.getStorePasswordCallback(null),
    		null
		);
        Assert.assertNotNull(ksm);
        final String[] aliases = ksm.getAliases();
        Assert.assertNotNull(aliases);

        for (final String alias : aliases) {
        	final X509Certificate cert = ksm.getCertificate(alias);
        	Assert.assertNotNull("No se pudo recuperar el certificado", cert); //$NON-NLS-1$
        	System.out.println(alias + ": " + AOUtil.getCN(cert)); //$NON-NLS-1$
        }

        final Signature signature = Signature.getInstance("SHA512withRSA"); //$NON-NLS-1$
        signature.initSign(ksm.getKeyEntry(SIGN_ALIAS).getPrivateKey());
        signature.update("Hola mundo!".getBytes()); //$NON-NLS-1$
        final byte[] sign = signature.sign();
        System.out.println("FIRMA: " + AOUtil.hexify(sign, true)); //$NON-NLS-1$

    }

    /** Prueba de carga y uso del almac&eacute;n DNIe en MS-CAPI.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita un DNIe
    public void testDnieCapi() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

        System.setProperty(KeyStoreUtilities.DISABLE_DNIE_NATIVE_DRIVER, "true"); //$NON-NLS-1$

        final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
    		AOKeyStore.WINDOWS,
    		null,
    		"Afirma-DNIe-CAPI", //$NON-NLS-1$
    		AOKeyStore.WINDOWS.getStorePasswordCallback(null),
    		null
		);
        Assert.assertNotNull(ksm);
        final String[] aliases = ksm.getAliases();
        Assert.assertNotNull(aliases);

        String signAlias = null;

        for (final String alias : aliases) {
        	final X509Certificate cert = ksm.getCertificate(alias);
        	Assert.assertNotNull("No se pudo recuperar el certificado", cert); //$NON-NLS-1$
        	if (cert.getSubjectX500Principal().toString().contains("(FIRMA)") && cert.getIssuerX500Principal().toString().contains("DNIE")) { //$NON-NLS-1$ //$NON-NLS-2$
        		System.out.println("Alias: " + alias); //$NON-NLS-1$
        		signAlias = alias;
        	}
        }

        Assert.assertNotNull(signAlias);

        final Signature signature = Signature.getInstance("SHA512withRSA"); //$NON-NLS-1$
        signature.initSign(ksm.getKeyEntry(signAlias).getPrivateKey());
        signature.update("Hola mundo!".getBytes()); //$NON-NLS-1$
        final byte[] sign = signature.sign();
        System.out.println("FIRMA: " + AOUtil.hexify(sign, true)); //$NON-NLS-1$

    }

	/** Main.
	 * @param args No se usa.
	 * @throws Exception En cualquier error. */
	public static void main(final String[] args) throws Exception {
		new TestDnie().testDnieJava();
	}

    /**
     * Prueba de carga y uso del DNIe a trav&eacute;s del proveedor del
     * gestor de almac&eacute;n de tipo PKCS#11 del Cliente.
     * @throws Exception En cualquier error.
     */
    @SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita un DNIe
    public void testDnie64BitsPkcs11() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

		final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
    		AOKeyStore.PKCS11,
    		DNIE_P11_64,
    		"Afirma-P11-64", //$NON-NLS-1$
    		AOKeyStore.PKCS11.getStorePasswordCallback(null),
    		null
		);
        Assert.assertNotNull(ksm);
        final String[] aliases = ksm.getAliases();
        Assert.assertNotNull(aliases);

        for (final String alias : aliases) {
        	final X509Certificate cert = ksm.getCertificate(alias);
        	Assert.assertNotNull("No se pudo recuperar el certificado", cert); //$NON-NLS-1$
        	System.out.println(alias + ": " + AOUtil.getCN(cert)); //$NON-NLS-1$
        }

        final Signature signature = Signature.getInstance("SHA512withRSA"); //$NON-NLS-1$
        signature.initSign(ksm.getKeyEntry(SIGN_ALIAS).getPrivateKey());
        signature.update("Hola mundo!".getBytes()); //$NON-NLS-1$
        final byte[] sign = signature.sign();
        System.out.println("FIRMA: " + AOUtil.hexify(sign, true)); //$NON-NLS-1$
    }

    /**
     * Prueba de carga y uso de DNIe directamente a trav&eacute;s de su PKCS#11, sin usar clases
     * del cliente.
     * @throws Exception En cualquier error.
     */
    @SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita un DNIe
    public void testRawDniePkcs11() throws Exception {

        final String p11ProviderName = "Afirma-P11-64"; //$NON-NLS-1$
        final byte[] config = KeyStoreUtilities.createPKCS11ConfigFile(DNIE_P11_64, p11ProviderName, null).getBytes();

        final Provider p = Security.getProvider("SunPKCS11"); //$NON-NLS-1$
        final File f = File.createTempFile("pkcs11_", ".cfg");  //$NON-NLS-1$//$NON-NLS-2$
        try (
        		final OutputStream fos = new FileOutputStream(f);
        		) {
        	fos.write(config);
        	fos.close();
        }

        final Method configureMethod = Provider.class.getMethod("configure", String.class); //$NON-NLS-1$
        final Provider configuredProvider = (Provider) configureMethod.invoke(p, f.getAbsolutePath());
        f.deleteOnExit();
        Security.addProvider(configuredProvider);


        final PasswordCallback storePasswordCallback = AOKeyStore.PKCS11.getStorePasswordCallback(null);

    	final PasswordCallbackHandler handler = new PasswordCallbackHandler(null, storePasswordCallback);
    	final KeyStore.CallbackHandlerProtection chp = new KeyStore.CallbackHandlerProtection(handler);

    	final KeyStore.Builder builder = KeyStore.Builder.newInstance(
    		AOKeyStore.PKCS11.getProviderName(),
			configuredProvider,
			chp
		);

    	final KeyStore ks = builder.getKeyStore();
    	ks.load(null, null);

    	final PasswordCallback certPasswordCallback = AOKeyStore.PKCS11.getCertificatePasswordCallback(null);
    	final PrivateKeyEntry keyEntry = (PrivateKeyEntry) ks.getEntry(SIGN_ALIAS, new KeyStore.PasswordProtection(certPasswordCallback.getPassword()));

    	final Signature signature = Signature.getInstance("SHA512withRSA"); //$NON-NLS-1$
    	signature.initSign(keyEntry.getPrivateKey());


//    	final Provider signProvider = signature.getProvider();
//    	System.out.println(signProvider.getName() + ": " + signProvider.getClass());

    	signature.update("Hola mundo!".getBytes()); //$NON-NLS-1$
        final byte[] sign = signature.sign();
        System.out.println("FIRMA: " + AOUtil.hexify(sign, true)); //$NON-NLS-1$

    }
}
