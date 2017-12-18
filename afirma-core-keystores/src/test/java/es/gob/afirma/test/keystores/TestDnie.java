/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.test.keystores;

import java.security.Signature;
import java.security.cert.X509Certificate;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.KeyStoreUtilities;
import es.gob.jmulticard.AOUtil;

/** Pruebas espec&iacute;ficas para el almac&eacute;n DNIe.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestDnie {

	private static final String SIGN_ALIAS = "CertFirmaDigital";  //$NON-NLS-1$
	private static final String DNIE_P11_64 = "c:\\windows\\system32\\DNIe_P11_x64.dll"; //$NON-NLS-1$
	private static final String DNIE_P11_32 = "c:\\windows\\system32\\DNIe_P11.dll"; //$NON-NLS-1$

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

    /** Prueba de carga y uso del almac&eacute;n DNIe 100% Java.
     * @throws Exception En cualquier error. */
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

}
