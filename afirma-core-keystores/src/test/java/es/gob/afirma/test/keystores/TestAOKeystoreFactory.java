/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.test.keystores;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.Signature;
import java.security.cert.X509Certificate;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;

/**
 * Pruebas de AOKeyStoreFactory
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public class TestAOKeystoreFactory {

	/** Prueba directa.
	 * @param args No se usa.
	 * @throws Exception En cualquier error. */
	public static void main(final String args[]) throws Exception {
		new TestAOKeystoreFactory().testAOKeystoreFactoryCAPI();
	}

    /** Pruebas de AOKeyStoreFactory de los tipos sin dependencias de otros m&oacute;dulos
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
//	@Ignore // Solo para Windows
    public void testAOKeystoreFactoryCAPI() throws Exception {
    	Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
    	final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
			AOKeyStore.WINDOWS, // Store
			null, 				// Lib
			null, 				// Description
			AOKeyStore.WINDOWS.getStorePasswordCallback(null),
			null				// Parent
		);
    	for (final String alias : ksm.getAliases()) {
    		System.out.println(alias);
    	}
    	final PrivateKeyEntry pke = ksm.getKeyEntry(
			ksm.getAliases()[0]
		);
    	System.out.println(pke.toString());
    	final Signature s = Signature.getInstance("SHA512withRSA"); //$NON-NLS-1$
    	s.initSign(
			ksm.getKeyEntry(
				ksm.getAliases()[1]
			).getPrivateKey()
		);
    	s.update("hola".getBytes()); //$NON-NLS-1$
    	System.out.println(new String(s.sign()));

    }

    /** Pruebas de AOKeyStoreFactory de los tipos sin dependencias de otros m&oacute;dulos
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testAOKeystoreFactory() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        AOKeyStoreManager ksm;
        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(AOKeyStore.APPLE, null, null, null, null);
            Assert.assertNotNull(ksm);
            final String[] aliases = ksm.getAliases();
            Assert.assertNotNull(aliases);
            if (aliases.length > 0) {
                final String alias = aliases[0];
                final X509Certificate cert = ksm.getCertificate(alias);
                Assert.assertNotNull(cert);
            }
        }

        final byte[] p12file = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("ANF_PF_Activo.pfx")); //$NON-NLS-1$
        Assert.assertTrue("No se ha podido leer el P12", p12file.length > 0); //$NON-NLS-1$
        final File tmpFile = File.createTempFile("temp", "afirma"); //$NON-NLS-1$ //$NON-NLS-2$
        tmpFile.deleteOnExit();
        final OutputStream os = new FileOutputStream(tmpFile);
        os.write(p12file);
        os.flush();
        os.close();

        final PasswordCallback pc = new PasswordCallback(">", false); //$NON-NLS-1$
        pc.setPassword("12341234".toCharArray()); //$NON-NLS-1$

        ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
                    AOKeyStore.PKCS12,
                    tmpFile.getAbsolutePath(),
                    null,
                    pc,
                    null
        );
        Assert.assertNotNull(ksm);
        final String[] aliases = ksm.getAliases();
        Assert.assertNotNull(aliases);
        final X509Certificate cert = ksm.getCertificate("anf usuario activo"); //$NON-NLS-1$
        Assert.assertNotNull(cert);

    }

}
