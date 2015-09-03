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
import java.security.cert.X509Certificate;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;

/** Pruebas espec&iacute;ficas para los almacenes de Mac OS X.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestMacKeyChain {

    /** Prueba de carga y uso de un <i>KeyChain</i> en fichero suelto.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testStandaloneKeyChain() throws Exception {
        if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
            return;
        }
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        System.out.println(System.getProperty("java.version")); //$NON-NLS-1$
        System.out.println(System.getProperty("java.vm.vendor")); //$NON-NLS-1$
        System.out.println(System.getProperty("java.home")); //$NON-NLS-1$

        // Copiamos el KeyChain a un fichero temporal
        final File kc = File.createTempFile("test", ".keychain"); //$NON-NLS-1$ //$NON-NLS-2$
        kc.deleteOnExit();
        final OutputStream os = new FileOutputStream(kc);
        os.write(AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("test.keychain"))); //$NON-NLS-1$
        os.flush();
        os.close();

        final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
    		AOKeyStore.APPLE,
    		kc.getAbsolutePath(),
    		"Mac-Afirma", //$NON-NLS-1$
    		AOKeyStore.APPLE.getStorePasswordCallback(null),
    		null
		);
        Assert.assertNotNull(ksm);
        final String[] aliases = ksm.getAliases();
        Assert.assertNotNull(aliases);

        final PrivateKeyEntry pke = ksm.getKeyEntry(
    		"anf usuario activo" //$NON-NLS-1$
		);
        Assert.assertNotNull(pke);

        final X509Certificate cert = (X509Certificate) pke.getCertificate();
        Assert.assertNotNull(cert);

        Assert.assertEquals(
            "C=ES, OU=Clase 2 persona fisica, EMAILADDRESS=test@prueba.com, SERIALNUMBER=12345678Z, SURNAME=Usuario Activo, GIVENNAME=ANF, CN=ANF Usuario Activo",  //$NON-NLS-1$
            cert.getSubjectX500Principal().toString()
        );

        Assert.assertNotNull(pke.getPrivateKey());
        System.out.println(pke.getPrivateKey());

    }

    /** Prueba de carga y uso del <i>KayChain</i> del sistema.
     * Requiere importada en el sistema una entrada con alias "anf usuario activo" que tenga clave privada
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testSystemKeyChain() throws Exception {
        if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
            return;
        }
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

        final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(AOKeyStore.APPLE, null, "Mac-Afirma", null, null); //$NON-NLS-1$
        Assert.assertNotNull(ksm);
        final String[] aliases = ksm.getAliases();
        Assert.assertNotNull(aliases);

        final PrivateKeyEntry pke = ksm.getKeyEntry("anf usuario activo"); //$NON-NLS-1$
        Assert.assertNotNull(pke);

        final X509Certificate cert = (X509Certificate) pke.getCertificate();
        Assert.assertNotNull(cert);

        Assert.assertNotNull(
            cert.getSubjectX500Principal().toString()
        );
        System.out.println(cert.getSubjectX500Principal().toString());

        Assert.assertNotNull(pke.getPrivateKey());
        System.out.println(pke.getPrivateKey());

    }

}
