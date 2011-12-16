/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either versión 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.test.keystores;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.cert.X509Certificate;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.main.common.AOKeyStore;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerFactory;

/**
 * Pruebas de AOKeyStoreFactory
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public class TestAOKeystoreFactory {
    
    /** Pruebas de AOKeyStoreFactory de los tipos sin dependencias de otros m&oacute;dulos
     * @throws Exception
     */
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
                X509Certificate cert = ksm.getCertificate(alias);
                Assert.assertNotNull(cert);
            }
        }
        
        byte[] p12file = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("ANF_PF_Activo.pfx")); //$NON-NLS-1$
        Assert.assertTrue("No se ha podido leer el P12", p12file.length > 0); //$NON-NLS-1$
        File tmpFile = File.createTempFile("temp", "afirma"); //$NON-NLS-1$ //$NON-NLS-2$
        tmpFile.deleteOnExit();
        OutputStream os = new FileOutputStream(tmpFile);
        os.write(p12file);
        os.flush();
        os.close();

        PasswordCallback pc = new PasswordCallback(">", false); //$NON-NLS-1$
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
        X509Certificate cert = ksm.getCertificate("anf usuario activo"); //$NON-NLS-1$
        Assert.assertNotNull(cert);
        
    }

}
