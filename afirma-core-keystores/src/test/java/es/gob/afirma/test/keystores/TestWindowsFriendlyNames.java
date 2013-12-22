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

import java.security.Signature;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.main.common.AOKeyStore;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.main.common.KeyStoreUtilities;

/** Prueba la conversi&oacute;n de alias en nombres significativos en CAPI.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestWindowsFriendlyNames {

	private static final AOKeyStore KEYSTORE_TYPE = AOKeyStore.WINDOWS;

    /** Prueba la conversi&oacute;n de alias en nombres significativos en CAPI.
     * @throws Exception */
    @SuppressWarnings("static-method")
	@Test
	@Ignore
    public void testWindowsFriendlyNames() throws Exception {
        if (!Platform.OS.WINDOWS.equals(Platform.getOS())) {
            return;
        }

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
        		KEYSTORE_TYPE,
                null,
               "Almacen Windows",  //$NON-NLS-1$
               null,
               null
        );

        System.out.println();
        final Map<String, String> aliases = KeyStoreUtilities.getAliasesByFriendlyName(
               ksm.getAliases(),
               ksm,
               true, // Check private keys
               true, // Show expired
               null  // filters
       );

       for (final String key : aliases.keySet()) {
           System.out.println(key + "\n\t" + aliases.get(key)); //$NON-NLS-1$
           final Signature s = Signature.getInstance("SHA512withRSA"); //$NON-NLS-1$
           s.initSign(ksm.getKeyEntry(
			   key,
			   AOKeyStore.WINDOWS.getCertificatePasswordCallback(null)
		   ).getPrivateKey());
           s.update("Hola".getBytes()); //$NON-NLS-1$
           System.out.println(AOUtil.hexify(s.sign(), true));
       }
    }

}
