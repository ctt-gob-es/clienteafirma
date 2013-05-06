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

import java.util.logging.Level;
import java.util.logging.Logger;

import junit.framework.Assert;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.keystores.main.callbacks.UIPasswordCallback;
import es.gob.afirma.keystores.main.common.AOKeyStore;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerFactory;

/** Pruebas espec&iacute;ficas para el almac&eacute;n DNIe 100% Java.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestDnieJava {

    /** Prueba de carga y uso del almac&eacute;n DNIe 100% Java.
     * @throws Exception */
	@Ignore
    @SuppressWarnings("static-method")
	@Test
    public void testDnieJava() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

        final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
    		AOKeyStore.DNIEJAVA,
    		null,
    		"Afirma-DNIe", //$NON-NLS-1$
    		new UIPasswordCallback("PIN del DNIe", null), //$NON-NLS-1$
    		null
		);
        Assert.assertNotNull(ksm);
        final String[] aliases = ksm.getAliases();
        Assert.assertNotNull(aliases);

        for (final String alias : aliases) {
        	System.out.println(alias);
        }

        for (final String alias : aliases) {
        	System.out.println(ksm.getCertificate(alias));
        }

    }

}
