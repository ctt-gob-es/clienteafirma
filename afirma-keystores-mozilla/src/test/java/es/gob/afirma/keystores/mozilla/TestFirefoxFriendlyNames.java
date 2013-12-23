/*
 * Copyright (C) 2011 [Gobierno de Espana] This file is part of
 * "Cliente @Firma". "Cliente @Firma" is free software; you can redistribute it
 * and/or modify it under the terms of: - the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the License,
 * or (at your option) any later version. - or The European Software License;
 * either version 1.1 or (at your option) any later version. Date: 11/01/11 You
 * may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.keystores.mozilla;

import java.io.IOException;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.keystores.main.AOKeyStore;
import es.gob.afirma.keystores.main.AOKeyStoreManager;
import es.gob.afirma.keystores.main.AOKeyStoreManagerException;
import es.gob.afirma.keystores.main.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.main.KeyStoreUtilities;

/**
 * Prueba la conversi&oacute;n de alias en nombres significativos en CAPI.
 *
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public class TestFirefoxFriendlyNames {

	/** Ejecuci&oacute;n de test problem&aacute;ticos en Maven desde main.
	 * @param args
	 * @throws Exception */
	public static void main(final String[] args) throws Exception {
		final TestFirefoxFriendlyNames test = new TestFirefoxFriendlyNames();
		try {
			test.testWindowsFriendlyNames();
		}
		catch (final IOException e) {
			System.err.println(e.toString() + e.getCause().toString());
		}
		catch (final AOKeyStoreManagerException e) {
			System.err.println(e.toString() + e.getCause().toString());
		}
	}

	/** Prueba la conversi&oacute;n de alias en nombres significativos en CAPI.
	 * @throws Exception */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testWindowsFriendlyNames() throws Exception {
		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
		final AOKeyStoreManager ksm = AOKeyStoreManagerFactory
		.getAOKeyStoreManager(AOKeyStore.MOZ_UNI, null, "TEST",  //$NON-NLS-1$
				null, null);

		System.out.println();
		final Map<String, String> aliases = KeyStoreUtilities
		.getAliasesByFriendlyName(ksm.getAliases(), ksm, true, // Check
				// private
				// keys
				true, // Show expired
				null  // filters
		);
		for (final String key : aliases.keySet()) {
			System.out.println(key + "\n\t" + aliases.get(key)); //$NON-NLS-1$
		}
	}


    /** El acceso al almacen de certificados de Mozilla a traves de la factoria de almacenes.
     * @throws Exception Cuando ocurre cualquier error.
     */
    @SuppressWarnings("static-method")
	@Test
	@Ignore
    public void testMozillaKeystoreFromAOKeyStoreManagerFactory() throws Exception {

    	final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
    			AOKeyStore.MOZ_UNI,
    			null,
    			"KeyStore Mozilla + DNIe", //$NON-NLS-1$
    			null,
    			null);

    	for (final String alias : ksm.getAliases()) {
    		System.out.println(alias);
    	}
    }
}