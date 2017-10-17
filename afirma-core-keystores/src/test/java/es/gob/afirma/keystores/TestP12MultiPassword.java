/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.keystores.callbacks.CachePasswordCallback;

/** Pruebas de almac&eacute;n PKCS#12 con distintas conttrase&ntilde;as
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestP12MultiPassword {

	/** Prueba un almac&eacute;n PKCS#12 con distintas conttrase&ntilde;as.
	 * @throws Exception Si ocurre cualquier problema */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testPkcs12StoreWithMultiplePasswords() throws Exception {
		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

		final AOKeyStoreManager ksm = new Pkcs12KeyStoreManager();
		ksm.init(
			null,
			ClassLoader.getSystemResourceAsStream("multi_almacen.p12"), //$NON-NLS-1$
			new CachePasswordCallback("1111".toCharArray()), //$NON-NLS-1$
			null,
			false
		);

		for (int i=0;i<ksm.getAliases().length;i++) {
			if (i==2 || i==4) {
				continue;
			}
			System.out.println("Certificado: " + ksm.getAliases()[i]); //$NON-NLS-1$
			ksm.setEntryPasswordCallBack(
				new CachePasswordCallback("12341234".toCharArray()) //$NON-NLS-1$
			);
			System.out.println(
				ksm.getKeyEntry(
					ksm.getAliases()[i]
				).getClass().getName()
			);
		}
	}

}
