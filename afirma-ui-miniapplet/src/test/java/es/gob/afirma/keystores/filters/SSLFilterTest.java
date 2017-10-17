/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.filters;

import java.io.File;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.callbacks.CachePasswordCallback;
import es.gob.afirma.keystores.filters.SSLFilter;

/**
 * Prueba el filtrado de certificado a partir de un numero de serie en hexadecimal. No prueba la
 * condicion de, si se indica el numero de serie del certificado de autenticacion de un DNIe
 * se seleccione en lugar el certificado de firma de ese DNIe.
 */
public class SSLFilterTest {

	private static final AOKeyStore KEYSTORE_TYPE = AOKeyStore.PKCS12;
    private static final String CERT_PATH = "src" + File.separator + "test" + File.separator + "resources" + File.separator + "ANF_PF_Activo.pfx"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_SN = "03 ea"; //$NON-NLS-1$

    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

	/** Comprobaci&oacute;n del filtrado.
	 * @throws Exception Cuando ocurre alg&uacute;n error. */
	@SuppressWarnings("static-method")
	@Test
	public void match() throws Exception {

		final SSLFilter filter = new SSLFilter(CERT_SN);

		final String path = CERT_PATH == null ? null : new File(CERT_PATH).getAbsolutePath();

		System.out.println("Ruta del almacen: " + path); //$NON-NLS-1$

		final AOKeyStoreManager manager =
			AOKeyStoreManagerFactory.getAOKeyStoreManager(
					KEYSTORE_TYPE,
					path,
					"TEST", //$NON-NLS-1$
					new CachePasswordCallback(CERT_PASS.toCharArray()),
					null);

		System.out.println("Alias filtrados:"); //$NON-NLS-1$
		for (final String alias : filter.matches(manager.getAliases(), manager)) {
			System.out.println(alias);
			Assert.assertEquals("El certificado recuperado no es el esperado", CERT_ALIAS, alias); //$NON-NLS-1$
		}
	}
}
