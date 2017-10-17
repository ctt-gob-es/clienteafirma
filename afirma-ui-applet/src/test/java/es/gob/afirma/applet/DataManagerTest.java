/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.security.NoSuchAlgorithmException;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;

/**
 * Comprueba los m&eacute;todos de entrada y salida de datos del applet, adem&aacute;s de los
 * m&eacute;todos de utilidad para conversi&oacute;n de texto y base64.
 */
public class DataManagerTest {

	private static final String DATA_FILE = "txt"; //$NON-NLS-1$

	private static final String DATA_FILE_URI;

	private static final byte[] DATA;

	private static final String DATA_BASE64;

	private static final String DATA_HASH_BASE64;

	static {

		DATA_FILE_URI = DataManagerTest.class.getResource("/" + DATA_FILE).toString(); //$NON-NLS-1$

		byte[] data;
		try {
			final InputStream isData = DataManagerTest.class.getResourceAsStream("/" + DATA_FILE); //$NON-NLS-1$
			data = AOUtil.getDataFromInputStream(isData);
			isData.close();
		}
		catch (final IOException e) {
			data = null;
		}
		Assert.assertNotNull("No se han cargado los datos", data); //$NON-NLS-1$
		Assert.assertTrue(data != null && data.length > 0);
		DATA = data;

		DATA_BASE64 = Base64.encode(DATA);

		byte[] hash;
		try {
			hash = CryptoUtils.getMessageDigest(DATA, "SHA-1"); //$NON-NLS-1$
		} catch (final NoSuchAlgorithmException e) {
			hash = null;
		}
		DATA_HASH_BASE64 = Base64.encode(hash);
		Assert.assertNotNull("No se ha calculado el hash", DATA_HASH_BASE64); //$NON-NLS-1$

	}


	/**
	 * Comprueba que los datos introducidos y recuperamos mediante los distintos m&eacute;todos
	 * coincidan.
	 */
	@SuppressWarnings({ "static-method" })
	@Test
	public void recoverData() {

		try {

			final SignApplet applet = new SignApplet();
			String dataB64;
			String fileUri;

			applet.setFileuri(DATA_FILE_URI);
			fileUri = applet.getFileUsedPath();
			Assert.assertEquals(DATA_FILE_URI, fileUri);

			applet.setFileuri(DATA_FILE_URI);
			dataB64 = applet.getFileBase64Encoded(false);

			Assert.assertEquals(DATA_BASE64, dataB64);

			System.out.println("Ruta fichero: " + new File(DATA_FILE_URI).getAbsolutePath()); //$NON-NLS-1$
			applet.setFileuri(DATA_FILE_URI);
			dataB64 = applet.getFileBase64Encoded(false);
			Assert.assertEquals(DATA_BASE64, dataB64);

			dataB64 = applet.getFileBase64Encoded(DATA_FILE_URI, false);
			Assert.assertEquals(DATA_BASE64, dataB64);

			applet.setFileuri(DATA_FILE_URI);
			Assert.assertEquals(DATA_HASH_BASE64, applet.getFileHashBase64Encoded());

		}
		catch(final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}

	}

}
