/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import junit.framework.Assert;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.ciphers.AOCipherConstants;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;

/** Generadoe de conjuntos completos de firmas. */
public class CipherTest {

	private static final String PLAIN_DATA_FILE = "plain"; //$NON-NLS-1$

	private static final String CIPHER_DATA_WITH_KEY_FILE = "cipher_key"; //$NON-NLS-1$

	private static final String CIPHER_DATA_WITH_PASS_FILE = "cipher_pass"; //$NON-NLS-1$

	private static final String CIPHER_AES_KEY = "41vAOZAQqNLA591mdcBwVw=="; //$NON-NLS-1$

	private static final String CIPHER_PBE_PASS = "(0NtrA5en4"; //$NON-NLS-1$


    /**
     * Genera el cifrado sim&eacute;trico de un fichero.
     */
    @SuppressWarnings("static-method")
	@Test
	@Ignore
	public void cipherFile() {

    	try {
	    	final String plainDataFile = CipherTest.getResourcePath(PLAIN_DATA_FILE);
	    	final String cipherDataFile = CipherTest.getResourcePath(CIPHER_DATA_WITH_KEY_FILE);

	    	final SignApplet applet = new SignApplet();
	    	final String cipherDataB64 = applet.getFileBase64Encoded(cipherDataFile, false);
	    	final String plainDataB64 = applet.getFileBase64Encoded(plainDataFile, false);

	    	applet.initialize();

	    	applet.setCipherAlgorithm(AOCipherAlgorithm.AES.getName());
	    	applet.setUseCipherKeyStore(false);
	    	applet.setKeyMode(AOCipherConstants.KEY_MODE_USERINPUT);
	    	Assert.assertEquals(AOCipherConstants.KEY_MODE_USERINPUT, applet.getKeyMode());
	    	applet.setKey(CIPHER_AES_KEY);
	    	Assert.assertEquals(CIPHER_AES_KEY, applet.getKey());

	    	applet.cipherFile(plainDataFile);
	    	Assert.assertEquals(applet.isError(), false);

	    	final String resultCipherDataB64 = applet.getCipherData();
	    	Assert.assertNotNull(resultCipherDataB64);
	    	Assert.assertEquals(cipherDataB64, resultCipherDataB64);

	    	Assert.assertNotNull(applet.getKey());
	    	Assert.assertEquals(CIPHER_AES_KEY, applet.getKey());

	    	applet.initialize();

	    	applet.setCipherAlgorithm(AOCipherAlgorithm.AES.getName());
	    	applet.setKeyMode(AOCipherConstants.KEY_MODE_USERINPUT);
	    	applet.setKey(CIPHER_AES_KEY);

	    	applet.setCipherData(resultCipherDataB64);

	    	applet.decipherData();
	    	Assert.assertEquals(applet.isError(), false);

	    	final String resultPlainDataB64 = applet.getPlainData();
	    	Assert.assertNotNull(resultPlainDataB64);
	    	Assert.assertEquals(plainDataB64, resultPlainDataB64);
    	}
		catch(final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}
	}

    /**
     * Cifra datos y los descifra. La clave de cifrado se genera al vuelo.
     */
    @SuppressWarnings("static-method")
	@Test
	@Ignore
	public void cipherDataGeneratedKey() {

    	try {
	    	final String plainDataFile = CipherTest.getResourcePath(PLAIN_DATA_FILE);

	    	final SignApplet applet = new SignApplet();
	    	final String plainDataB64 = applet.getFileBase64Encoded(plainDataFile, false);

	    	applet.initialize();

	    	applet.setCipherAlgorithm(AOCipherAlgorithm.AES.getName());
	    	applet.setUseCipherKeyStore(false);
	    	applet.setKeyMode(AOCipherConstants.KEY_MODE_GENERATEKEY);
	    	Assert.assertEquals(AOCipherConstants.KEY_MODE_GENERATEKEY, applet.getKeyMode());

	    	applet.setPlainData(plainDataB64);

	    	applet.cipherData();
	    	Assert.assertEquals(applet.isError(), false);

	    	final String cipherDataB64 = applet.getCipherData();
	    	Assert.assertNotNull(cipherDataB64);

	    	final String keyB64 = applet.getKey();
	    	Assert.assertNotNull(keyB64);

	    	System.out.println(keyB64);

	    	applet.initialize();

	    	applet.setCipherAlgorithm(AOCipherAlgorithm.AES.getName());
	    	applet.setKeyMode(AOCipherConstants.KEY_MODE_GENERATEKEY);

	    	applet.setCipherData(cipherDataB64);
	    	applet.setKey(keyB64);

	    	applet.decipherData();
	    	Assert.assertEquals(applet.isError(), false);

	    	final String resultPlainDataB64 = applet.getPlainData();
	    	Assert.assertNotNull(resultPlainDataB64);
	    	Assert.assertEquals(plainDataB64, resultPlainDataB64);
    	}
		catch(final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}
	}

    /**
     * Cifra datos y los descifra. La clave de cifrado se genera al vuelo.
     */
    @SuppressWarnings("static-method")
	@Test
	@Ignore
	public void cipherDataUserInput() {

    	try {
	    	final String plainDataFile = CipherTest.getResourcePath(PLAIN_DATA_FILE);
	    	final String cipherDataFile = CipherTest.getResourcePath(CIPHER_DATA_WITH_KEY_FILE);

	    	final SignApplet applet = new SignApplet();
	    	final String plainDataB64 = applet.getFileBase64Encoded(plainDataFile, false);
	    	final String cipherDataB64 = applet.getFileBase64Encoded(cipherDataFile, false);

	    	applet.initialize();

	    	applet.setCipherAlgorithm(AOCipherAlgorithm.AES.getName());
	    	applet.setUseCipherKeyStore(false);
	    	applet.setKeyMode(AOCipherConstants.KEY_MODE_USERINPUT);
	    	Assert.assertEquals(AOCipherConstants.KEY_MODE_USERINPUT, applet.getKeyMode());
	    	applet.setKey(CIPHER_AES_KEY);
	    	Assert.assertEquals(CIPHER_AES_KEY, applet.getKey());

	    	applet.setPlainData(plainDataB64);

	    	applet.cipherData();
	    	Assert.assertEquals(applet.isError(), false);

	    	final String resultCipherDataB64 = applet.getCipherData();
	    	Assert.assertNotNull(resultCipherDataB64);
	    	Assert.assertEquals(cipherDataB64, resultCipherDataB64);

	    	Assert.assertNotNull(applet.getKey());
	    	Assert.assertEquals(CIPHER_AES_KEY, applet.getKey());

	    	applet.initialize();

	    	applet.setCipherAlgorithm(AOCipherAlgorithm.AES.getName());
	    	applet.setKeyMode(AOCipherConstants.KEY_MODE_USERINPUT);

	    	applet.setCipherData(resultCipherDataB64);
	    	applet.setKey(CIPHER_AES_KEY);

	    	applet.decipherData();
	    	Assert.assertEquals(applet.isError(), false);

	    	final String resultPlainData = applet.getPlainData();
	    	Assert.assertNotNull(resultPlainData);
	    	Assert.assertEquals(plainDataB64, resultPlainData);
    	}
		catch(final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}
	}

    /**
     * Cifra datos y los descifra. La clave de cifrado se genera al vuelo.
     */
    @SuppressWarnings("static-method")
	@Test
	@Ignore
	public void cipherDataPassword() {

    	try {
	    	final String plainDataFile = CipherTest.getResourcePath(PLAIN_DATA_FILE);
	    	final String cipherDataFile = CipherTest.getResourcePath(CIPHER_DATA_WITH_PASS_FILE);

	    	final SignApplet applet = new SignApplet();
	    	final String plainDataB64 = applet.getFileBase64Encoded(plainDataFile, false);
	    	final String cipherDataB64 = applet.getFileBase64Encoded(cipherDataFile, false);

	    	applet.initialize();

	    	applet.setCipherAlgorithm(AOCipherAlgorithm.PBEWITHSHA1ANDDESEDE.getName());
	    	applet.setKeyMode(AOCipherConstants.KEY_MODE_PASSWORD);
	    	Assert.assertEquals(AOCipherConstants.KEY_MODE_PASSWORD, applet.getKeyMode());
	    	applet.setPassword(CIPHER_PBE_PASS);
	    	Assert.assertEquals(CIPHER_PBE_PASS, applet.getPassword());

	    	applet.setPlainData(plainDataB64);

	    	applet.cipherData();
	    	Assert.assertEquals(applet.isError(), false);

	    	final String resultCipherDataB64 = applet.getCipherData();
	    	Assert.assertNotNull(resultCipherDataB64);
	    	Assert.assertEquals(cipherDataB64, resultCipherDataB64);

	    	Assert.assertNotNull(applet.getPassword());
	    	Assert.assertEquals(CIPHER_PBE_PASS, applet.getPassword());

	    	applet.initialize();

	    	applet.setCipherAlgorithm(AOCipherAlgorithm.PBEWITHSHA1ANDDESEDE.getName());
	    	applet.setKeyMode(AOCipherConstants.KEY_MODE_PASSWORD);
	    	applet.setPassword(CIPHER_PBE_PASS);

	    	applet.setCipherData(resultCipherDataB64);

	    	applet.decipherData();
	    	Assert.assertEquals(applet.isError(), false);

	    	final String resultPlainDataB64 = applet.getPlainData();
	    	Assert.assertNotNull(resultPlainDataB64);
	    	Assert.assertEquals(plainDataB64, resultPlainDataB64);
    	}
		catch(final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}
	}

    private static String getResourcePath(final String filename) {
    	return CipherTest.class.getResource("/" + filename).toString().substring(6); //$NON-NLS-1$
    }
}
