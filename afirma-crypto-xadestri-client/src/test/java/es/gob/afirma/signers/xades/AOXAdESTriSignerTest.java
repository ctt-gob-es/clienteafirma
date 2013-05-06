/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.xades;

import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;

public class AOXAdESTriSignerTest {

	private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
	private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

	private static final String DATA_FILENAME = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
	private static final String SIGNATURE_FILENAME = "firma.xml"; //$NON-NLS-1$

	private static final String SERVER_URL = "http://172.24.22.235:8080/SignFolderMobileProxy/SignatureService"; //$NON-NLS-1$

	@Test
	public void pruebaFirmaXAdES() throws Exception {

		final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(DATA_FILENAME));

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final Properties config = new Properties();
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final AOXAdESTriSigner signer = new AOXAdESTriSigner();

		final byte[] result = signer.sign(data, AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA, pke.getPrivateKey(), pke.getCertificateChain(), config);

		System.out.println("Resultado:\n" + new String(result));
	}

	@Test
	public void pruebaCofirmaXAdES() throws Exception {

		final byte[] sign = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(SIGNATURE_FILENAME));

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final Properties config = new Properties();
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final AOXAdESTriSigner signer = new AOXAdESTriSigner();

		final byte[] result = signer.cosign(sign, AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA, pke.getPrivateKey(), pke.getCertificateChain(), config);

		System.out.println("Resultado:\n" + new String(result));
	}
}