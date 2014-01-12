/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.xadestri.client;

import java.io.File;
import java.io.FileOutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import javax.xml.crypto.dsig.DigestMethod;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;

/** Pruebas XAdES trif&aacute;sico. */
public class AOXAdESTriPhaseSignerTest {

	private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
	private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

	private static final String DATA_FILENAME = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
	private static final String SIGNATURE_FILENAME = "firma.xml"; //$NON-NLS-1$
	private static final String COSIGNATURE_FILENAME = "cofirma.xml"; //$NON-NLS-1$

	//private static final String SERVER_URL = "https://valide.redsara.es/firmaMovil/TriPhaseSignerServer/SignatureService"; //$NON-NLS-1$
	//private static final String SERVER_URL = "https://prevalide.redsara.es/firmaMovil/TriPhaseSignerServer/SignatureService"; //$NON-NLS-1$
	//private static final String SERVER_URL = "http://localhost:8080/TriPhaseSignerServer/SignatureService"; //$NON-NLS-1$
	private static final String SERVER_URL = "http://localhost:8080/afirma-server-triphase-signer/SignatureService"; //$NON-NLS-1$

	private static final Properties[] CONFIGS;


	static {
		CONFIGS = new Properties[3];

		final Properties config0 = new Properties();
		config0.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$
		CONFIGS[0] = config0;

		final Properties config1 = new Properties();
		config1.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$
		config1.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_DETACHED); //$NON-NLS-1$
		config1.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
		config1.setProperty("policyIdentifier", "urn:oid:2.16.724.1.3.1.1.2.1.8"); //$NON-NLS-1$ //$NON-NLS-2$
		config1.setProperty("policyIdentifierHash", "V8lVVNGDCPen6VELRD1Ja8HARFk=");  //$NON-NLS-1$//$NON-NLS-2$
		config1.setProperty("policyIdentifierHashAlgorithm", DigestMethod.SHA1);         //$NON-NLS-1$
		config1.setProperty("policyDescription", "Politica de firma electronica para las Administraciones Publicas en Espana"); //$NON-NLS-1$ //$NON-NLS-2$
		config1.setProperty("policyQualifier", "http://administracionelectronica.gob.es/es/ctt/politicafirma/politica_firma_AGE_v1_8.pdf"); //$NON-NLS-1$ //$NON-NLS-2$
		CONFIGS[1] = config1;

		final Properties config2 = new Properties();
		config2.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$
		config2.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING); //$NON-NLS-1$
		config2.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$
		CONFIGS[2] = config2;
	}

	/** Prueba de firma.
	 * @throws Exception */
	@SuppressWarnings("static-method")
	@Test
	public void pruebaFirmaXAdES() throws Exception {

		final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(DATA_FILENAME));

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		for (final Properties config : CONFIGS) {

			final byte[] result = signer.sign(data, AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA, pke.getPrivateKey(), pke.getCertificateChain(), config);

			//		System.out.println("Resultado:\n" + new String(result)); //$NON-NLS-1$

			final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
			final FileOutputStream fos = new FileOutputStream(tempFile);
			fos.write(result);
			fos.close();

			System.out.println("El resultado de la firma se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
		}
	}

	/** Prueba de cofirma.
	 * @throws Exception */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void pruebaCofirmaXAdES() throws Exception {

		final byte[] sign = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(SIGNATURE_FILENAME));

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final Properties config = new Properties();
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		final byte[] result = signer.cosign(sign, AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA, pke.getPrivateKey(), pke.getCertificateChain(), config);

		System.out.println("Resultado:\n" + new String(result)); //$NON-NLS-1$
	}

	/** Prueba de contrafirma.
	 * @throws Exception */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void pruebaContrafirmaXAdES() throws Exception {

		final byte[] sign = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(COSIGNATURE_FILENAME));

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final Properties config = new Properties();
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$
		config.setProperty("target", "tree"); //$NON-NLS-1$ //$NON-NLS-2$

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		final byte[] result = signer.countersign(sign, AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA, CounterSignTarget.LEAFS, null, pke.getPrivateKey(), pke.getCertificateChain(), config);

		System.out.println("Resultado:\n" + new String(result)); //$NON-NLS-1$
	}
}