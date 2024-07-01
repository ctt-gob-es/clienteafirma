/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xadestri.client;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import javax.xml.crypto.dsig.DigestMethod;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.http.UrlHttpManagerImpl;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.xadestri.client.asic.AOXAdESASiCSTriPhaseSigner;

/** Pruebas XAdES trif&aacute;sico. */
public class TestAOXAdESTriPhaseSigner {

	private static final String CERT_PATH = "PFActivoFirSHA1.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
	private static final String CERT_ALIAS = "fisico activo prueba"; //$NON-NLS-1$

	private static final String CERT_PATH2 = "PJActivoFirSHA1.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS2 = "12341234"; //$NON-NLS-1$
	private static final String CERT_ALIAS2 = "juridico activo prueba-b12345678"; //$NON-NLS-1$

//	private static final String CERT_PATH3 = "ANCERTCCP_FIRMA.p12"; //$NON-NLS-1$
//	private static final String CERT_PASS3 = "1111"; //$NON-NLS-1$
//	private static final String CERT_ALIAS3 = "juan ejemplo espa\u00F1ol"; //$NON-NLS-1$

	private static final String CERT_PATH4 = "EIDAS_CERTIFICADO_PRUEBAS___99999999R.p12"; //$NON-NLS-1$
	private static final String CERT_PASS4 = "1234"; //$NON-NLS-1$
	private static final String CERT_ALIAS4 = "eidas_certificado_pruebas___99999999r"; //$NON-NLS-1$

	private static final String CERT_PATH_EC = "ciudadanohw_ecc_2023v1.p12"; //$NON-NLS-1$
	private static final String CERT_PASS_EC = "ciudadanohw_ecc_2023v1"; //$NON-NLS-1$
	private static final String CERT_ALIAS_EC = "manuela blanco vidal - nif:10000322z"; //$NON-NLS-1$

	private static final String DATA_FILENAME = "factura_sinFirmar.xml"; //$NON-NLS-1$
	private static final String SIGNATURE_FILENAME = "firma.xml"; //$NON-NLS-1$

	//private static final String SERVER_URL = "https://valide.redsara.es/firmaMovil/TriPhaseSignerServer/SignatureService"; //$NON-NLS-1$
	//private static final String SERVER_URL = "https://prevalide.redsara.es/firmaMovil/TriPhaseSignerServer/SignatureService"; //$NON-NLS-1$
	//private static final String SERVER_URL = "http://localhost:8080/TriPhaseSignerServer/SignatureService"; //$NON-NLS-1$
	private static final String SERVER_URL = "http://localhost:8080/afirma-server-triphase-signer/SignatureService"; //$NON-NLS-1$
	//private static final String SERVER_URL = "http://127.0.0.1:8080/triphase-signer/SignatureService"; //$NON-NLS-1$

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
		config2.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED); //$NON-NLS-1$
		config2.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
//		config2.setProperty("policyIdentifier", "http://www.facturae.es/politica_de_firma_formato_facturae/politica_de_firma_formato_facturae_v3_1.pdf"); //$NON-NLS-1$ //$NON-NLS-2$
//		config2.setProperty("policyIdentifierHash", "Ohixl6upD6av8N7pEvDABhEL6hM=");  //$NON-NLS-1$//$NON-NLS-2$
//		config2.setProperty("policyIdentifierHashAlgorithm", DigestMethod.SHA1);         //$NON-NLS-1$
//		config2.setProperty("policyDescription", "facturae31"); //$NON-NLS-1$ //$NON-NLS-2$
		config2.setProperty("signerClaimedRoles", "emisor"); //$NON-NLS-1$ //$NON-NLS-2$
		config2.setProperty("facturaeSign", "true"); //$NON-NLS-1$ //$NON-NLS-2$:P
		CONFIGS[2] = config2;

		// Desactiva la comprobacion SSL
		System.setProperty(UrlHttpManagerImpl.JAVA_PARAM_DISABLE_SSL_CHECKS, Boolean.TRUE.toString());
	}

	/**
	 * Prueba de firma XAdES-ASiC-S.
	 * @throws Exception Cuando falla la firma.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita un servidor trifasico
	public void pruebaFirmaXAdESASiCS() throws Exception {

		final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(DATA_FILENAME));

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final AOSigner signer = new AOXAdESASiCSTriPhaseSigner();

		for (final Properties config : CONFIGS) {

			final byte[] result = signer.sign(data, AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, pke.getPrivateKey(), pke.getCertificateChain(), config);

			final File tempFile = File.createTempFile("xades-asic-s-", ".zip"); //$NON-NLS-1$ //$NON-NLS-2$
			try (
				final FileOutputStream fos = new FileOutputStream(tempFile);
			) {
				fos.write(result);
			}

			System.out.println("El resultado de la firma se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
		}
	}

	/** Prueba de firma XAdES Detached de fichero grande.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void pruebaFirmaXAdESDetached() throws Exception {

		final byte[] data = AOUtil.getDataFromInputStream(
			ClassLoader.getSystemResourceAsStream("TEST_PDF_Certified.pdf") //$NON-NLS-1$
		);

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		final Properties config = new Properties();
		config.put("format", "XAdES Detached"); //$NON-NLS-1$ //$NON-NLS-2$
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final byte[] result = signer.sign(
			data,
			AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(result);
		}

		System.out.println("El resultado de la firma se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/**
	 * Prueba de cofirma XAdES Detached de fichero grande.
	 * @throws Exception Cuando falla la cofirma.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void pruebaCofirmaXAdESDetached() throws Exception {
		final byte[] signature;
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream("firma-xades-detached.xml"); //$NON-NLS-1$
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH2), CERT_PASS2.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS2, new KeyStore.PasswordProtection(CERT_PASS2.toCharArray()));

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		final Properties config = new Properties();
		config.put("format", "XAdES Detached"); //$NON-NLS-1$ //$NON-NLS-2$
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final byte[] result = signer.cosign(
			signature,
			AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(result);
		}

		System.out.println("El resultado de la cofirma XAdES Detached se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/**
	 * Prueba de contrafirma XAdES Detached.
	 * @throws Exception Cuando falla la contrafirma.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void pruebaContrafirmaXAdESDetached() throws Exception {

		final byte[] signature;
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream("firma-xades-detached.xml"); //$NON-NLS-1$
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final Properties config = new Properties();
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		final byte[] result = signer.countersign(signature, AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, CounterSignTarget.LEAFS, null, pke.getPrivateKey(), pke.getCertificateChain(), config);

		final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(result);
		}

		System.out.println("El resultado de la contrafirma XAdES Detached se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/**
	 * Prueba de contrafirma de una cofirma XAdES Detached.
	 * @throws Exception Cuando falla la contrafirma.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void pruebaContrafirmaDeCofirmaXAdESDetached() throws Exception {

		final byte[] signature;
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream("cofirma-xades-detached.xml"); //$NON-NLS-1$
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final Properties config = new Properties();
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		final byte[] result = signer.countersign(signature, AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, CounterSignTarget.LEAFS, null, pke.getPrivateKey(), pke.getCertificateChain(), config);

		final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(result);
		}

		System.out.println("El resultado de la contrafirma XAdES Detached se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/**
	 * Prueba de cofirma de contrafirma XAdES Detached.
	 * @throws Exception Cuando falla la contrafirma.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void pruebaCofirmadeContrafirmaXAdESDetached() throws Exception {

		final byte[] signature;
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream("contrafirma-xades-detached.xml"); //$NON-NLS-1$
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH2), CERT_PASS2.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS2, new KeyStore.PasswordProtection(CERT_PASS2.toCharArray()));

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		final Properties config = new Properties();
		config.put("format", "XAdES Detached"); //$NON-NLS-1$ //$NON-NLS-2$
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final byte[] result = signer.cosign(
			signature,
			AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(result);
		}

		System.out.println("El resultado de la cofirma de contrafirma XAdES Detached se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/**
	 * Prueba de contrafirmar una contrafirma XAdES Detached.
	 * @throws Exception Cuando falla la contrafirma.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void pruebaContrafirmaDeContrafirmaXAdESDetached() throws Exception {

		final byte[] signature;
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream("contrafirma-xades-detached.xml"); //$NON-NLS-1$
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH2), CERT_PASS2.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS2, new KeyStore.PasswordProtection(CERT_PASS2.toCharArray()));

		final Properties config = new Properties();
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		final byte[] result = signer.countersign(signature, AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, CounterSignTarget.TREE, null, pke.getPrivateKey(), pke.getCertificateChain(), config);

		final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(result);
		}

		System.out.println("El resultado de la contrafirma de la contrafirma XAdES Detached se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/**
	 * Prueba de firma XAdES Enveloping.
	 * @throws Exception Cuando falla la firma.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void pruebaFirmaXAdESEnveloping() throws Exception {

		final byte[] data = AOUtil.getDataFromInputStream(
			ClassLoader.getSystemResourceAsStream("TEST_PDF_Certified.pdf") //$NON-NLS-1$
		);

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		final Properties config = new Properties();
		config.put("format", "XAdES Enveloping"); //$NON-NLS-1$ //$NON-NLS-2$
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final byte[] result = signer.sign(
			data,
			AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(result);
		}

		System.out.println("El resultado de la firma se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/**
	 * Prueba de cofirma XAdES Enveloping.
	 * @throws Exception Cuando falla la cofirma.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void pruebaCofirmaXAdESEnveloping() throws Exception {

		final byte[] signature;
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream("firma-xades-enveloping.xml"); //$NON-NLS-1$
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH2), CERT_PASS2.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS2, new KeyStore.PasswordProtection(CERT_PASS2.toCharArray()));

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		final Properties config = new Properties();
		config.put("format", "XAdES Enveloping"); //$NON-NLS-1$ //$NON-NLS-2$
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final byte[] result = signer.cosign(
			signature,
			AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(result);
		}

		System.out.println("El resultado de la cofirma XAdES Enveloping se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/**
	 * Prueba de contrafirma XAdES Enveloping.
	 * @throws Exception Cuando falla la operaci&oacute;n.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void pruebaContrafirmaXAdESEnveloping() throws Exception {

		final byte[] signature;
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream("firma-xades-enveloping.xml"); //$NON-NLS-1$
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final Properties config = new Properties();
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		final byte[] result = signer.countersign(signature, AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, CounterSignTarget.LEAFS, null, pke.getPrivateKey(), pke.getCertificateChain(), config);

		final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(result);
		}

		System.out.println("El resultado de la contrafirma XAdES Enveloping se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/**
	 * Prueba de contrafirma XAdES Detached XL.
	 * @throws Exception Cuando falla la operaci&oacute;n.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void pruebaContrafirmaXAdESDetachedXL() throws Exception {

		final byte[] signature;
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream("contrafirma-xades-detached-xl.xml"); //$NON-NLS-1$
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH4), CERT_PASS4.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS4, new KeyStore.PasswordProtection(CERT_PASS4.toCharArray()));

		final Properties config = new Properties();
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		final byte[] result = signer.countersign(signature, AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, CounterSignTarget.LEAFS, null, pke.getPrivateKey(), pke.getCertificateChain(), config);

		final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(result);
		}

		System.out.println("El resultado de la contrafirma XAdES Detached XL se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/**
	 * Prueba de contrafirma de una cofirma XAdES Enveloping.
	 * @throws Exception Cuando falla la operaci&oacute;n.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void pruebaContrafirmaDeCofirmaXAdESEnveloping() throws Exception {

		final byte[] signature;
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream("cofirma-xades-enveloping.xml"); //$NON-NLS-1$
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final Properties config = new Properties();
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		final byte[] result = signer.countersign(signature, AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, CounterSignTarget.LEAFS, null, pke.getPrivateKey(), pke.getCertificateChain(), config);

		final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(result);
		}

		System.out.println("El resultado de la contrafirma XAdES Enveloping se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

//	/** Prueba de contrafirma XAdES sobre una firma generada por la IGAE.
//	 * @throws Exception */
//	@SuppressWarnings("static-method")
//	@Test
//	public void pruebaContrafirmaXAdESSobreFirmaIGAE() throws Exception {
//		final InputStream is = ClassLoader.getSystemResourceAsStream("IgaeXadesSignature.xml"); //$NON-NLS-1$
//
//		final byte[] signature = AOUtil.getDataFromInputStream(is);
//
//		is.close();
//		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
//		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
//		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
//
//		final Properties config = new Properties();
//		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$
//
//		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();
//
//		final byte[] result = signer.countersign(signature, AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, CounterSignTarget.LEAFS, null, pke.getPrivateKey(), pke.getCertificateChain(), config);
//
//		final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
//		final FileOutputStream fos = new FileOutputStream(tempFile);
//		fos.write(result);
//		fos.close();
//
//		System.out.println("El resultado de la contrafirma XAdES de una firma de la IGAE se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
//	}
//
//	/** Prueba de contrafirma XAdES sobre una contrafirma generada por la IGAE.
//	 * @throws Exception */
//	@SuppressWarnings("static-method")
//	@Test
//	public void pruebaContrafirmaXAdESSobreContraFirmaIGAE() throws Exception {
//		final InputStream is = ClassLoader.getSystemResourceAsStream("IgaeXadesCounterSignature.xml"); //$NON-NLS-1$
//
//		final byte[] signature = AOUtil.getDataFromInputStream(is);
//
//		is.close();
//		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
//		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
//		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
//
//		final Properties config = new Properties();
//		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$
//
//		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();
//
//		final byte[] result = signer.countersign(signature, AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, CounterSignTarget.LEAFS, null, pke.getPrivateKey(), pke.getCertificateChain(), config);
//
//		final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
//		final FileOutputStream fos = new FileOutputStream(tempFile);
//		fos.write(result);
//		fos.close();
//
//		System.out.println("El resultado de la contrafirma XAdES de una contrafirma de la IGAE se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
//	}


	/**
	 * Prueba de cofirma de contrafirma XAdES Enveloping.
	 * @throws Exception Cuando falla la operaci&oacute;n.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void pruebaCofirmadeContrafirmaXAdESEnveloping() throws Exception {

		final byte[] signature;
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream("contrafirma-xades-enveloping.xml"); //$NON-NLS-1$
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH2), CERT_PASS2.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS2, new KeyStore.PasswordProtection(CERT_PASS2.toCharArray()));

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		final Properties config = new Properties();
		config.put("format", "XAdES Enveloping"); //$NON-NLS-1$ //$NON-NLS-2$
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final byte[] result = signer.cosign(
			signature,
			AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(result);
		}

		System.out.println("El resultado de la cofirma de contrafirma XAdES Enveloping se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/**
	 * Prueba de contrafirmar una contrafirma XAdES Enveloping.
	 * @throws Exception Cuando falla la operaci&oacute;n.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void pruebaContrafirmaDeContrafirmaXAdESEnveloping() throws Exception {

		final byte[] signature;
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream("contrafirma-xades-enveloping.xml"); //$NON-NLS-1$
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH2), CERT_PASS2.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS2, new KeyStore.PasswordProtection(CERT_PASS2.toCharArray()));

		final Properties config = new Properties();
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		final byte[] result = signer.countersign(signature, AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, CounterSignTarget.TREE, null, pke.getPrivateKey(), pke.getCertificateChain(), config);

		final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(result);
		}

		System.out.println("El resultado de la contrafirma de la contrafirma XAdES Enveloping se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/**
	 * Prueba de firma XAdES Enveloped.
	 * @throws Exception Cuando falla la operaci&oacute;n.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void pruebaFirmaXAdESEnveloped() throws Exception {

		final byte[] data = AOUtil.getDataFromInputStream(
			ClassLoader.getSystemResourceAsStream("xml_with_ids.xml") //$NON-NLS-1$
		);

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		final Properties config = new Properties();
		config.put("format", "XAdES Enveloped"); //$NON-NLS-1$ //$NON-NLS-2$
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final byte[] result = signer.sign(
			data,
			AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(result);
		}

		System.out.println("El resultado de la firma XAdES Enveloped se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/**
	 * Prueba de cofirma XAdES Enveloped.
	 * @throws Exception Cuando falla la operaci&oacute;n.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void pruebaCofirmaXAdESEnveloped() throws Exception {

		final byte[] signature;
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream("firma-xades-enveloped.xml"); //$NON-NLS-1$
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH2), CERT_PASS2.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS2, new KeyStore.PasswordProtection(CERT_PASS2.toCharArray()));

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		final Properties config = new Properties();
		config.put("format", "XAdES Enveloped"); //$NON-NLS-1$ //$NON-NLS-2$
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final byte[] result = signer.cosign(
			signature,
			AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(result);
		}

		System.out.println("El resultado de la cofirma XAdES Enveloped se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/**
	 * Prueba de contrafirma XAdES Enveloped.
	 * @throws Exception Cuando falla la contrafirma.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void pruebaContrafirmaXAdESEnveloped() throws Exception {

		final byte[] signature;
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream("firma-xades-enveloped.xml"); //$NON-NLS-1$
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final Properties config = new Properties();
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		final byte[] result = signer.countersign(signature, AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, CounterSignTarget.LEAFS, null, pke.getPrivateKey(), pke.getCertificateChain(), config);

		final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(result);
		}

		System.out.println("El resultado de la contrafirma XAdES Enveloped se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/**
	 * Prueba de contrafirma de cofirma XAdES Enveloped.
	 * @throws Exception Cuando falla la contrafirma.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void pruebaContrafirmaDeCofirmaXAdESEnveloped() throws Exception {

		final byte[] signature;
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream("cofirma-xades-enveloped.xml"); //$NON-NLS-1$
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final Properties config = new Properties();
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		final byte[] result = signer.countersign(signature, AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, CounterSignTarget.LEAFS, null, pke.getPrivateKey(), pke.getCertificateChain(), config);

		final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(result);
		}

		System.out.println("El resultado de la contrafirma XAdES Enveloped se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/**
	 * Prueba de cofirma de contrafirma XAdES Enveloped.
	 * @throws Exception Cuando falla la contrafirma.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void pruebaCofirmadeContrafirmaXAdESEnveloped() throws Exception {

		final byte[] signature;
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream("contrafirma-xades-enveloped.xml"); //$NON-NLS-1$
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH2), CERT_PASS2.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS2, new KeyStore.PasswordProtection(CERT_PASS2.toCharArray()));

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		final Properties config = new Properties();
		config.put("format", "XAdES Enveloped"); //$NON-NLS-1$ //$NON-NLS-2$
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final byte[] result = signer.cosign(
			signature,
			AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config
		);

		final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(result);
		}

		System.out.println("El resultado de la cofirma de contrafirma XAdES Enveloped se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/**
	 * Prueba de contrafirmar una contrafirma XAdES Enveloped.
	 * @throws Exception Cuando falla la contrafirma.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void pruebaContrafirmaDeContrafirmaXAdESEnveloped() throws Exception {

		final byte[] signature;
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream("contrafirma-xades-enveloped.xml"); //$NON-NLS-1$
		) {
			signature = AOUtil.getDataFromInputStream(is);
		}

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH2), CERT_PASS2.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS2, new KeyStore.PasswordProtection(CERT_PASS2.toCharArray()));

		final Properties config = new Properties();
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		final byte[] result = signer.countersign(signature, AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, CounterSignTarget.TREE, null, pke.getPrivateKey(), pke.getCertificateChain(), config);

		final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(result);
		}

		System.out.println("El resultado de la contrafirma de la contrafirma XAdES Enveloped se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/**
	 * Prueba de firma XAdES.
	 * @throws Exception Cuando falla la firma.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita un servidor trifasico
	public void pruebaFirmaXAdES() throws Exception {

		final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(DATA_FILENAME));

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		for (final Properties config : CONFIGS) {

			final byte[] result = signer.sign(data, AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, pke.getPrivateKey(), pke.getCertificateChain(), config);

			final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
			try (
				final OutputStream fos = new FileOutputStream(tempFile);
			) {
				fos.write(result);
			}

			System.out.println("El resultado de la firma se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
		}
	}

	/**
	 * Prueba de firma XAdES con certificado de curva eliptica.
	 * @throws Exception Cuando falla la firma.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita un servidor trifasico
	public void pruebaFirmaXAdES_ECDSA() throws Exception {

		System.out.println("Java version: " + System.getProperty("java.version"));

		byte[] data;
		try (InputStream fis = new FileInputStream("C:\\Entrada\\autofirma_config.afconfig")) {
			data = AOUtil.getDataFromInputStream(fis);
		}

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH_EC), CERT_PASS_EC.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS_EC, new KeyStore.PasswordProtection(CERT_PASS_EC.toCharArray()));

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		//for (final Properties config : CONFIGS) {

		final Properties config = CONFIGS[0];

			final byte[] result = signer.sign(data, AOSignConstants.SIGN_ALGORITHM_SHA256WITHECDSA, pke.getPrivateKey(), pke.getCertificateChain(), config);

			final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
			try (
				final OutputStream fos = new FileOutputStream(tempFile);
			) {
				fos.write(result);
			}

			System.out.println("El resultado de la firma se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
		//}
	}

	/**
	 * Prueba de cofirma con MANIFEST.
	 * @throws Exception Cuando falla la operaci&oacute;n.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita un servidor trifasico
	public void pruebaCofirmaXAdESManifest() throws Exception {

		final byte[] sign = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(SIGNATURE_FILENAME));

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final Properties config = new Properties();
		config.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$
		config.setProperty("useManifest", "true"); //$NON-NLS-1$ //$NON-NLS-2$

		final AOXAdESTriPhaseSigner signer = new AOXAdESTriPhaseSigner();

		final byte[] result = signer.cosign(sign, AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, pke.getPrivateKey(), pke.getCertificateChain(), config);

		final File tempFile = File.createTempFile("xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(result);
		}

		System.out.println("El resultado de la cofirma se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}
}