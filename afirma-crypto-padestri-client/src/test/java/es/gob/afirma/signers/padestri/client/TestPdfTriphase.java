/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.padestri.client;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signers.pades.common.BadPdfPasswordException;
import es.gob.afirma.signers.pades.common.PdfExtraParams;
import es.gob.afirma.signers.pades.common.PdfIsPasswordProtectedException;

/** Pruebas de firma trif&aacute;sica. */
public class TestPdfTriphase {

	/** Nombre de la propiedad de URL del servidor de firma trif&aacute;sica. */
	private static final String PROPERTY_NAME_SIGN_SERVER_URL = "serverUrl"; //$NON-NLS-1$
	private static final String PROPERTY_VALUE_SIGN_SERVER_URL = "http://localhost:8080/afirma-server-triphase-signer/SignatureService"; //$NON-NLS-1$

	private static final String PROPERTY_NAME_DOC_ID = "documentId"; //$NON-NLS-1$

	private static final String PDF_FILENAME = "TEST_PDF.pdf"; //$NON-NLS-1$
	private static final String PDF_WITH_PASSWORD_FILENAME = "TEST_PDF_Password.pdf"; //$NON-NLS-1$

	private static final String PROPERTY_NAME_ATTACH = "attach"; //$NON-NLS-1$
	private static final String PROPERTY_NAME_ATTACH_FILENAME = "attachFileName"; //$NON-NLS-1$
	private static final String PROPERTY_NAME_ATTACH_DESCRIPTION = "attachDescription"; //$NON-NLS-1$

    private static final String CERT_PATH = "PFActivoFirSHA256.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "fisico activo prueba"; //$NON-NLS-1$

	private static final String CERT_PATH2 = "ANF PJURIDICA ACTIVO.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS2 = "12341234"; //$NON-NLS-1$
	private static final String CERT_ALIAS2 = "anf usuario activo"; //$NON-NLS-1$

	private static final String TEST_IMAGE_FILE = "splash.png"; //$NON-NLS-1$

	private PrivateKeyEntry pke;
	private PrivateKeyEntry pke2;

	private Properties serverConfig;

	private byte[] data;
	private byte[] protectedData;

	/** Carga el almac&acute;n de pruebas.
	 * @throws Exception En cualquier error. */
	@Before
	public void init() throws Exception {

		// Cargamos la referencia a la clave privada
		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		this.pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final KeyStore ks2 = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks2.load(ClassLoader.getSystemResourceAsStream(CERT_PATH2), CERT_PASS2.toCharArray());
		this.pke2 = (PrivateKeyEntry) ks2.getEntry(CERT_ALIAS2, new KeyStore.PasswordProtection(CERT_PASS2.toCharArray()));


		// Establecemos la configuracion
		this.serverConfig = new Properties();
		this.serverConfig.setProperty(PROPERTY_NAME_SIGN_SERVER_URL, PROPERTY_VALUE_SIGN_SERVER_URL);

		this.data = loadFile(PDF_FILENAME);

		this.protectedData = loadFile(PDF_WITH_PASSWORD_FILENAME);
	}

	/** Prueba de validaci&oacute;n de formato.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testFormat() throws Exception {
		Assert.assertTrue(
			new AOPDFTriPhaseSigner().isValidDataFile(
				AOUtil.getDataFromInputStream(
					TestPdfTriphase.class.getResourceAsStream("/TEST_PDF.pdf") //$NON-NLS-1$
				)
			)
		);
	}

	/** Prueba de firma trif&aacute;sica normal.
	 * @throws Exception En cualquier error. */
	@Test
	@Ignore
	public void testFirma() throws Exception {
		final AOSigner signer = new AOPDFTriPhaseSigner();

		final Properties config = new Properties();
		for (final String key : this.serverConfig.keySet().toArray(new String[this.serverConfig.size()])) {
			config.setProperty(key, this.serverConfig.getProperty(key));
		}

		final byte[] result = signer.sign(
			this.data,
			"SHA512withRSA",  //$NON-NLS-1$
			this.pke.getPrivateKey(),
			this.pke.getCertificateChain(),
			config
		);

		Assert.assertNotNull("Error durante el proceso de firma, resultado nulo", result); //$NON-NLS-1$
		Assert.assertFalse("Se recibio un codigo de error desde el servidor", new String(result).startsWith("ERR-")); //$NON-NLS-1$ //$NON-NLS-2$

		final File tempFile = savePdfTempFile(result);
		System.out.println("La firma se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/** Prueba de firma trifasica PDF con los mismos par&aacute;metros que se usan para la firma PAdES en
	 * el portafirmas del MinHAP.
	 * @throws Exception Cuando ocurre cualquier error. */
	@Test
	@Ignore
	public void testFirmaParamsPortafirmas() throws Exception {

		final AOSigner signer = new AOPDFTriPhaseSigner();

		final Properties config = new Properties();
		for (final String key : this.serverConfig.keySet().toArray(new String[this.serverConfig.size()])) {
			config.setProperty(key, this.serverConfig.getProperty(key));
		}
		config.setProperty("signatureSubFilter", "ETSI.CAdES.detached"); //$NON-NLS-1$ //$NON-NLS-2$
		config.setProperty("policyIdentifier", "2.16.724.1.3.1.1.2.1.9"); //$NON-NLS-1$ //$NON-NLS-2$
		config.setProperty("policyIdentifierHash", "G7roucf600+f03r/o0bAOQ6WAs0="); //$NON-NLS-1$ //$NON-NLS-2$
		config.setProperty("policyIdentifierHashAlgorithm", "1.3.14.3.2.26"); //$NON-NLS-1$ //$NON-NLS-2$
		config.setProperty("policyQualifier", "https://sede.060.gob.es/politica_de_firma_anexo_1.pdf"); //$NON-NLS-1$ //$NON-NLS-2$

		final byte[] result = signer.sign(
			this.data,
			"SHA512withRSA",  //$NON-NLS-1$
			this.pke.getPrivateKey(),
			this.pke.getCertificateChain(),
			config
		);

		Assert.assertNotNull("Error durante el proceso de firma, resultado nulo", result); //$NON-NLS-1$
		Assert.assertFalse("Se recibio un codigo de error desde el servidor", new String(result).startsWith("ERR-")); //$NON-NLS-1$ //$NON-NLS-2$

		final File tempFile = savePdfTempFile(result);
		System.out.println("La firma se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

	/** Prueba de firma trif&aacute;sica con adjunto en el PDF.
	 * @throws Exception En cualquier error. */
	@Test
	@Ignore
	public void firmaConAdjunto() throws Exception {
		final AOSigner signer = new AOPDFTriPhaseSigner();

		final Properties config = new Properties();
		for (final String key : this.serverConfig.keySet().toArray(new String[this.serverConfig.size()])) {
			config.setProperty(key, this.serverConfig.getProperty(key));
		}

		config.setProperty(PROPERTY_NAME_ATTACH, loadFileOnBase64(TEST_IMAGE_FILE));
		config.setProperty(PROPERTY_NAME_ATTACH_FILENAME, "splash.png"); //$NON-NLS-1$
		config.setProperty(PROPERTY_NAME_ATTACH_DESCRIPTION, "Imagen adjunta de prueba"); //$NON-NLS-1$

		final byte[] result = signer.sign(
				this.data,
				"SHA512withRSA", //$NON-NLS-1$
				this.pke.getPrivateKey(),
				this.pke.getCertificateChain(),
				config
				);

		Assert.assertNotNull("Error durante el proceso de firma, resultado nulo", result); //$NON-NLS-1$
		Assert.assertFalse("Se recibio un codigo de error desde el servidor", new String(result).startsWith("ERR-")); //$NON-NLS-1$ //$NON-NLS-2$

		final File tempFile = savePdfTempFile(result);
		System.out.println("La firma con adjunto se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

	}

	private static String loadFileOnBase64(final String filename) throws Exception {
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream(filename);
		) {
			return Base64.encode(AOUtil.getDataFromInputStream(is), true);
		}
	}

	private static byte[] loadFile(final String filename) throws Exception {
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream(filename);
		) {
			return AOUtil.getDataFromInputStream(is);
		}
	}

	/** Prueba de firma trif&aacute;sica normal.
	 * @throws Exception En cualquier error. */
	@Test
	@Ignore
	public void cofirma() throws Exception {
		final AOSigner signer = new AOPDFTriPhaseSigner();

		final Properties config = new Properties(this.serverConfig);

		for (final String key : this.serverConfig.keySet().toArray(new String[this.serverConfig.size()])) {
			config.setProperty(key, this.serverConfig.getProperty(key));
		}

		final byte[] signature = signer.sign(
				this.data,
				"SHA512withRSA",  //$NON-NLS-1$
				this.pke.getPrivateKey(),
				this.pke.getCertificateChain(),
				config
				);

		Assert.assertNotNull("Error durante el proceso de firma, resultado nulo", signature); //$NON-NLS-1$
		Assert.assertFalse("Se recibio un codigo de error desde el servidor", new String(signature).startsWith("ERR-")); //$NON-NLS-1$ //$NON-NLS-2$

		config.setProperty(PROPERTY_NAME_DOC_ID, Base64.encode(signature, true));

		final byte[] coSignature = signer.cosign(
				signature,
				"SHA512withRSA",  //$NON-NLS-1$
				this.pke2.getPrivateKey(),
				this.pke2.getCertificateChain(),
				config
				);

		Assert.assertNotNull("Error durante el proceso de firma, resultado nulo", coSignature); //$NON-NLS-1$
		Assert.assertFalse("Se recibio un codigo de error desde el servidor", new String(coSignature).startsWith("ERR-")); //$NON-NLS-1$ //$NON-NLS-2$

		final File tempFile = savePdfTempFile(coSignature);
		System.out.println("La cofirma se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}

	private static File savePdfTempFile(final byte[] content) throws IOException {
		final File tempFile = File.createTempFile("test", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(content);
		}
		return tempFile;
	}

	/**
	 * Prueba de firma trif&aacute;sica de un PDF con contrase&ntilde;a para la
	 * que no se ha indicado contrase&ntilde;a.
	 * @throws Exception En cualquier error.
	 */
	@Test
	@Ignore
	public void firmaConContrasenaSinIndicar() throws Exception {
		final AOSigner signer = new AOPDFTriPhaseSigner();

		final Properties config = new Properties();
		for (final String key : this.serverConfig.keySet().toArray(new String[this.serverConfig.size()])) {
			config.setProperty(key, this.serverConfig.getProperty(key));
		}

		try {
			signer.sign(
					this.protectedData,
					"SHA512withRSA", //$NON-NLS-1$
					this.pke.getPrivateKey(),
					this.pke.getCertificateChain(),
					config
					);

			Assert.fail("Se completo la firma cuando debio fallar"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			if (!(e instanceof PdfIsPasswordProtectedException)) {
				Assert.fail("La firma fallo con una error " + e.getClass().getName() + " cuando se esperaba " + PdfIsPasswordProtectedException.class.getName()); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
	}

	/**
	 * Prueba de firma trif&aacute;sica de un PDF con contrase&ntilde;a para la
	 * que se ha indicado una contrase&ntilde;a incorrecta.
	 * @throws Exception En cualquier error.
	 */
	@Test
	@Ignore
	public void firmaConContrasenaErronea() throws Exception {
		final AOSigner signer = new AOPDFTriPhaseSigner();

		final Properties config = new Properties();
		for (final String key : this.serverConfig.keySet().toArray(new String[this.serverConfig.size()])) {
			config.setProperty(key, this.serverConfig.getProperty(key));
		}
		config.setProperty(PdfExtraParams.OWNER_PASSWORD_STRING, "1234"); //$NON-NLS-1$

		try {
			signer.sign(
					this.protectedData,
					"SHA512withRSA", //$NON-NLS-1$
					this.pke.getPrivateKey(),
					this.pke.getCertificateChain(),
					config
					);

			Assert.fail("Se completo la firma cuando debio fallar"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			if (!(e instanceof BadPdfPasswordException)) {
				Assert.fail("La firma fallo con una error " + e.getClass().getName() + " cuando se esperaba " + BadPdfPasswordException.class.getName()); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}

	}
}
