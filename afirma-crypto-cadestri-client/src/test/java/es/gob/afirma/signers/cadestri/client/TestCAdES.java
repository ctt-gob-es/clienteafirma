/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.cadestri.client;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;

/** Pruebas del m&oacute;dulo CAdES de Afirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestCAdES {

	private static final String SERVER_URL = "http://localhost:8080/afirma-server-triphase-signer/SignatureService"; //$NON-NLS-1$

	private static final String CERT_PATH = "ANF PFISICA ACTIVO.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
	private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

	private static final String CERT_PATH3 = "ANF PJURIDICA ACTIVO.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS3 = "12341234"; //$NON-NLS-1$
	private static final String CERT_ALIAS3 = "anf usuario activo"; //$NON-NLS-1$

	private static final String[] DATA_FILES = {
		"ANF PFISICA ACTIVO.pfx" //$NON-NLS-1$
	};

	private static final List<byte[]> DATA = new ArrayList<>(2);
	static {
		for (final String dataFile : DATA_FILES) {
			try (InputStream is = ClassLoader.getSystemResourceAsStream(dataFile)) {
				DATA.add(AOUtil.getDataFromInputStream(is));
			}
			catch (final IOException e) {
				Logger.getLogger("es.gob.afirma").severe("No se ha podido cargar el fichero de pruebas '" + dataFile + "': " + e);  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
				DATA.add(null);
			}
		}
	}

	private static final Properties[] CADES_MODES;

	static {
		final Properties p1 = new Properties();
		p1.setProperty("format", AOSignConstants.SIGN_FORMAT_CADES); //$NON-NLS-1$
		p1.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
		p1.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final Properties p2 = new Properties();
		p2.setProperty("format", AOSignConstants.SIGN_FORMAT_CADES); //$NON-NLS-1$
		p2.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
		p2.setProperty("policyIdentifier", "urn:oid:2.16.724.1.3.1.1.2.1.8"); //$NON-NLS-1$ //$NON-NLS-2$
		p2.setProperty("policyIdentifierHash", "7SxX3erFuH31TvAw9LZ70N7p1vA="); //$NON-NLS-1$ //$NON-NLS-2$
		p2.setProperty("policyIdentifierHashAlgorithm", "http://www.w3.org/2000/09/xmldsig#sha1"); //$NON-NLS-1$ //$NON-NLS-2$
		//p2.setProperty("policyQualifier", "http://www.google.com"); //$NON-NLS-1$ //$NON-NLS-2$
		p2.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final Properties p3 = new Properties();
		p3.setProperty("format", AOSignConstants.SIGN_FORMAT_CADES); //$NON-NLS-1$
		p3.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$
		p3.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		CADES_MODES = new Properties[] {
			p1, p2, p3
		};
	}

	/** Algoritmos de firma a probar. */
	private final static String[] ALGOS = {
		AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
		//		AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA
	};

	/** Prueba de firma convencional.
	 * @throws Exception en cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore("Necesita el servidor")
	public void testSignature() throws Exception {

		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
		final PrivateKeyEntry pke;

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try (InputStream is = ClassLoader.getSystemResourceAsStream(CERT_PATH)) {
			ks.load(is, CERT_PASS.toCharArray());
		}
		pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final AOSigner signer = new AOCAdESTriPhaseSigner();

		String prueba;
		for (final Properties extraParams : CADES_MODES) {
			for (final String algo : ALGOS) {
				for (int i = 0; i < DATA_FILES.length; i++) {
					if (DATA.get(i) == null) {
						continue;
					}

					prueba = "Firma CAdES trifasica del fichero " + DATA_FILES[i] + " en modo '" +  //$NON-NLS-1$ //$NON-NLS-2$
							extraParams.getProperty("mode") +  //$NON-NLS-1$
							"' con el algoritmo ': " + //$NON-NLS-1$
							algo +
							"' y politica '" + //$NON-NLS-1$
							extraParams.getProperty("policyIdentifier") + //$NON-NLS-1$
							"'"; //$NON-NLS-1$

					System.out.println(prueba);

					final byte[] result = signer.sign(DATA.get(i), algo, pke.getPrivateKey(), pke.getCertificateChain(), extraParams);

					final File saveFile = File.createTempFile(algo + "-", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$
					try (
						OutputStream os = new FileOutputStream(saveFile);
					) {
						os.write(result);
						os.flush();
					}
					System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$
				}
			}
		}
	}

	/**
	 * Prueba de cofirma.
	 * @throws Exception en cualquier error
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore("Necesita el servidor")
	public void testCoSignature() throws Exception {

		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
		final PrivateKeyEntry pke1 = loadKeyEntry(CERT_PATH, CERT_ALIAS, CERT_PASS);
		final PrivateKeyEntry pke3 = loadKeyEntry(CERT_PATH3, CERT_ALIAS3, CERT_PASS3);

		final AOSigner signer = new AOCAdESTriPhaseSigner();

		String prueba;

		for (final Properties extraParams : CADES_MODES) {
			for (final String algo : ALGOS) {
				for (int i = 0; i < DATA_FILES.length; i++) {
					if (DATA.get(i) == null) {
						continue;
					}

					prueba = "Cofirma CAdES trifasica del fichero " + DATA_FILES[i] + " en modo '" +  //$NON-NLS-1$ //$NON-NLS-2$
							extraParams.getProperty("mode") +  //$NON-NLS-1$
							"' con el algoritmo ': " + //$NON-NLS-1$
							algo +
							"'"; //$NON-NLS-1$

					System.out.println(prueba);

					// Firma simple
					final byte[] sign1 = sign(signer, DATA.get(i), algo, pke1, extraParams);

					// Cofirma sin indicar los datos
					final byte[] sign2 = cosign(signer, sign1, algo, pke3, extraParams);

					// Cofirma indicando los datos
					final byte[] sign3 = cosign(signer, DATA.get(i), sign2, algo, pke3, extraParams);
					Assert.assertNotNull(sign3);


					final File saveFile = File.createTempFile("Cofirma_CAdES_" + algo + "-", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					try (
						OutputStream os = new FileOutputStream(saveFile)
					) {
						os.write(sign3);
						os.flush();
					}
					System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$
				}
			}
		}
	}

	/** Carga la clave privada un certificado de un almac&eacute;n en disco.
	 * @param pkcs12File Fichero P12/PFX.
	 * @param alias Alias del certificado.
	 * @param password Contrase&ntilde;a.
	 * @return Clave privada del certificado.
	 * @throws Exception Cuando falla la carga de la clave. */
	private static PrivateKeyEntry loadKeyEntry(final String pkcs12File,
			                                    final String alias,
			                                    final String password) throws Exception {
		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try (InputStream is = ClassLoader.getSystemResourceAsStream(pkcs12File)) {
			ks.load(is, password.toCharArray());
		}
		return (PrivateKeyEntry) ks.getEntry(alias, new KeyStore.PasswordProtection(password.toCharArray()));
	}

	private static byte[] sign(final AOSigner signer,
			                   final byte[] data,
			                   final String algorithm,
			                   final PrivateKeyEntry pke,
			                   final Properties params) throws Exception {
		return signer.sign(data, algorithm, pke.getPrivateKey(), pke.getCertificateChain(), params);
	}

	/** Cofirma sin necesidad de los datos originales.
	 * @param signer Firmador.
	 * @param sign Firma que se debe cofirmar.
	 * @param algorithm Algoritmo de firma.
	 * @param pke Entra de clave privada para la firma.
	 * @param params Par&aacute;metros extra de configuraci&oacute;n.
	 * @return Cofirma.
	 * @throws Exception Cuando falla la cofirma. */
	private static byte[] cosign(final AOSigner signer, final byte[] sign, final String algorithm, final PrivateKeyEntry pke, final Properties params) throws Exception {
		return signer.cosign(sign, algorithm, pke.getPrivateKey(), pke.getCertificateChain(), params);
	}

	private static byte[] cosign(final AOSigner signer, final byte[] data, final byte[] sign, final String algorithm, final PrivateKeyEntry pke, final Properties params) throws Exception {
		return signer.cosign(data, sign, algorithm, pke.getPrivateKey(), pke.getCertificateChain(), params);
	}
}
