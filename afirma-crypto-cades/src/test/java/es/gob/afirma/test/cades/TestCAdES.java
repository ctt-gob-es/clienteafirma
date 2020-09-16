/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.test.cades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.cades.CAdESExtraParams;
import es.gob.afirma.signers.cades.CAdESValidator;

/** Pruebas del m&oacute;dulo CAdES de Afirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestCAdES {

    private static final String CERT_PATH = "PruebaEmpleado4Activo.p12"; //$NON-NLS-1$
    private static final String CERT_PASS = "Giss2016"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "givenname=prueba4empn+serialnumber=idces-00000000t+sn=p4empape1 p4empape2 - 00000000t+cn=prueba4empn p4empape1 p4empape2 - 00000000t,ou=personales,ou=certificado electronico de empleado publico,o=secretaria de estado de la seguridad social,c=es"; //$NON-NLS-1$

	private static final String[] DATA_FILES = {
//		"txt", //$NON-NLS-1$
		"xml" //$NON-NLS-1$
	};

	private static final List<byte[]> DATA = new ArrayList<>(2);
	static {
		for (final String dataFile : DATA_FILES) {
			try (
				final InputStream is = TestCAdES.class.getResourceAsStream("/" + dataFile) //$NON-NLS-1$
			) {
				DATA.add(AOUtil.getDataFromInputStream(is));
			}
			catch (final IOException e) {
				Logger.getLogger("es.gob.afirma").severe("No se ha podido cargar el fichero de pruebas '" + dataFile + "': " + e);  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
				DATA.add(null);
			}
		}
	}

	private static final String CATCERT_POLICY = "0.4.0.2023.1.1"; //$NON-NLS-1$
	private static final String CATCERT_TSP = "http://psis.catcert.net/psis/catcert/tsp"; //$NON-NLS-1$
	private static final Boolean CATCERT_REQUIRECERT = Boolean.TRUE;

	private static final Properties[] CADES_MODES;

	static {
		final Properties p1 = new Properties();
		p1.setProperty("format", AOSignConstants.SIGN_FORMAT_CADES); //$NON-NLS-1$
		p1.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$

		final Properties p2 = new Properties();
		p2.setProperty("format", AOSignConstants.SIGN_FORMAT_CADES); //$NON-NLS-1$
		p2.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
		p2.setProperty("policyIdentifier", "urn:oid:2.16.724.1.3.1.1.2.1.8"); //$NON-NLS-1$ //$NON-NLS-2$
		p2.setProperty("policyIdentifierHash", "7SxX3erFuH31TvAw9LZ70N7p1vA="); //$NON-NLS-1$ //$NON-NLS-2$
		p2.setProperty("policyIdentifierHashAlgorithm", "http://www.w3.org/2000/09/xmldsig#sha1"); //$NON-NLS-1$ //$NON-NLS-2$
		//p2.setProperty("policyQualifier", "http://www.google.com"); //$NON-NLS-1$ //$NON-NLS-2$

		final Properties p3 = new Properties();
		p3.setProperty("format", AOSignConstants.SIGN_FORMAT_CADES); //$NON-NLS-1$
		p3.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$

		final Properties p4 = new Properties();
		p4.setProperty("format", AOSignConstants.SIGN_FORMAT_CADES); //$NON-NLS-1$
		p4.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
        p4.put("tsaURL", CATCERT_TSP); //$NON-NLS-1$
        p4.put("tsaPolicy", CATCERT_POLICY); //$NON-NLS-1$
        p4.put("tsaRequireCert", CATCERT_REQUIRECERT); //$NON-NLS-1$
        p4.put("tsaHashAlgorithm", "SHA-512"); //$NON-NLS-1$ //$NON-NLS-2$

		CADES_MODES = new Properties[] {
				//p1,
				p2, p3,
				//p4
		};
	}

	/** Algoritmos de firma a probar. */
	private final static String[] ALGOS = new String[] {
//		AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
		AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
//		AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
//		AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA
	};

	/** Prueba de firma de JPEG para establecimiento de ContentHint.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testJpegSign() throws Exception {

		final PrivateKeyEntry pke;

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream(CERT_PATH)
		) {
			ks.load(is, CERT_PASS.toCharArray());
		}
		pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final AOSigner signer = new AOCAdESSigner();

		final Properties p = new Properties();
		p.put("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$

		final byte[] data;
		try (
			final InputStream is = TestCAdES.class.getResourceAsStream("/rubric.jpg") //$NON-NLS-1$
		) {
			data = AOUtil.getDataFromInputStream(is);
		}

		final byte[] sign = signer.sign(data, "SHA512withRSA", pke.getPrivateKey(), pke.getCertificateChain(), p); //$NON-NLS-1$

		try (
			final OutputStream fos = new FileOutputStream(File.createTempFile("JPG_", ".csig")) //$NON-NLS-1$ //$NON-NLS-2$
		) {
			fos.write(sign);
		}
	}

	/** Prueba de firma convencional.
	 * @throws Exception en cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testSignature() throws Exception {
		/*
      es.gob.afirma.platform.ws.TestSignVerifier verifier = null;
      try {
          verifier = new TestSignVerifier();
      }
      catch (Exception e) {
          System.out.println("No se ha podido inicializar el validador de firmas, no se validaran como parte de las pruebas: " + e); //$NON-NLS-1$
      }
		 */

		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
		final PrivateKeyEntry pke;
		final X509Certificate cert;

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream(CERT_PATH)
		) {
			ks.load(is, CERT_PASS.toCharArray());
		}
		pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
		cert = (X509Certificate) ks.getCertificate(CERT_ALIAS);

		final AOSigner signer = new AOCAdESSigner();

		String prueba;
		for (final Properties extraParams : CADES_MODES) {
			for (final String algo : ALGOS) {
				for (int i = 0; i < DATA_FILES.length; i++) {
					if (DATA.get(i) == null || DATA.get(i).length == 0) {
						continue;
					}

					prueba = "Firma CAdES del fichero " + DATA_FILES[i] + " en modo '" +  //$NON-NLS-1$ //$NON-NLS-2$
							extraParams.getProperty("mode") +  //$NON-NLS-1$
							"' con el algoritmo ': " + //$NON-NLS-1$
							algo +
							"' y politica '" + //$NON-NLS-1$
							extraParams.getProperty("policyIdentifier") + //$NON-NLS-1$
							"'"; //$NON-NLS-1$

					System.out.println(prueba);

					final byte[] result = signer.sign(
						DATA.get(i), algo, pke.getPrivateKey(), pke.getCertificateChain(), extraParams
					);

					final File saveFile = File.createTempFile(algo + "-", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$
					try (
						final OutputStream os = new FileOutputStream(saveFile)
					) {
						os.write(result);
						os.flush();
					}
					System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$

					// Enviamos a validar a AFirma
					//              if (verifier != null) {
					//                  Assert.assertTrue("Fallo al validar " + saveFile.getAbsolutePath(), verifier.verifyBin(result)); //$NON-NLS-1$
					//              }

					Assert.assertNotNull(prueba, result);
					Assert.assertTrue(signer.isSign(result));
					Assert.assertTrue(CAdESValidator.isCAdESValid(result, AOSignConstants.CMS_CONTENTTYPE_SIGNEDDATA, true));

					AOTreeModel tree = signer.getSignersStructure(result, false);
					Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject()); //$NON-NLS-1$
					Assert.assertEquals("PRUEBA4EMPN P4EMPAPE1 P4EMPAPE2 - 00000000T", ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject()); //$NON-NLS-1$

					tree = signer.getSignersStructure(result, true);
					Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject()); //$NON-NLS-1$
					final AOSimpleSignInfo simpleSignInfo = (AOSimpleSignInfo) ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject();

					//                Assert.assertEquals("CAdES", simpleSignInfo.getSignFormat());
					//                Assert.assertEquals(algo, simpleSignInfo.getSignAlgorithm());
					Assert.assertNotNull(simpleSignInfo.getSigningTime());
					Assert.assertEquals(cert, simpleSignInfo.getCerts()[0]);


					//System.out.println(prueba + ": OK"); //$NON-NLS-1$
				}
			}
		}
	}


	/** Prueba de firma indicando los cargos del firmante.
	 * @throws Exception en cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testSignatureWithClaimedRoles() throws Exception {

		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
		final PrivateKeyEntry pke;

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream(CERT_PATH)
		) {
			ks.load(is, CERT_PASS.toCharArray());
		}
		pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final AOSigner signer = new AOCAdESSigner();

		final Properties extraParams = new Properties();
		extraParams.setProperty(CAdESExtraParams.MODE, "implicit"); //$NON-NLS-1$
		extraParams.setProperty(CAdESExtraParams.SIGNER_CLAIMED_ROLES, "Apoderado de empresa|Director de proyecto"); //$NON-NLS-1$

		final byte[] result = signer.sign(
			DATA.get(0), AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA, pke.getPrivateKey(), pke.getCertificateChain(), extraParams
		);

		final File saveFile = File.createTempFile("CAdES-ClaimedRoles-", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream os = new FileOutputStream(saveFile)
		) {
			os.write(result);
			os.flush();
		}
		System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$
	}
}
