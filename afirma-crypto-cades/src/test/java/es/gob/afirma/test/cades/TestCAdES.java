/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.test.cades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import junit.framework.Assert;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.cades.CAdESValidator;


/**
 * Pruebas del m&oacute;dulo CAdES de Afirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestCAdES {

	private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
	private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

	private static final String CERT_PATH2 = "CATCERT GENCAT SAFP PF Identidad y Firma Reconocida de Clase 1 Caducado.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS2 = "1234"; //$NON-NLS-1$
	private static final String CERT_ALIAS2 = "{71e526c4-0f27-4f32-8be0-90df52dcbc53}"; //$NON-NLS-1$

	private static final String CERT_PATH3 = "CAMERFIRMA_PF_SW_Clave_usuario_Activo.p12"; //$NON-NLS-1$
	private static final String CERT_PASS3 = "1111"; //$NON-NLS-1$
	private static final String CERT_ALIAS3 = "1"; //$NON-NLS-1$

	private static final String[] DATA_FILES = {
		"txt", //$NON-NLS-1$
		"xml" //$NON-NLS-1$
	};

	private static final List<byte[]> DATA = new ArrayList<byte[]>(2);
	static {
		for (final String dataFile : DATA_FILES) {
			try {
				DATA.add(AOUtil.getDataFromInputStream(TestCAdES.class.getResourceAsStream(dataFile)));
			} catch (final IOException e) {
				Logger.getLogger("es.gob.afirma").severe("No se ha podido cargar el fichero de pruebas: " + dataFile);  //$NON-NLS-1$//$NON-NLS-2$
				DATA.add(null);
			}
		}
	}

	private static final Properties[] CADES_MODES;

	static {
		final Properties p1 = new Properties();
		p1.setProperty("format", AOSignConstants.SIGN_FORMAT_CADES); //$NON-NLS-1$
		p1.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$

		final Properties p2 = new Properties();
		p2.setProperty("format", AOSignConstants.SIGN_FORMAT_CADES); //$NON-NLS-1$
		p2.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
		p2.setProperty("policyIdentifier", "urn:oid:2.16.724.1.3.1.1.2.1.8"); //$NON-NLS-1$ //$NON-NLS-2$
		p2.setProperty("policyIdentifierHash", "V8lVVNGDCPen6VELRD1Ja8HARFk="); //$NON-NLS-1$ //$NON-NLS-2$
		p2.setProperty("policyIdentifierHashAlgorithm", "http://www.w3.org/2000/09/xmldsig#sha1"); //$NON-NLS-1$ //$NON-NLS-2$
		//p2.setProperty("policyQualifier", "http://www.google.com"); //$NON-NLS-1$ //$NON-NLS-2$

		final Properties p3 = new Properties();
		p3.setProperty("format", AOSignConstants.SIGN_FORMAT_CADES); //$NON-NLS-1$
		p3.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$

		CADES_MODES = new Properties[] {
				p1, p2, p3
		};
	}

	/** Algoritmos de firma a probar. */
	private final static String[] ALGOS = new String[] {
		AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
		AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
		AOSignConstants.SIGN_ALGORITHM_MD2WITHRSA,
		AOSignConstants.SIGN_ALGORITHM_MD5WITHRSA,
		AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
		AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA
	};

	/**
	 * Prueba de firma convencional.
	 * @throws Exception en cualquier error
	 */
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
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
		cert = (X509Certificate) ks.getCertificate(CERT_ALIAS);

		final AOSigner signer = new AOCAdESSigner();

		String prueba;
		for (final Properties extraParams : CADES_MODES) {
			for (final String algo : ALGOS) {
				for (int i = 0; i < DATA_FILES.length; i++) {
					if (DATA.get(i) == null) {
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

					final byte[] result = signer.sign(DATA.get(i), algo, pke, extraParams);

					final File saveFile = File.createTempFile(algo + "-", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$
					final OutputStream os = new FileOutputStream(saveFile);
					os.write(result);
					os.flush();
					os.close();
					System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$

					// Enviamos a validar a AFirma
					//              if (verifier != null) {
					//                  Assert.assertTrue("Fallo al validar " + saveFile.getAbsolutePath(), verifier.verifyBin(result)); //$NON-NLS-1$
					//              }

					Assert.assertNotNull(prueba, result);
					Assert.assertTrue(signer.isSign(result));
					Assert.assertTrue(CAdESValidator.isCAdESValid(result, AOSignConstants.CMS_CONTENTTYPE_SIGNEDDATA));

					AOTreeModel tree = signer.getSignersStructure(result, false);
					Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject()); //$NON-NLS-1$
					Assert.assertEquals("ANF Usuario Activo", ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject()); //$NON-NLS-1$

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

		signer.sign(DATA.get(0), "SHA1withRSA", pke, null); //$NON-NLS-1$

	}

	/**
	 * Prueba de cofirma.
	 * @throws Exception en cualquier error
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testCoSignature() throws Exception {

		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
		final PrivateKeyEntry pke1 = loadKeyEntry(CERT_PATH, CERT_ALIAS, CERT_PASS);
		final PrivateKeyEntry pke2 = loadKeyEntry(CERT_PATH2, CERT_ALIAS2, CERT_PASS2);
		final PrivateKeyEntry pke3 = loadKeyEntry(CERT_PATH3, CERT_ALIAS3, CERT_PASS3);

		final AOSigner signer = new AOCAdESSigner();

		String prueba;

		for (final Properties extraParams : CADES_MODES) {
			for (final String algo : ALGOS) {
				for (int i = 0; i < DATA_FILES.length; i++) {
					if (DATA.get(i) == null) {
						continue;
					}

					prueba = "Cofirma CAdES " + DATA_FILES[i] + " en modo '" +  //$NON-NLS-1$ //$NON-NLS-2$
							extraParams.getProperty("mode") +  //$NON-NLS-1$
							"' con el algoritmo ': " + //$NON-NLS-1$
							algo +
							"'"; //$NON-NLS-1$

					System.out.println(prueba);

					// Firma simple
					final byte[] sign1 = sign(signer, DATA.get(i), algo, pke1, extraParams);

					// Cofirma sin indicar los datos
					final byte[] sign2 = cosign(signer, sign1, algo, pke2, extraParams);

					checkSign(signer, sign2, new PrivateKeyEntry[] {pke1, pke2}, new String[] {"ANF Usuario Activo", "CPISR-1 Pf\u00EDsica De la Se\u00F1a Pruebasdit"}, prueba); //$NON-NLS-1$ //$NON-NLS-2$

					// Cofirma indicando los datos
					final byte[] sign3 = cosign(signer, DATA.get(i), sign2, algo, pke3, extraParams);
					Assert.assertNotNull(sign3);


					final File saveFile = File.createTempFile("Cofirma_CAdES_" + algo + "-", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					final OutputStream os = new FileOutputStream(saveFile);
					os.write(sign3);
					os.flush();
					os.close();
					System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$


					//checkSign(signer, sign3, new PrivateKeyEntry[] {pke1, pke2, pke3}, new String[] {"ANF Usuario Activo", "CPISR-1 Pf\u00EDsica De la Se\u00F1a Pruebasdit", "Certificado Pruebas Software V\u00E1lido"}, prueba); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

					//System.out.println(prueba + ": OK"); //$NON-NLS-1$
				}
			}
		}
	}

	/**
	 * Carga la clave privada un certificado de un almac&eacute;n en disco.
	 * @param pkcs12File Fichero P12/PFX.
	 * @param alias Alias del certificado.
	 * @param password Contrase&ntilde;a.
	 * @return Clave privada del certificado.
	 */
	private static PrivateKeyEntry loadKeyEntry(final String pkcs12File, final String alias, final String password) throws Exception {
		final PrivateKeyEntry pke;

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(pkcs12File), password.toCharArray());
		pke = (PrivateKeyEntry) ks.getEntry(alias, new KeyStore.PasswordProtection(password.toCharArray()));

		return pke;
	}

	private static byte[] sign(final AOSigner signer, final byte[] data, final String algorithm, final PrivateKeyEntry pke, final Properties params) throws Exception {
		return signer.sign(data, algorithm, pke, params);
	}

	/** Cofirma sin necesidad de los datos originales. */
	private static byte[] cosign(final AOSigner signer, final byte[] sign, final String algorithm, final PrivateKeyEntry pke, final Properties params) throws Exception {
		return signer.cosign(sign, algorithm, pke, params);
	}


	private static byte[] cosign(final AOSigner signer, final byte[] data, final byte[] sign, final String algorithm, final PrivateKeyEntry pke, final Properties params) throws Exception {
		return signer.cosign(data, sign, algorithm, pke, params);
	}

	/** Hace las comprobaciones b&aacute;sicas de una firma. */
	private static void checkSign(final AOSigner signer, final byte[] sign, final PrivateKeyEntry[] pke, final String[] signsAlias, final String prueba) throws Exception {
		Assert.assertNotNull(prueba, sign);
		Assert.assertTrue(signer.isSign(sign));
		Assert.assertTrue(CAdESValidator.isCAdESValid(sign, AOSignConstants.CMS_CONTENTTYPE_SIGNEDDATA));

		// Arbol de alias
		AOTreeModel tree = signer.getSignersStructure(sign, false);
		AOTreeNode root = (AOTreeNode) tree.getRoot();
		Assert.assertEquals("Datos", root.getUserObject()); //$NON-NLS-1$
		for (int i = 0; i < signsAlias.length; i++) {
			Assert.assertEquals(signsAlias[i], root.getChildAt(i).getUserObject());
		}

		// Arbol de AOSimpleSignersInfo
		tree = signer.getSignersStructure(sign, true);
		root = (AOTreeNode) tree.getRoot();
		Assert.assertEquals("Datos", root.getUserObject()); //$NON-NLS-1$
		for (int i = 0; i < signsAlias.length; i++) {
			final AOSimpleSignInfo simpleSignInfo = (AOSimpleSignInfo) root.getChildAt(i).getUserObject();
			//          Assert.assertEquals("CAdES", simpleSignInfo.getSignFormat());
			//          Assert.assertEquals(algo, simpleSignInfo.getSignAlgorithm());
			Assert.assertNotNull(simpleSignInfo.getSigningTime());
			Assert.assertEquals(pke[i].getCertificate(), simpleSignInfo.getCerts()[0]);
		}
	}
}
