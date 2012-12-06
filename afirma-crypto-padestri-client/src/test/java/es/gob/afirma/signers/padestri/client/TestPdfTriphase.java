/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.padestri.client;

import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSigner;

/** Pruebas de firma trif&aacute;sica. */
public class TestPdfTriphase {

	/** Nombre de la propiedad de URL del servidor de firma trif&aacute;sica. */
	private static final String PROPERTY_NAME_SIGN_SERVER_URL = "serverUrl"; //$NON-NLS-1$
	private static final String PROPERTY_VALUE_SIGN_SERVER_URL = "http://localhost:8080/afirma-crypto-padestri-sample/SignatureService"; //$NON-NLS-1$

	private static final String PROPERTY_NAME_DOC_ID = "documentId"; //$NON-NLS-1$
	private static final String PROPERTY_VALUE_DOC_ID = "docIdPrueba"; //$NON-NLS-1$

	private static final String PROPERTY_NAME_ATTACH = "attach"; //$NON-NLS-1$
	private static final String PROPERTY_NAME_ATTACH_FILENAME = "attachFileName"; //$NON-NLS-1$
	private static final String PROPERTY_NAME_ATTACH_DESCRIPTION = "attachDescription"; //$NON-NLS-1$


    private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

    private static final String TEST_IMAGE_FILE = "splash.png"; //$NON-NLS-1$

    private PrivateKeyEntry pke;

    private Properties serverConfig;

	/** Carga el almac&acute;n de pruebas.
	 * @throws Exception */
	@Before
	public void loadKeystore() throws Exception {

		// Cargamos la referencia a la clave privada
        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        this.pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        // Establecemos la configuracion
        this.serverConfig = new Properties();
        this.serverConfig.setProperty(PROPERTY_NAME_SIGN_SERVER_URL, PROPERTY_VALUE_SIGN_SERVER_URL);
        this.serverConfig.setProperty(PROPERTY_NAME_DOC_ID, PROPERTY_VALUE_DOC_ID);
	}

	/** Prueba de firma trif&aacute;sica normal.
	 * @throws Exception */
	@Ignore
	@Test
	public void firma() throws Exception {
		final AOSigner signer = new AOPDFTriPhaseSigner();

		final Properties config = new Properties(this.serverConfig);

		for (final String key : this.serverConfig.keySet().toArray(new String[this.serverConfig.size()])) {
			config.setProperty(key, this.serverConfig.getProperty(key));
		}
		System.out.println(config.size());

        final byte[] result = signer.sign(null, "SHA512withRSA", this.pke, config); //$NON-NLS-1$

        Assert.assertNotNull("Error durante el proceso de firma, resultado nulo", result); //$NON-NLS-1$
        Assert.assertEquals("No se recibió un OK desde servidor", "OK", new String(result)); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Prueba de firma trif&aacute;sica con adjunto en el PDF.
	 * @throws Exception */
	@Test
	public void firmaConAdjunto() throws Exception {
		final AOSigner signer = new AOPDFTriPhaseSigner();

		final Properties config = new Properties(this.serverConfig);
		for (final String key : this.serverConfig.keySet().toArray(new String[this.serverConfig.size()])) {
			config.setProperty(key, this.serverConfig.getProperty(key));
		}

		config.setProperty(PROPERTY_NAME_ATTACH, loadFileOnBase64(TEST_IMAGE_FILE));
		config.setProperty(PROPERTY_NAME_ATTACH_FILENAME, "splash.png"); //$NON-NLS-1$
		config.setProperty(PROPERTY_NAME_ATTACH_DESCRIPTION, "Imagen adjunta de prueba"); //$NON-NLS-1$

        final byte[] result = signer.sign(null, "SHA512withRSA", this.pke, config); //$NON-NLS-1$

        Assert.assertNotNull("Error durante el proceso de firma, resultado nulo", result); //$NON-NLS-1$
        Assert.assertTrue("Se recibio un codigo de error desde el servidor", !new String(result).startsWith("ERR-")); //$NON-NLS-1$ //$NON-NLS-2$
        System.out.println(new String(result));

	}

	private static String loadFileOnBase64(final String filename) throws Exception {
		final InputStream is = ClassLoader.getSystemResourceAsStream(filename);
		final byte[] encoded = AOUtil.getDataFromInputStream(is);
		is.close();

		return Base64.encode(encoded);
	}
}
