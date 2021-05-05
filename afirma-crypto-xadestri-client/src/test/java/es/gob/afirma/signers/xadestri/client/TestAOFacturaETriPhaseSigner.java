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
import java.io.FileOutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;

/** Pruebas XAdES FacturaE trif&aacute;sico.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestAOFacturaETriPhaseSigner {

	private static final String CERT_PATH = "PFActivoFirSHA1.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
	private static final String CERT_ALIAS = "fisico activo prueba"; //$NON-NLS-1$

	private static final String DATA_FILENAME = "factura_sinFirmar.xml"; //$NON-NLS-1$

	//private static final String SERVER_URL = "https://valide.redsara.es/firmaMovil/TriPhaseSignerServer/SignatureService"; //$NON-NLS-1$
	//private static final String SERVER_URL = "https://prevalide.redsara.es/firmaMovil/TriPhaseSignerServer/SignatureService"; //$NON-NLS-1$
	//private static final String SERVER_URL = "http://localhost:8080/TriPhaseSignerServer/SignatureService"; //$NON-NLS-1$
	private static final String SERVER_URL = "http://localhost:8080/afirma-server-triphase-signer/SignatureService"; //$NON-NLS-1$

	/**
	 * Prueba de firma FacturaE trif&aacute;sica.
	 * @throws Exception Cuando falla la firma.
	 */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita un servidor trifasico
	public void pruebaFirmaFacturaE() throws Exception {

		final Properties config0 = new Properties();
		config0.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

		final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(DATA_FILENAME));

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final AOSigner signer = AOSignerFactory.getSigner(AOSignConstants.SIGN_FORMAT_FACTURAE_TRI);

		final byte[] result = signer.sign(
			data,
			AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			config0 // extraParams
		);

		final File tempFile = File.createTempFile("xades-facturae-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final FileOutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(result);
		}

		System.out.println("El resultado de la firma se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$

	}

}