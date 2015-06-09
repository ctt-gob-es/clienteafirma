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
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signers.cades.AOCAdESSigner;


/**
 * Pruebas del m&oacute;dulo CAdES de Afirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestCAdESCommitment {

	private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
	private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

	private static final String[] DATA_FILES = {
		"txt", //$NON-NLS-1$
	};

	private static final List<byte[]> DATA = new ArrayList<byte[]>(2);
	static {
		for (final String dataFile : DATA_FILES) {
			try {
				DATA.add(AOUtil.getDataFromInputStream(TestCAdESCommitment.class.getResourceAsStream(dataFile)));
			} catch (final IOException e) {
				Logger.getLogger("es.gob.afirma").severe("No se ha podido cargar el fichero de pruebas: " + dataFile);  //$NON-NLS-1$//$NON-NLS-2$
				DATA.add(null);
			}
		}
	}

	private static final Properties[] CADES_MODES;

	static {

		final Properties p3 = new Properties();
		p3.setProperty("format", AOSignConstants.SIGN_FORMAT_CADES); //$NON-NLS-1$
		p3.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
		p3.setProperty("format", AOSignConstants.SIGN_FORMAT_CADES); //$NON-NLS-1$
		p3.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
	    p3.setProperty("commitmentTypeIndications", "1"); //$NON-NLS-1$ //$NON-NLS-2$
	    p3.setProperty("commitmentTypeIndication0Identifier", "1"); //$NON-NLS-1$ //$NON-NLS-2$
	    p3.setProperty("commitmentTypeIndication0CommitmentTypeQualifiers", "1.2.3.4.1|1.2.3.4.2|1.2.3.4.3"); //$NON-NLS-1$ //$NON-NLS-2$
	    p3.setProperty("signerLocationPostalAddress", "c/ Albarracin 25\n28037\nMadrid"); //$NON-NLS-1$ //$NON-NLS-2$
		p3.setProperty("signerLocationCountryName", "Spain"); //$NON-NLS-1$ //$NON-NLS-2$
		p3.setProperty("signerLocationLocalityName", "Madrid"); //$NON-NLS-1$ //$NON-NLS-2$

		CADES_MODES = new Properties[] {
			p3
		};
	}

	/** Algoritmos de firma a probar. */
	private final static String[] ALGOS = new String[] {
		AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
	};

	/** Prueba de firma con CommitmentTypeIndications.
	 * @throws Exception en cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testSignature() throws Exception {

		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
		final PrivateKeyEntry pke;

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final AOSigner signer = new AOCAdESSigner();

		String prueba;
		for (final Properties extraParams : CADES_MODES) {
			for (final String algo : ALGOS) {
				for (int i = 0; i < DATA_FILES.length; i++) {
					if (DATA.get(i) == null) {
						continue;
					}

					prueba = "Firma CAdES con Commitment del fichero " + DATA_FILES[i] + " en modo '" +  //$NON-NLS-1$ //$NON-NLS-2$
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
					final OutputStream os = new FileOutputStream(saveFile);
					os.write(result);
					os.flush();
					os.close();
					System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$

				}
			}
		}

	}

}
