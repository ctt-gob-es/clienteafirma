/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.odf;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.CertificateException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;


/** Comprueba la ejecuci&oacute;n de firmas electr&oacute;nicas ODF. */
public final class AOODFSignerTest {

    private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

    private static final String CERT_PATH2 = "CATCERT GENCAT SAFP PF Identidad y Firma Reconocida de Clase 1 Caducado.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS2 = "1234"; //$NON-NLS-1$
    private static final String CERT_ALIAS2 = "{71e526c4-0f27-4f32-8be0-90df52dcbc53}"; //$NON-NLS-1$

    private static final String[] DATA_FILES = {
    	"odt", //$NON-NLS-1$
    };

    private static final List<byte[]> DATA = new ArrayList<byte[]>(2);
    static {
    	for (final String dataFile : DATA_FILES) {
        	try {
    			DATA.add(AOUtil.getDataFromInputStream(AOODFSignerTest.class.getResourceAsStream("/" + dataFile))); //$NON-NLS-1$
    		} catch (final IOException e) {
    			Logger.getLogger("es.gob.afirma").severe("No se ha podido cargar el fichero de pruebas: " + dataFile);  //$NON-NLS-1$//$NON-NLS-2$
    			DATA.add(null);
    		}
    	}
    }

    private static final Properties[] CONFIGS;

    static {
        final Properties p1 = new Properties();

        CONFIGS = new Properties[] {
                p1
        };
    }

	/**
	 * Firma documentos ODF.
	 * @throws Exception Cuando se produce un error en el proceso de firma.
	 */
	@SuppressWarnings("static-method")
	@Test
    public void firmaODF() throws Exception {

        final PrivateKeyEntry pke = loadKeyEntry(CERT_PATH, CERT_PASS, CERT_ALIAS);

		final AOODFSigner signer = new AOODFSigner();

		for (final Properties extraParams : CONFIGS) {
			for (int i = 0; i < DATA_FILES.length; i++) {
				if (DATA.get(i) != null) {

					System.out.println("Se va a firma el documento " + DATA_FILES[i]); //$NON-NLS-1$

					final File tempFile = File.createTempFile("odfSign", "." + DATA_FILES[i]); //$NON-NLS-1$ //$NON-NLS-2$
					final FileOutputStream fos = new FileOutputStream(tempFile);

					final byte[] result = signer.sign(DATA.get(i), "SHA1withRSA", pke, extraParams); //$NON-NLS-1$
					Assert.assertNotNull(result);

					fos.write(result);
					try {
						fos.flush();
						fos.close();
					} catch (final Exception e) {
						// No hacemos nada
					}

					System.out.println("Temporal para comprobacion manual: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
				}
			}
		}
    }


    /**
     * Prueba a firmar y cofirmar un fichero ODF.
     * @throws Exception Cuando se produce un error durante la operaci&oacute;n de firma o cofirma.
     */
	@SuppressWarnings("static-method")
	@Test
    public void firmaYCofirmaODF() throws Exception {
        final PrivateKeyEntry pke = loadKeyEntry(CERT_PATH, CERT_PASS, CERT_ALIAS);
        final PrivateKeyEntry pke2 = loadKeyEntry(CERT_PATH2, CERT_PASS2, CERT_ALIAS2);

		final AOODFSigner signer = new AOODFSigner();

		for (final Properties extraParams : CONFIGS) {
			for (int i = 0; i < DATA_FILES.length; i++) {
				if (DATA.get(i) != null) {

					System.out.println("Se va a firma y cofirmar el documento " + DATA_FILES[i]); //$NON-NLS-1$

					final File tempFile = File.createTempFile("odfCosign", "." + DATA_FILES[i]); //$NON-NLS-1$ //$NON-NLS-2$
					final FileOutputStream fos = new FileOutputStream(tempFile);

					final byte[] signature = signer.sign(DATA.get(i), "SHA1withRSA", pke, extraParams); //$NON-NLS-1$
					Assert.assertNotNull(signature);

					final byte[] result = signer.cosign(signature, "SHA1withRSA", pke2, extraParams); //$NON-NLS-1$
					Assert.assertNotNull(result);

					fos.write(result);
					try {
						fos.flush();
						fos.close();
					} catch (final Exception e) {
						// No hacemos nada
					}

					System.out.println("Temporal para comprobacion manual: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
				}
			}
		}
    }

	private static PrivateKeyEntry loadKeyEntry(final String certPath, final String certPass, final String certAlias) throws Exception, CertificateException, IOException {
		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(certPath), certPass.toCharArray());
        return (PrivateKeyEntry) ks.getEntry(certAlias, new KeyStore.PasswordProtection(certPass.toCharArray()));
	}
}
