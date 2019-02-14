/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.odf;

import java.io.ByteArrayInputStream;
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

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;


/** Comprueba la ejecuci&oacute;n de firmas electr&oacute;nicas ODF. */
public final class TestAOODFSignerTest {

    private static final String CERT_PATH = "PruebaEmpleado4Activo.p12"; //$NON-NLS-1$
    private static final String CERT_PASS = "Giss2016"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "givenname=prueba4empn+serialnumber=idces-00000000t+sn=p4empape1 p4empape2 - 00000000t+cn=prueba4empn p4empape1 p4empape2 - 00000000t,ou=personales,ou=certificado electronico de empleado publico,o=secretaria de estado de la seguridad social,c=es"; //$NON-NLS-1$

    private static final String CERT_PATH2 = "CATCERT GENCAT SAFP PF Identidad y Firma Reconocida de Clase 1 Caducado.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS2 = "1234"; //$NON-NLS-1$
    private static final String CERT_ALIAS2 = "{71e526c4-0f27-4f32-8be0-90df52dcbc53}"; //$NON-NLS-1$

    private static final String[] DATA_FILES = {
    	"odt", "ods", "odp" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    };

    private static final List<byte[]> DATA = new ArrayList<>(2);
    static {
    	for (final String dataFile : DATA_FILES) {
        	try {
    			DATA.add(AOUtil.getDataFromInputStream(TestAOODFSignerTest.class.getResourceAsStream("/" + dataFile))); //$NON-NLS-1$
    		} catch (final IOException e) {
    			Logger.getLogger("es.gob.afirma").severe("No se ha podido cargar el fichero de pruebas'" + dataFile + "': " + e);  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
    			DATA.add(null);
    		}
    	}
    }

    private static final Properties[] CONFIGS;

    static {
        final Properties p1 = new Properties();

        final Properties p2 = new Properties();
        try {
        p2.load(new ByteArrayInputStream(
    		"useOpenOffice31Mode=true".getBytes()) //$NON-NLS-1$
		);
        }
        catch (final Exception e) {
        	System.err.println("No se ha podido cargar una de las configuraciones de pruebas de ODF: " + e); //$NON-NLS-1$
		}
        CONFIGS = new Properties[] {
                p1, p2
        };
    }

    /** Main para pruebas sin JUnit.
     * @param argc No se usa.
     * @throws Exception En cualquier error. */
    public static void main(final String[] argc) throws Exception {
    	new TestAOODFSignerTest().firmaODF();
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

					System.out.println("Se va a firma el documento " + DATA_FILES[i] + (extraParams.size() == 0 ? "" : " con las opciones:")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					for (final String key : extraParams.keySet().toArray(new String[0])) {
						System.out.println(key + ": " + extraParams.getProperty(key)); //$NON-NLS-1$
					}

					final File tempFile = File.createTempFile("odfSign", "." + DATA_FILES[i]); //$NON-NLS-1$ //$NON-NLS-2$
					try (
						final FileOutputStream fos = new FileOutputStream(tempFile);
					) {

						final byte[] result = signer.sign(
							DATA.get(i),
							"SHA512withRSA", //$NON-NLS-1$
							pke.getPrivateKey(),
							pke.getCertificateChain(),
							extraParams
						);
						Assert.assertNotNull(result);

						fos.write(result);
						fos.flush();
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
					try (
						final FileOutputStream fos = new FileOutputStream(tempFile);
					) {

						final byte[] signature = signer.sign(
							DATA.get(i),
							"SHA1withRSA", //$NON-NLS-1$
							pke.getPrivateKey(),
							pke.getCertificateChain(),
							extraParams
						);
						Assert.assertNotNull(signature);

						final byte[] result = signer.cosign(
							signature,
							"SHA1withRSA", //$NON-NLS-1$
							pke2.getPrivateKey(),
							pke2.getCertificateChain(),
							extraParams
						);
						Assert.assertNotNull(result);

						fos.write(result);
						fos.flush();
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
