/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.test.cades;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.nio.charset.StandardCharsets;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.Provider;
import java.security.Security;
import java.util.Date;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.signers.cades.CAdESParameters;
import es.gob.afirma.signers.cades.GenCAdESEPESSignedData;

/** Pruebas espec&iacute;ficas de CAdES para DNIe.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class MiniTestDNI {

    private static final String DNIE_DRIVER_PATH = "name=testdni\r\nlibrary=c:/windows/system32/UsrPkcs11.dll\r\nshowInfo=false"; //$NON-NLS-1$

    private static final char[] DNI_PIN = "pin".toCharArray();  //$NON-NLS-1$

    private static final String DNI_SIGN_ALIAS = "CertFirmaDigital"; //$NON-NLS-1$

    private static final String TEXTO_FIRMAR = "Tom\u00F3"; //$NON-NLS-1$

    /** Mini-prueba CAdES especifica para DNIe.
     * @throws Exception en caso de cualquier tipo de problema. */
    @SuppressWarnings({ "static-method" })
    @Ignore // Necesita un DNIe
	@Test
    public void testCAdESDNIe() throws Exception {

        final Constructor<?> sunPKCS11Contructor = Class.forName("sun.security.pkcs11.SunPKCS11").getConstructor(InputStream.class); //$NON-NLS-1$

		final Provider p = (Provider) sunPKCS11Contructor.newInstance(new ByteArrayInputStream(DNIE_DRIVER_PATH.getBytes()));

        Security.addProvider(p);
        final KeyStore ks = KeyStore.getInstance("PKCS11", p); //$NON-NLS-1$
        ks.load(null, DNI_PIN);

        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(DNI_SIGN_ALIAS, new KeyStore.PasswordProtection(DNI_PIN));

        final CAdESParameters parameters = new CAdESParameters();
        parameters.setContentData(TEXTO_FIRMAR.getBytes(StandardCharsets.UTF_8));
        parameters.setDataDigest(null);// Se calcula internamente el digest de los datos a firmar.
        parameters.setDigestAlgorithm("SHA-512"); //$NON-NLS-1$
        parameters.setSigningCertificateV2(true);
        parameters.setIncludedIssuerSerial(true);
        parameters.setIncludedPolicyOnSigningCertificate(true);
        parameters.setSigningTime(new Date());

        final byte[] firma = GenCAdESEPESSignedData.generateSignedData(
    		"SHA512withRSA", //$NON-NLS-1$
    		pke.getPrivateKey(),
    		pke.getCertificateChain(),
    		parameters
		);

        Assert.assertNotNull(firma);
        try (
    		final java.io.FileOutputStream fos = new java.io.FileOutputStream("C:/pruebas/salida/MiniTestCadesNuevo.csig"); //$NON-NLS-1$
		) {
            fos.write(firma);
        }

    }

}
