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

import java.io.ByteArrayInputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.Provider;
import java.security.Security;
import java.security.cert.X509Certificate;
import java.util.Properties;

import junit.framework.Assert;

import org.junit.Test;

import sun.security.pkcs11.SunPKCS11;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.signers.cades.GenCAdESEPESSignedData;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;

/**
 * Pruebas espec&iacute;ficas de CAdES para DNIe.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 *
 */
@SuppressWarnings("restriction")
public class MiniTestDNI {

    private static final String DNIE_DRIVER_PATH = "name=testdni\r\nlibrary=c:/windows/system32/UsrPkcs11.dll\r\nshowInfo=false"; //$NON-NLS-1$

    private static final char[] DNI_PIN = "pin".toCharArray();  //$NON-NLS-1$

    private static final String DNI_SIGN_ALIAS = "CertFirmaDigital"; //$NON-NLS-1$

    private static final String TEXTO_FIRMAR = "Tom\u00F3"; //$NON-NLS-1$

    /**
     * Mini-prueba CAdES espec’fica para DNIe.
     * @throws Exception en caso de cualquier tipo de problema
     */
    @SuppressWarnings({ "static-method" })
	@Test
    public void testCAdESDNIe() throws Exception {

        final Provider p = new SunPKCS11(new ByteArrayInputStream(DNIE_DRIVER_PATH.getBytes()));
        Security.addProvider(p);
        final KeyStore ks = KeyStore.getInstance("PKCS11", p); //$NON-NLS-1$
        ks.load(null, DNI_PIN);

        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(DNI_SIGN_ALIAS, new KeyStore.PasswordProtection(DNI_PIN));

        final P7ContentSignerParameters p7ContentSignerParameters = new P7ContentSignerParameters(TEXTO_FIRMAR.getBytes("UTF-8"), "SHA1withRSA", (X509Certificate[]) pke.getCertificateChain());  //$NON-NLS-1$ //$NON-NLS-2$

        final boolean omitContent = false;
        final byte[] messageDigest = null; // Se calcula internamente el digest de los datos a firmar.

        final byte[] firma = GenCAdESEPESSignedData.generateSignedData(p7ContentSignerParameters, omitContent, new AdESPolicy(new Properties()), pke, messageDigest, false);

        Assert.assertNotNull(firma);

//        java.io.FileOutputStream fos = new java.io.FileOutputStream("C:/pruebas/salida/MiniTestCadesNuevo"+ (omitContent ? "Expl" : "Impl") + ".csig");
//        fos.write(firma);
//        try { fos.close(); } catch (Exception e) { }
    }

}
