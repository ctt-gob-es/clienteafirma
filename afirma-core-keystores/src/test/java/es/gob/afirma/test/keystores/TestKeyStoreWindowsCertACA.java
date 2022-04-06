/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.test.keystores;

import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.Certificate;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;

/** Pruebas espec&iacute;ficas para los almacenes de Windows con certificado ACA.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestKeyStoreWindowsCertACA {

    /** Prueba de carga y uso de certificado ACA en CAPI con AOKeyStoreManager.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
	@Ignore
    public void testStandaloneKeyChain() throws Exception {

    	if (!Platform.OS.WINDOWS.equals(Platform.getOS())) {
            return;
        }

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

        final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
    		AOKeyStore.WINDOWS,
    		null,
    		null,
    		null,
    		null
		);

        final String ALIAS = "EA=demo.empleado@cgae.redabogacia.org, CN=NOMBRE EMPLEADO EMPLEADO DEMO - NIF 08967425R, OU=Informatica, O=Consejo General de la Abogac\u00EDa Espa\u00F1ola / CGAE / 2000, C=ES, ST=Madrid, OID.2.5.4.12=#1308506572736F6E616C, OID.1.3.6.1.4.1.4710.1.3.2=#1309513238363330303649, OID.2.5.4.5=#1309303839363734323552, OID.2.5.4.42=#130444454D4F, OID.2.5.4.4=#1308454D504C4541444F, OID.1.3.6.1.4.1.16533.30.1=#1308454D504C4541444F - 1404001362"; //$NON-NLS-1$

        Assert.assertNotNull("No se pudo cargar el almacen de Windows", ksm); //$NON-NLS-1$

        final Certificate cert = ksm.getCertificate(ALIAS);
        Assert.assertNotNull("No se ha encontrado el certificado de prueba de ACA en su almacen de Windows", cert); //$NON-NLS-1$

        PrivateKeyEntry pke = null;
        try {
        	pke = ksm.getKeyEntry(
    			ALIAS
			);
        }
        catch (final Exception e) {
        	Logger.getLogger("es.gob.afirma").log(Level.WARNING, "No se pudo cargar la clave de firma", e); //$NON-NLS-1$ //$NON-NLS-2$
        }
        Assert.assertNotNull("No se pudo extraer la clave del almacen", pke); //$NON-NLS-1$
    }

    /** Prueba de CAPI para certificados ACA.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
	@Ignore
    public void testMSCapi() throws Exception {

    	final String ALIAS = "EA=demo.empleado@cgae.redabogacia.org, CN=NOMBRE EMPLEADO EMPLEADO DEMO - NIF 08967425R, OU=Informatica, O=Consejo General de la Abogac\u00EDa Espa\u00F1ola / CGAE / 2000, C=ES, ST=Madrid, OID.2.5.4.12=#1308506572736F6E616C, OID.1.3.6.1.4.1.4710.1.3.2=#1309513238363330303649, OID.2.5.4.5=#1309303839363734323552, OID.2.5.4.42=#130444454D4F, OID.2.5.4.4=#1308454D504C4541444F, OID.1.3.6.1.4.1.16533.30.1=#1308454D504C4541444F"; //$NON-NLS-1$

    	final KeyStore ks = KeyStore.getInstance("Windows-MY"); //$NON-NLS-1$
    	ks.load(null, null);

    	final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(ALIAS, null);

    	Assert.assertNotNull("No se ha recuperado la clave del certificado", pke); //$NON-NLS-1$
    }

    /** Prueba de PKCS#12 para certificados ACA.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testPkcs12() throws Exception {

    	final String ALIAS = "EA=demo.empleado@cgae.redabogacia.org, CN=NOMBRE EMPLEADO EMPLEADO DEMO - NIF 08967425R, OU=Informatica, O=Consejo General de la Abogac\u00EDa Espa\u00F1ola / CGAE / 2000, C=ES, ST=Madrid, OID.2.5.4.12=#1308506572736F6E616C, OID.1.3.6.1.4.1.4710.1.3.2=#1309513238363330303649, OID.2.5.4.5=#1309303839363734323552, OID.2.5.4.42=#130444454D4F, OID.2.5.4.4=#1308454D504C4541444F, OID.1.3.6.1.4.1.16533.30.1=#1308454D504C4541444F"; //$NON-NLS-1$
    	final char[] PWD = "adm2013".toCharArray(); //$NON-NLS-1$

    	final PrivateKeyEntry pke;
    	try (
			final InputStream ksIs = TestKeyStoreWindowsCertACA.class.getResourceAsStream("/ACA PF Administrativo Activo.p12"); //$NON-NLS-1$
		) {
	    	final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
	    	ks.load(ksIs, PWD);
	    	pke = (PrivateKeyEntry) ks.getEntry(ALIAS, new KeyStore.PasswordProtection(PWD));
    	}

    	Assert.assertNotNull("No se ha recuperado el par clave-certificado", pke); //$NON-NLS-1$
    	Assert.assertNotNull("No se ha recuperado la clave privada del certificado", pke.getPrivateKey()); //$NON-NLS-1$
    }
}
