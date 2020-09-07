/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.misc;

import java.io.IOException;
import java.util.Properties;

import org.junit.Assert;
import org.junit.Test;

/** M&eacute;todos generales de utilidad para toda la aplicaci&oacute;n.
 * @version 0.3
 */
public final class TestAOUtil {

	/** Prueba los m&eacute;todos de <code>Platform</code> */
	@SuppressWarnings("static-method")
	@Test
	public void testPlatform() {
		System.out.println(Platform.getSystemLibDir());
	}

	/** Prueba de paso a formato URI de ruta de fichero.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testCreateUri() throws Exception {
		System.out.println(AOUtil.createURI("c:\\kaka\\naca\\/das des\\a.txt")); //$NON-NLS-1$
	}

    /** Prueba para el getCN(), reconstruido para prescindir de <code>javax.naming</code>. */
    @SuppressWarnings("static-method")
	@Test
    public void testGetCN() {
        final String principals[][] = {
                {"CN=\"fulanito Menganito\", C=ES", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {" CN=\"fulanito Menganito\", C=ES", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"CN=\"fulanito, Menganito\", C=ES", "fulanito, Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"CN=fulanito Menganito, C=ES", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"CN=fulanito Menganito , C=ES", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"CN = fulanito Menganito, C=ES", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"CN=fulanito Menganito, C=ES", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, CN=fulanito Menganito", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES,CN=fulanito Menganito", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES,CN =fulanito Menganito", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES,CN = fulanito Menganito", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES,CN=fulanito Menganito", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES,CN =\"fulanito Menganito\"", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES,CN= \"fulanito Menganito\"", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"OU=\"mi organizacion\", C=ES", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {" OU=\"mi organizacion\", C=ES", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"OU=\"mi organizacion\", C=ES", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"OU=mi organizacion, C=ES", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"OU=mi organizacion , C=ES", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"OU = mi organizacion, C=ES", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"OU=mi organizacion, C=ES", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, OU=mi organizacion", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, OU = mi organizacion", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES,OU=mi organizacion", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES,OU = mi organizacion", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES,OU=\"mi organizacion\"", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES,OU= \"mi organizacion\"", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"PCN=mi organizacion, C=ES", "mi organizacion"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"CN=, C=ES", ""}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, PCN=fulanito Menganito", "ES"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, P CN=\"fulanito Menganito\"", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, P ,CN=\"fulanito Menganito\"", "fulanito Menganito"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, CN=\"\"", ""}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, CN=", ""}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, CN=\"", ""}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, CN=\" ", " "}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, CN", "ES"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"C=ES, CN a", "ES"}, //$NON-NLS-1$ //$NON-NLS-2$
                {"Hola Mundo!!", "Hola Mundo!!"}, //$NON-NLS-1$ //$NON-NLS-2$
                {null, null}
        };

        for (final String principal[] : principals) {
            System.out.println(principal[0] + ": " + principal[1]); //$NON-NLS-1$
            Assert.assertEquals("El metodo getCN() no devuelve el resultado correcto para " + principal[0], AOUtil.getCN(principal[0]), principal[1]); //$NON-NLS-1$
        }
    }

    /** Prueba de conversi&oacute;n de Base64 a <code>Properties</code>.
     * @throws IOException En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testBase64ToProperties() throws IOException {

    	final String base64 = "Iw0KI1RodSBTZXAgMTkgMTY6MDc6MDAgQ0VTVCAyMDE5DQpzZXJ2ZXJVcmw9aHR0cHNcOi8vc2Vydmlkb3JjZW50cmFsXDo4NDQzL2ZpcmUtc2lnbmF0dXJlL3B1YmxpYy9hZmlybWEvdHJpcGhhc2VTaWduU2VydmljZQ0KaWduYXR1cmVSb3RhdGlvbj10cnVlDQpsYXllcjJUZXh0PUZpcm1hZG8gZWxlY3Ryw7NuaWNhbWVudGUgcG9yIEZJREVMIE1BUlRJTkVaIEZFUk5BTkRFWiBlbCBkw61hICQkU0lHTkRBVEVcPWRkL01NL3l5eXkgYSBsYXMgSEhcOm1tXDpzcyQkLg0KbGF5ZXIyRm9udFNpemU9Ng0Kc2lnbmF0dXJlRmllbGQ9RW1wdHlTaWduYXR1cmVGaWVsZA0KbGF5ZXIyRm9udEZhbWlseT0xDQphcHBseVN5c3RlbURhdGU9ZmFsc2UNCg=="; //$NON-NLS-1$
    	final Properties prop = AOUtil.base642Properties(base64);
    	for (final String k : prop.keySet().toArray(new String[0])) {
    		System.out.println(k + ": " + prop.getProperty(k) + "\n"); //$NON-NLS-1$ //$NON-NLS-2$
    	}
    	System.out.println(prop.toString());
    }
}

