/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.miniapplet;

import junit.framework.Assert;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;

/** Pruebas del MiniApplet.  */
public final class MiniAfirmaAppletTest {

	/** Prueba de firma simple con DNIe. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void signWithDNIe() {
		final MiniAfirmaApplet applet = new MiniAfirmaApplet();
		try {
			applet.sign(
					Base64.encode("Hola Mundo!!".getBytes()), //$NON-NLS-1$
					"SHA1withRSA", //$NON-NLS-1$
					"CAdES", //$NON-NLS-1$
						"mode=implicit\n" + //$NON-NLS-1$
						"Filter=DNIe:" //$NON-NLS-1$
			);
		}
		catch (final Exception e) {
			System.out.println("Error: " + e); //$NON-NLS-1$
			return;
		}
	}
	
	/** Prueba de obtenci&oacute;n de la estructura de firmantes.
	 * @throws Exception */
	@Test
	@SuppressWarnings("static-method")
	public void testGetSignersStructure() throws Exception {
	    final MiniAfirmaApplet applet = new MiniAfirmaApplet();
	    String ss = applet.getSignersStructure(Base64.encode(AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("SHA512withRSA.pdf")))); //$NON-NLS-1$
	    Assert.assertNotNull(ss);
	    System.out.println(ss);
	}
}
