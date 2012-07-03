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

import java.util.logging.Logger;

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
	 * @throws Exception Cuando se produce un error al cargar o procesar la firma. */
	@Test
	@SuppressWarnings("static-method")
	public void testGetSignersStructure() throws Exception {
	    final MiniAfirmaApplet applet = new MiniAfirmaApplet();

	    final byte[] signature = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("SHA512withRSA.pdf")); //$NON-NLS-1$
	    Logger.getLogger("es.gob.afirma").info("Tamano de la firma (bytes): " + signature.length); //$NON-NLS-1$ //$NON-NLS-2$

	    try {
	    	AOUtil.classForName("es.gob.afirma.signers.pades.AOPDFSigner"); //$NON-NLS-1$
	    	Logger.getLogger("es.gob.afirma").info("Se ha encontrado el Signer de PAdES"); //$NON-NLS-1$ //$NON-NLS-2$
	    } catch (final Exception e) {
	    	Logger.getLogger("es.gob.afirma").severe("El Signer de PAdES no esta enlazado: " + e); //$NON-NLS-1$ //$NON-NLS-2$
	    	throw e;
		}

	    final String ss = applet.getSignersStructure(Base64.encode(signature));
	    Assert.assertNotNull(ss);
	    System.out.println(ss);
	}

	@Test
	@Ignore
	public void signWithMozilla() throws Exception {
		final MiniAfirmaApplet applet = new MiniAfirmaApplet();

		System.out.println(
				applet.sign(
						Base64.encode("Hola".getBytes()),
						"SHA1withRSA",
						"CAdES",
						null)
		);


	}
}
