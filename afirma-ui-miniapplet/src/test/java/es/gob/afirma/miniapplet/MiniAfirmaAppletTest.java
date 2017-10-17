/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.miniapplet;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;

/** Pruebas del MiniApplet.  */
public final class MiniAfirmaAppletTest {

	/** Prueba de obtenci&oacute;n del <code>CodeBase</code> del Applet cuando no est&aacute;
	 * publicado en una Web. */
	@SuppressWarnings("static-method")
	@Test
	public void testLocalCodebase() {
		System.out.println(new MiniAfirmaApplet().getCodeBase().toString());
	}

	/** Prueba de firma simple con DNIe. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita DNIe
	public void signWithDNIe() {
		final MiniAfirmaApplet applet = new MiniAfirmaApplet();
		try {
			applet.addData(Base64.encode("Hola Mundo!!".getBytes())); //$NON-NLS-1$
			applet.sign(
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

	/** Realiza una firma con el almac&eacute;n por defecto.
	 * @throws Exception Cuando ocurre alg&uacute;n error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita un almacen por defecto
	public void signWithDefault() throws Exception {
		final MiniAfirmaApplet applet = new MiniAfirmaApplet();

		applet.addData(Base64.encode("Hola".getBytes())); //$NON-NLS-1$
		System.out.println(
			applet.sign(
				"SHA1withRSA", //$NON-NLS-1$
				"CAdES", //$NON-NLS-1$
				null
			)
		);
		System.out.println("Done"); //$NON-NLS-1$
	}

	/** Prueba de firma de fichero grande y obtenci&oacute;n del resultado en porciones.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita GUI y almacen por defecto
	public void testLargeFileChunkedSupport() throws Exception {

		final String sourcePdfAsBase64 = Base64.encode(AOUtil.getDataFromInputStream(MiniAfirmaAppletTest.class.getResourceAsStream("/PDF_LARGER_THAN_2MB.pdf"))); //$NON-NLS-1$
		final InputStream is = new ByteArrayInputStream(sourcePdfAsBase64.getBytes());
		final int BUFFER_SIZE = 512 * 1024;
		byte[] buf;
		final MiniAfirmaApplet mini = new MiniAfirmaApplet();

		while (true) {
			final int available = is.available();
			if (available >= BUFFER_SIZE) {
				buf = new byte[BUFFER_SIZE];
			}
			else if (available > 0){
				buf = new byte[available];
			}
			else {
				break;
			}
			final int readed = is.read(buf);
			if (readed != buf.length) {
				throw new IOException("Lectura rara"); //$NON-NLS-1$
			}
			mini.addData(new String(buf));
		}

		final StringBuilder sb = new StringBuilder(
			mini.sign(
				"SHA1withRSA", AOSignConstants.SIGN_FORMAT_PDF, null //$NON-NLS-1$
			)
		);
		String ret = mini.getRemainingData();
		while (!"%%EOF%%".equals(ret)) { //$NON-NLS-1$
			sb.append(ret);
			ret = mini.getRemainingData();
		}

		// El resultado de la operacion es la concatenacion del
		// certificado utilizado y la firma resultante separados por '|'
		// String certB64 = sb.substring(0, sb.indexOf("|"));
		final String signatureB64 = sb.substring(sb.indexOf("|") + 1); //$NON-NLS-1$

		final File o = File.createTempFile("LARGE", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(o);
		) {
			fos.write(Base64.decode(signatureB64));
			fos.flush();
		}
	}
}
