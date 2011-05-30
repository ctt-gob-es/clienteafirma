/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.cliente.utilidades.browser;

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URLEncoder;
import java.security.DigestOutputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.logging.Logger;

import org.apache.commons.codec.binary.Base64;

public final class FirmadorWeb {

	public static class FirmaWeb {

		public final File tmpWebDataFile;

		FirmaWeb(final byte[] hash, final String hashAlgorithm,
				final File tmpWebDataFile) {
			this.tmpWebDataFile = tmpWebDataFile;
		}
	}

	FirmaWeb firmar(final String html, final Attachment[] attachments,
			final String hashAlgorithm) throws IOException,
			NoSuchAlgorithmException {
		byte[] hash;
		File tmpWebDataFile = File.createTempFile("afirma5_firmaweb", ".tmp");
		tmpWebDataFile.deleteOnExit();

		OutputStream os = new FileOutputStream(tmpWebDataFile);
		try {
			// Usamos el DigestOutputStream para calcular el hash a la vez que
			// lo escribimos
			DigestOutputStream dos = new DigestOutputStream(os,
					MessageDigest.getInstance(hashAlgorithm.toUpperCase()));

			// Lo usaremos para escribir los ficheros en b64
			BufferedOutputStream bos = new BufferedOutputStream(dos);
			// Base64OutputStrea b64os= new Base64OutputStream(dos);

			// Escribimos el HTML
			dos.write(html.getBytes());
			dos.flush();

			// Anadimos los ficheros
			for (int i = 0; i < attachments.length; i++) {
				Attachment attach = attachments[i];

				if (attach.url.trim().length() > 0) {
					// Escribimos el tag de apertura
					final String openTag = "<afirma type='filecontent' path='"
							+ URLEncoder.encode(attach.url, "UTF-8")
							+ "'><!--\n";
					dos.write(openTag.getBytes());
					dos.flush();

					// Leemos el fichero con ventana grafica
					final FileInputStream attachIS = attach
							.getContentInputStream();
					try {
						// Volcamos el fichero en b64
						Logger.getLogger("es.map,afirma").info(
								"Tamano del buffer: " + 1024);
						int nBytes;
						byte[] buffer = new byte[1024];
						ByteArrayOutputStream baos = new ByteArrayOutputStream();
						while ((nBytes = attachIS.read(buffer)) != -1) {
							baos.write(buffer, 0, nBytes);
						}
						try {
							attachIS.close();
						} catch (Exception e) {
							Logger.getLogger("es.gob.afirma")
									.warning(
											"No se pudo cerrar el flujo del fichero adjunto");
						}

						bos.write(Base64.encodeBase64(baos.toByteArray()));
						bos.flush();
					} finally {
						// Cerramos el fichero que estamos leyendo
						attachIS.close();
					}

					// Escribimos el tag de cierre
					String closeTag = "\n--></afirma>";
					dos.write(closeTag.getBytes());
					dos.flush();
				}
			}

			tryClose(bos);
			tryClose(dos);

			hash = dos.getMessageDigest().digest();
		} finally {
			tryClose(os);
		}

		return new FirmaWeb(hash, hashAlgorithm, tmpWebDataFile);
	}

	private void tryClose(OutputStream os) {
		try {
			if (os != null)
				os.close();
		} catch (IOException e) {
		}
	}
}
