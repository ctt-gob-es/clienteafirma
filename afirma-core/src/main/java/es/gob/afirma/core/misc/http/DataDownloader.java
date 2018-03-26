/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.misc.http;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.logging.Logger;
import java.util.zip.GZIPInputStream;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;

/** Utilidades para la descarga de datos.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class DataDownloader {

	private static final int GUNZIP_BUFFER_SIZE = 1024;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private DataDownloader() {
		// No instanciable
	}

	/** Descarga datos del origen indicado.
	 * @param ds Origen de los datos. Este puede ser, y se eval&uacute;a en este orden:
	 *           <ol>
	 *            <li>Una URL de tipo HTTP, HTTPS o FTP.</li>
	 *            <li>Datos en formato Base64.</li>
	 *            <li>Datos textuales (en este caso de devuelven ellos mismos como binario).</li>
	 *           </ol>
	 * @param gzipped Indica si los datos de entrada est&aacute;n comprimidos con GZIP y deben devolverse
	 *                descomprimidos.
	 * @return Datos obtenidos del origen indicado.
	 * @throws IOException Si no se pueden obtener los datos. */
	public static byte[] downloadData(final String ds, final boolean gzipped) throws IOException {
		if (gzipped && Base64.isBase64(ds.getBytes())) {
			LOGGER.info(
				"Se ha indicado que los datos de entrada estan comrpimidos con GZIP" //$NON-NLS-1$
			);
			return downloadData(
				Base64.encode(
					gunzipBytes(
						Base64.decode(ds.replace("_", "/").replace("-", "+")) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
					)
				)
			);
		}
		return downloadData(ds);
	}

	/** Descarga datos del origen indicado.
	 * @param ds Origen de los datos. Este puede ser, y se eval&uacute;a en este orden:
	 *           <ol>
	 *            <li>Una URL de tipo HTTP, HTTPS o FTP.</li>
	 *            <li>Datos en formato Base64.</li>
	 *            <li>Datos textuales (en este caso de devuelven ellos mismos como binario).</li>
	 *           </ol>
	 * @return Datos obtenidos del origen indicado.
	 * @throws IOException Si no se pueden obtener los datos. */
	public static byte[] downloadData(final String ds) throws IOException {

		if (ds == null) {
			throw new IllegalArgumentException("La fuente de datos no puede ser nula"); //$NON-NLS-1$
		}

		final String dataSource = ds.trim();

		// Miramos primero si los datos son una URL, en cuyo caso descargamos los datos
		if (dataSource.startsWith("http://") || dataSource.startsWith("https://")) { //$NON-NLS-1$ //$NON-NLS-2$
			return UrlHttpManagerFactory.getInstalledManager().readUrl(dataSource, UrlHttpMethod.GET);
		}

		if (dataSource.startsWith("ftp://")) { //$NON-NLS-1$
			try (
				final InputStream ftpStream = new URL(dataSource).openStream();
			) {
				return AOUtil.getDataFromInputStream(ftpStream);
			}
		}

		if (dataSource.startsWith("file:/")) { //$NON-NLS-1$
			try (
				final InputStream is = AOUtil.loadFile(new URI(dataSource));
				final InputStream bis = new BufferedInputStream(is);
			) {
				return AOUtil.getDataFromInputStream(bis);
			}
			catch (final URISyntaxException e) {
				throw new IOException(
					"Error leyendo el fichero (" + dataSource + "): " + e, e //$NON-NLS-1$ //$NON-NLS-2$
				);
			}
		}

		// No son URL, son los datos en si

		// Comprobamos que los datos se pueden tratar como base 64
		if (Base64.isBase64(dataSource.getBytes())) {
			LOGGER.info("El contenido a obtener es Base64"); //$NON-NLS-1$
			try {
				return Base64.decode(dataSource.replace("_", "/").replace("-", "+")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			}
			catch (final Exception e) {
				LOGGER.warning(
					"Los datos introducidos no se pueden tratar como Base64: " + e //$NON-NLS-1$
				);
			}
		}

		return dataSource.getBytes();
	}

	private static byte[] gunzipBytes(final byte[] compressedData) throws IOException {
        try (
    		final ByteArrayInputStream bis = new ByteArrayInputStream(compressedData);
    		final ByteArrayOutputStream bos = new ByteArrayOutputStream();
            final GZIPInputStream gzipIS = new GZIPInputStream(bis)
		) {
            final byte[] buffer = new byte[GUNZIP_BUFFER_SIZE];
            int len;
            while ((len = gzipIS.read(buffer)) != -1) {
                bos.write(buffer, 0, len);
            }
            return bos.toByteArray();
        }
	}
}
