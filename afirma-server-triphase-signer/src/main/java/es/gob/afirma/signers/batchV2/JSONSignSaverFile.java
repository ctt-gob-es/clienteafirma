/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.batchV2;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.triphase.server.document.DocumentManagerBase;

/** Guarda firmas envi&aacute;ndolas a un servicio HTTP POST.
 * <b>Esta clase es &uacute;nicamente un ejemplo de implementaci&oacute;n del interfaz <code>SignSaver</code>
 * para depuraci&oacute;n, <u>nunca</u> debe usarse en entornos reales</b> (no hay comprobaciones de
 * qu&eacute; ficheros pueden sobrescribirse).*/
public final class JSONSignSaverFile extends DocumentManagerBase {

	private static final String PROP_FILENAME = "outdir"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private String outDir;

	/** Constructor vac&iacute;o. */
	public JSONSignSaverFile() {
		// Vacio
	}

	@Override
	public void init(final Properties config) {
		if (config == null) {
			throw new IllegalArgumentException(
				"La configuracion no puede ser nula" //$NON-NLS-1$
			);
		}
		final String file = config.getProperty(PROP_FILENAME);
		if (file == null) {
			throw new IllegalArgumentException(
				"Es obligarorio que la configuracion incluya un valor para la propiedad " + PROP_FILENAME //$NON-NLS-1$
			);
		}
		this.outDir = file;
	}

	@Override
	public byte[] getDocument(final String reference, final X509Certificate[] certChain, final Properties config) throws IOException {
		LOGGER.info("Recuperamos el documento con referencia: " + reference); //$NON-NLS-1$

		final File file = new File(new String(Base64.decode(reference)));

		LOGGER.info("Buscamos el fichero: " + file.getAbsolutePath()); //$NON-NLS-1$

		if (!file.exists()) {
			throw new IOException("No se puede cargar el documento, no existe"); //$NON-NLS-1$
		}

		if (!file.isFile()) {
			throw new IOException(
				"No se puede cargar el documento, el elmento existe, pero no es un fichero" //$NON-NLS-1$
			);
		}

		if (!file.canRead()) {
			throw new IOException(
				"No se puede cargar el documento, no se tienen permisos de lectura sobre el" //$NON-NLS-1$
			);
		}

		final byte[] data;
		try (
			final InputStream fis = new FileInputStream(file);
		) {
			data = AOUtil.getDataFromInputStream(fis);
			fis.close();
		}
		catch (final IOException e) {
			LOGGER.warning("Error en la lectura del fichero '" + file.getAbsolutePath() + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
			throw e;
		}

		return data;
	}

	@Override
	public String storeDocument(final String sSignId, final X509Certificate[] certChain, final byte[] data, final Properties config) throws IOException {
		final File f = new File(this.outDir + File.separator + sSignId);
		if (!f.getParentFile().isDirectory()) {
			throw new IOException(
					"El directorio de guardado de la firma no existe: " + f.getParent() //$NON-NLS-1$
					);
		}
		try (
				final OutputStream fos = new FileOutputStream(this.outDir + File.separator + sSignId);
				final BufferedOutputStream bos = new BufferedOutputStream(
						fos,
						data.length
						)
				) {
			bos.write(data);
			bos.flush();
		}
		LOGGER.fine("Guardada finalmente la firma '" + sSignId + "' en: " + this.outDir); //$NON-NLS-1$ //$NON-NLS-2$

	return this.outDir + File.separator + sSignId;
}
}
