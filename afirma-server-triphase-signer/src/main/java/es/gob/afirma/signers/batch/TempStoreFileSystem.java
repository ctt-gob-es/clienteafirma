/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.batch;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.MessageDigest;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;

final class TempStoreFileSystem implements TempStore {

	/** N&uacute;mero de recuperaciones de fichero que realizaremos antes de iniciar un
	 * proceso de limpieza de temporales caducados. */
	private static final int RETRIEVES_BEFORE_CLEANING = 1000;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
	private static final MessageDigest MD;
	static {
		try {
			MD = MessageDigest.getInstance("SHA-1"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			throw new IllegalStateException(
				"No se ha podido cargar el motor de huellas para SHA-1: " + e, e //$NON-NLS-1$
			);
		}
	}

	private static int currentRetrievesBeforeCleaning = 0;



	@Override
	public void store(final byte[] dataToSave, final SingleSign ss, final String batchId) throws IOException {
		store(dataToSave, getFilename(ss, batchId));
		LOGGER.info("Firma '" + ss.getId() + "' almacenada temporalmente en " + getFilename(ss, batchId)); //$NON-NLS-1$ //$NON-NLS-2$
	}

	@Override
	public void store(final byte[] dataToSave, final String filename) throws IOException {
		try (
			final OutputStream fos = new FileOutputStream(
				new File(
					BatchConfigManager.getTempDir(),
					filename
				)
			);
			final BufferedOutputStream bos = new BufferedOutputStream(
				fos,
				dataToSave.length
			);
		) {
			bos.write(dataToSave);
			bos.flush();
			// Cerramos explicitamente
			bos.close();
		}
	}

	@Override
	public byte[] retrieve(final SingleSign ss, final String batchId) throws IOException {
		return retrieve(getFilename(ss, batchId));
	}

	@Override
	public byte[] retrieve(final String filename) throws IOException {

		if (!TempStoreFileSystemCleaner.isRunningCleaning()) {
			if (currentRetrievesBeforeCleaning >= RETRIEVES_BEFORE_CLEANING) {
				currentRetrievesBeforeCleaning = 0;
				new Thread(new TempStoreFileSystemCleaner()).start();
			}
			else {
				currentRetrievesBeforeCleaning++;
			}
		}

		byte[] ret;
		try (
			final InputStream fis = new FileInputStream(
				new File(
					BatchConfigManager.getTempDir(),
					filename
				)
			);
			final InputStream bis = new BufferedInputStream(fis);
		) {
			ret = AOUtil.getDataFromInputStream(bis);
		}
		return ret;
	}

	@Override
	public void delete(final SingleSign ss, final String batchId) {
		delete(getFilename(ss, batchId));
	}

	@Override
	public void delete(final String filename) {
		final File f = new File(
				BatchConfigManager.getTempDir(),
				filename
				);
		if (f.exists()) {
			f.delete();
		}
	}

	private static String getFilename(final SingleSign ss, final String batchId) {
		byte[] id = ss.getId().getBytes();
		
		// Si el ID supera los 20 caracteres, usaremos en su lugar el hash
		// Esto podria llevar a colisiones si se indican ID superiores a 20
		// caracteres con un hash que resulta ser el ID de otro documento,
		// pero esto no deberia ocurrir nunca y, de esta forma, no se
		// tendra que calcular el hash siempre. Cuando se calcula siempre
		// el hash, se ha comprobado que el espacio de tiempo que transcurre
		// entre generar el fichero temporal con la firma y que se recupere
		// es demasiado corto y a veces no se encuentra ese fichero 
		if (id.length > 20) {
			id = MD.digest(id);
		}
		return Base64.encode(id, true) + "." + batchId; //$NON-NLS-1$
	}
}
