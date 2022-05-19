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
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signers.batch.xml.SingleSign;
import es.gob.afirma.triphase.server.ConfigManager;

final class TempStoreFileSystem implements TempStore {

	/** N&uacute;mero de recuperaciones de fichero que realizaremos antes de iniciar un
	 * proceso de limpieza de temporales caducados. */
	private static final int RETRIEVES_BEFORE_CLEANING = 1000;

	private static File tempDir;

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

		if (ConfigManager.getTempDir() != null) {
			tempDir = ConfigManager.getTempDir();
		}
		else {
			tempDir = BatchConfigManager.getTempDir();
		}
	}

	private static int currentRetrievesBeforeCleaning = 0;



	@Override
	public void store(final byte[] dataToSave, final SingleSign ss, final String batchId) throws IOException {
		store(dataToSave, getFilename(ss, batchId));
		LOGGER.fine("Firma '" + ss.getId() + "' almacenada temporalmente en " + getFilename(ss, batchId)); //$NON-NLS-1$ //$NON-NLS-2$
	}

	@Override
	public void store(final byte[] dataToSave, final String filename) throws IOException {
		try (
			final OutputStream fos = new FileOutputStream(new File(tempDir, filename));
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
				final ExecutorService executor = Executors.newSingleThreadExecutor();
				try {
					executor.execute(new TempStoreFileSystemCleaner(tempDir));
				}
				catch (final Exception e) {
					LOGGER.warning("No se pudo ejecutar el hilo de limpieza de de temporales: " + e); //$NON-NLS-1$
					stopExecution(executor);
				}
			}
			else {
				currentRetrievesBeforeCleaning++;
			}
		}

		byte[] ret;
		try (
			final InputStream fis = new FileInputStream(new File(tempDir, filename));
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
		new File(tempDir, filename).delete();
	}

	private static String getFilename(final SingleSign ss, final String batchId) {
		byte[] id = ss.getId().getBytes(StandardCharsets.UTF_8);

		// Se calcula el hash del identificador de fichero para usarlo como nombre
		// del fichero temporal. De esta forma, nos aseguramos de que el nombre
		// tenga una longitud predefinida
		synchronized (MD) {
			id = MD.digest(id);
			MD.reset();
		}

		return Base64.encode(id, true) + "." + batchId; //$NON-NLS-1$
	}

	/**
	 * Detiene la ejecuci&oacute;n de las operaciones concurrentes.
	 * @param executorService Servicio de ejecuci&oacute;n.
	 */
	private static void stopExecution(final ExecutorService executorService) {
		executorService.shutdown();
		try {
		    if (!executorService.awaitTermination(200, TimeUnit.MILLISECONDS)) {
		        executorService.shutdownNow();
		    }
		}
		catch (final InterruptedException ex) {
			LOGGER.warning(
				"Error intentando hacer una parada controlada de hilo de limpieza de temporales: " + ex //$NON-NLS-1$
			);
		    executorService.shutdownNow();
		}
	}
}
