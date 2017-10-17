/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.hash;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.file.Path;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/** Utilidades para el c&aacute;culo y comprobaci&oacute;n de huellas digitales.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
final class HashUtil {

	private static final int BUFFER_SIZE = 4096;

	private HashUtil() {
		// No instanciable
	}

	/** Genera la huella digital de un fichero.
	 * @param filePath Ruta completa del fichero.
	 * @param algorithm Algoritmo de huella.
	 * @return Valor de la huella.
	 * @throws IOException Si hay errores en el tratamiento del fichero.
	 * @throws FileNotFoundException Si el fichero indicado no existe.
	 * @throws NoSuchAlgorithmException Si no se encuentra el algoritmo de huella. */
	static byte[] getFileHash(final String algorithm, final Path filePath) throws NoSuchAlgorithmException, IOException {
		return getFileHash(algorithm, filePath.toFile());
	}

	/** Genera la huella digital de un fichero.
	 * @param fileName Nombre (con ruta) del fichero.
	 * @param algorithm Algoritmo de huella.
	 * @return Valor de la huella.
	 * @throws IOException Si hay errores en el tratamiento del fichero.
	 * @throws FileNotFoundException Si el fichero indicado no existe.
	 * @throws NoSuchAlgorithmException Si no se encuentra el algoritmo de huella. */
	static byte[] getFileHash(final String algorithm, final String fileName) throws NoSuchAlgorithmException, IOException {
		return getFileHash(algorithm, new File(fileName));
	}

	/** Genera la huella digital de un fichero.
	 * @param file Fichero del cual queremos calcular la huella.
	 * @param algorithm Algoritmo de huella.
	 * @return Valor de la huella.
	 * @throws IOException Si hay errores en el tratamiento del fichero.
	 * @throws FileNotFoundException Si el fichero indicado no existe.
	 * @throws NoSuchAlgorithmException Si no se encuentra el algoritmo de huella. */
	static byte[] getFileHash(final String algorithm, final File file) throws NoSuchAlgorithmException, IOException {
		final MessageDigest md = MessageDigest.getInstance(algorithm);
		final byte[] buffer = new byte[BUFFER_SIZE];
		try (
			final RandomAccessFile raf = new RandomAccessFile(file, "r"); //$NON-NLS-1$
			final FileChannel channel = raf.getChannel();
			final InputStream fis = Channels.newInputStream(channel);
			final InputStream dis = new DigestInputStream(fis, md);
		) {
			while (dis.read(buffer) != -1) { /* Vacio */ }
		}
		return md.digest();
	}

}
