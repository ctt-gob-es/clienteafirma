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
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Path;
import java.security.NoSuchAlgorithmException;

/** Utilidades para el c&aacute;culo y comprobaci&oacute;n de huellas digitales.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
final class HashUtil {

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
	 * @param dataFile Fichero del cual queremos calcular la huella.
	 * @param algorithm Algoritmo de huella.
	 * @return Valor de la huella.
	 * @throws IOException Si hay errores en el tratamiento del fichero.
	 * @throws FileNotFoundException Si el fichero indicado no existe.
	 * @throws NoSuchAlgorithmException Si no se encuentra el algoritmo de huella. */
	static byte[] getFileHash(final String algorithm, final File dataFile) throws NoSuchAlgorithmException, IOException {

		byte[] result;
		final DigestManager digestManager = new DigestManager(algorithm, null);
		try (	final FileInputStream in = new FileInputStream(dataFile);
				final DigestManagerInputStream managerStreamHash = new DigestManagerInputStream(in, digestManager);) {
			managerStreamHash.readOptimized(DigestManager.SIZE_100KB);
			result = managerStreamHash.digest();
		}
		return result;
	}
}
