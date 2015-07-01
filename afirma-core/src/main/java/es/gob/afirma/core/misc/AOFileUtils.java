/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.core.misc;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.zip.ZipFile;

/** Clase con m&eacute;todos para el trabajo con ficheros. */
public final class AOFileUtils {

	private AOFileUtils() {
		// No permitimos la instanciacion
	}

	private static final String SHORTENER_ELLIPSE = "..."; //$NON-NLS-1$

	/** Crea un fichero ZIP en disco apto para manejarse.
	 * @param zipFileData Los datos del zip.
	 * @return Fichero Zip.
	 * @throws java.util.zip.ZipException Cuando los datos no eran realmente un Zip.
	 * @throws IOException Cuando ocurre un error al leer los datos o crear el temporal
	 *                     para abrir el Zip. */
	public static ZipFile createTempZipFile(final byte[] zipFileData)
			throws IOException {

		// Creamos un fichero temporal
		final File tempFile = File.createTempFile("afirmazip", null); //$NON-NLS-1$
		final FileOutputStream fos = new FileOutputStream(tempFile);
		fos.write(zipFileData);
		fos.flush();
		fos.close();
		tempFile.deleteOnExit();
		return new ZipFile(tempFile);
	}

	/** Acorta, con puntos suspensivos en la mitad, un nombre de ruta de fichero.
	 * Inspirado en: <a href="http://www.rgagnon.com/javadetails/java-0661.html">http://www.rgagnon.com/javadetails/java-0661.html</a>.
	 * @param path Nombre de ruta de fichero.
	 * @param limit L&iacute;mite de caracteres en el nombre de salida.
	 * @return Nombre de ruta de fichero acortado, con puntos suspensivos en la mitad,
	 *         al n&uacute;mero l&iacute;mite de caracteres indicado. */
	public static String pathLengthShortener(final String path, final int limit) {

		if (path == null) {
			throw new IllegalArgumentException("El numbre de la ruta no puede ser nulo"); //$NON-NLS-1$
		}


		if (path.length() <= limit) {
			return path;
		}

		final char shortPathArray[] = new char[limit];
		final char pathArray[] = path.toCharArray();
		final char ellipseArray[] = SHORTENER_ELLIPSE.toCharArray();

		final int pathindex = pathArray.length - 1;
		final int shortpathindex = limit - 1;

		int i = 0;
		for (; i < limit; i++) {
			if (pathArray[pathindex - i] != '/' && pathArray[pathindex - i] != '\\') {
				shortPathArray[shortpathindex - i] = pathArray[pathindex - i];
			}
			else {
				break;
			}
		}

		final int free = limit - i;

		if (free < SHORTENER_ELLIPSE.length()) {
			System.arraycopy(ellipseArray, 0, shortPathArray, 0, ellipseArray.length);
		}
		else {
			int j = 0;
			for (; j + ellipseArray.length < free; j++) {
				shortPathArray[j] = pathArray[j];
			}
			for (int k = 0; j + k < free; k++) {
				shortPathArray[j + k] = ellipseArray[k];
			}
		}
		return new String(shortPathArray);
	}

}
