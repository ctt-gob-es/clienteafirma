/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.misc;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.logging.Logger;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;

/** Clase con m&eacute;todos para el trabajo con ficheros. */
public final class AOFileUtils {

	private AOFileUtils() {
		// No permitimos la instanciacion
	}

	private static final String SHORTENER_ELLIPSE = "..."; //$NON-NLS-1$

	/** Guarda los datos en un temporal.
	 * @param data Datos a guardar.
	 * @return Fichero temporal.
	 * @throws IOException Cuando ocurre un error al leer los datos o crear el temporal. */
	public static File createTempFile(final byte[] data) throws IOException {

		// Creamos un fichero temporal
		final File tempFile = File.createTempFile("afirma", null); //$NON-NLS-1$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(data);
			fos.flush();
			fos.close();
		}
		return tempFile;
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

	/**
	 * Comprueba si los datos proporcionados son un XML v&aacute;lido.
	 * @param data Datos a evaluar.
	 * @return {@code true} cuando los datos son un XML bien formado. {@code false}
	 * en caso contrario.
	 */
    public static boolean isXML(final byte[] data) {

    	final SAXParserFactory factory = SAXParserFactory.newInstance();
    	factory.setValidating(false);
    	factory.setNamespaceAware(true);

    	try {
    		final SAXParser parser = factory.newSAXParser();
    		final XMLReader reader = parser.getXMLReader();
    		reader.setErrorHandler(
				new ErrorHandler() {
					@Override
					public void warning(final SAXParseException e) {
						log(e);
					}
					@Override
					public void fatalError(final SAXParseException e) {
						log(e);
					}
					@Override
					public void error(final SAXParseException e) {
						log(e);
					}
					private void log(final Exception e) {
						Logger.getLogger("es.gob.afirma").fine("El documento no es un XML: " + e); //$NON-NLS-1$ //$NON-NLS-2$
					}
				}
			);
    		reader.parse(new InputSource(new ByteArrayInputStream(data)));
    	}
    	catch (final Exception e) {
    		return false;
    	}
    	return true;
    }
}
