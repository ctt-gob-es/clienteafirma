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
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.attribute.AclEntry;
import java.nio.file.attribute.AclEntryFlag;
import java.nio.file.attribute.AclEntryPermission;
import java.nio.file.attribute.AclEntryType;
import java.nio.file.attribute.AclFileAttributeView;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.UserPrincipal;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.parsers.SAXParser;

import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;

/** Clase con m&eacute;todos para el trabajo con ficheros. */
public final class AOFileUtils {

	private static final String SHORTENER_ELLIPSE = "..."; //$NON-NLS-1$

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Indica si el sistema admite atributos POSIX. */
	private static boolean posixSupported = false;
	/** Valor bandera para indicar si la variable posixSupported se ha inicializado. */
	private static boolean posixSupportDefined = false;


	private AOFileUtils() {
		// No permitimos la instanciacion
	}

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

    	try {
    		final SAXParser parser = SecureXmlBuilder.getSecureSAXParser();
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
						LOGGER.fine("El documento no es un XML: " + e); //$NON-NLS-1$
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


	/**
	 * Establece permisos de lectura y escritura de un fichero para todos los usuarios.
	 * No lanza error en ning&uacute;n caso.
	 * @param file Fichero del que establecer los permisos.
	 */
	public static void setAllPermissions(final File file) {

		// Hacemos un establecimiento basico de permisos
		try {
			file.setReadable(true, false);
			file.setWritable(true, false);
			file.setExecutable(true, false);
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING,
					"No se han podido establecer los permisos del archivo " //$NON-NLS-1$
							+ LoggerUtil.getCleanUserHomePath(file.getAbsolutePath()), e);
		}

		// Si se soportan las listas de control de acceso, establecemos con ellas los mismos
		// permisos que elementos que sepamos que tienen los mismos permisos que queremos
		final AclFileAttributeView acl = getAclAttributes(file);
		if (acl != null) {

			try {
				final List<AclEntry> aclList = acl.getAcl();
				final List<AclEntry> newAclList = new ArrayList<>();

				for (final AclEntry entry : aclList) {
					final UserPrincipal principal = entry.principal();
					final AclEntry newEntry = AclEntry.newBuilder()
							.setType(AclEntryType.ALLOW)
							.setPrincipal(principal)
							.setFlags(
									AclEntryFlag.DIRECTORY_INHERIT,
									AclEntryFlag.FILE_INHERIT)
							.setPermissions(
									AclEntryPermission.WRITE_NAMED_ATTRS,
									AclEntryPermission.WRITE_ATTRIBUTES,
									AclEntryPermission.DELETE,
									AclEntryPermission.WRITE_DATA,
									AclEntryPermission.READ_ACL,
									AclEntryPermission.APPEND_DATA,
									AclEntryPermission.READ_ATTRIBUTES,
									AclEntryPermission.READ_DATA,
									AclEntryPermission.EXECUTE,
									AclEntryPermission.SYNCHRONIZE,
									AclEntryPermission.READ_NAMED_ATTRS,
									AclEntryPermission.DELETE,
									AclEntryPermission.DELETE_CHILD)
							.build();
					newAclList.add(newEntry);
				}
				acl.setAcl(newAclList);
			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING,
						"No se ha podido establecer la ACL del archivo " //$NON-NLS-1$
						+ LoggerUtil.getCleanUserHomePath(file.getAbsolutePath()), e);
			}
		}

		// Si se soporta POSIX, establecemos los permisos con el
		if (isPosixSupported()) {
			final Set<PosixFilePermission> perms = new HashSet<>();
			perms.add(PosixFilePermission.OWNER_READ);
			perms.add(PosixFilePermission.GROUP_READ);
			perms.add(PosixFilePermission.OTHERS_READ);
			perms.add(PosixFilePermission.OWNER_WRITE);
			perms.add(PosixFilePermission.GROUP_WRITE);
			perms.add(PosixFilePermission.OTHERS_WRITE);
			perms.add(PosixFilePermission.OWNER_EXECUTE);
			perms.add(PosixFilePermission.GROUP_EXECUTE);
			perms.add(PosixFilePermission.OTHERS_EXECUTE);
			try {
				Files.setPosixFilePermissions(file.toPath(), perms);
			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING,
						"No se han podido dar permisos posix sobre el archivo " //$NON-NLS-1$
						+ LoggerUtil.getCleanUserHomePath(file.getAbsolutePath()), e);
			}
		}
	}

	/**
	 * Indica si el sistema de ficheros admite el uso de atributos POSIX.
	 * @return {@code true} si admite atributos POSIX, {@code false} en caso contrario.
	 */
	private static boolean isPosixSupported() {
		if (!posixSupportDefined) {
			posixSupported = FileSystems.getDefault().supportedFileAttributeViews().contains("posix"); //$NON-NLS-1$
			posixSupportDefined = true;
		}
		return posixSupported;
	}


	/**
	 * Obtiene la ACL con los permisos del fichero indicado.
	 * @param file Fichero del que obtener los permisos.
	 * @return ACL asociada al fichero.
	 */
	private static AclFileAttributeView getAclAttributes(final File file) {
	    return Files.getFileAttributeView(file.toPath(), AclFileAttributeView.class);
	}
}
