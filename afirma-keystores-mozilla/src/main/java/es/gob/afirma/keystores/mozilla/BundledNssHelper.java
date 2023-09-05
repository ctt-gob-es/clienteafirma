/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.mozilla;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.KeyStoreUtilities;

final class BundledNssHelper {

	private static final int BUFFER_SIZE = 2048;

	/** Directorio del NSS propio. */
	public static final String AFIRMA_NSS_HOME =
		Platform.getUserHome() + File.separator +
			".afirma" + File.separator + //$NON-NLS-1$
				"nss" + File.separator + //$NON-NLS-1$
					Platform.getJavaArch();

	/** Desempaqueta el NSS contenido dentro de la aplicaci&oacute;n y devuelve el directorio en el
	 * que se puede encontrar, de tal forma que este puede ser utilizado directamente en la
	 * configuraci&oacute;n de la carga del PKCS#11 de NSS.
	 * @return Ruta del directorio de NSS.
	 * @throws IOException Cuando no ha sido posible obtener la ruta del fichero. */
	static String getBundledNssDirectory() throws IOException {
		final File bundledNssDir = getNssDirFile();

		uncompressZip(getNssZipResourceName(), bundledNssDir);

		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			return KeyStoreUtilities.getWindowsShortName(bundledNssDir.getAbsolutePath());
		}
		final String ret = bundledNssDir.getAbsolutePath();
		Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
			"Se ha encontrado un NSS incorporado en: " + ret.replace(System.getProperty("user.home"), "USERHOME") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		);
		return ret;
	}

	private static String getNssZipResourceName() {
		return "/nss/" + Platform.getOS().toString() + "/nss" + Platform.getJavaArch() + ".zip"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}

	private static File getNssDirFile() throws IOException {
		// Nos aseguramos de que tenemos el diretorio para NSS
		final File bundledNssDir = new File(AFIRMA_NSS_HOME);
		if (!bundledNssDir.exists() && !bundledNssDir.mkdirs()) {
			throw new IOException("No se ha podido crear el directorio de NSS: " + AFIRMA_NSS_HOME); //$NON-NLS-1$
		}
		if (!bundledNssDir.isDirectory()) {
			throw new IOException("No se ha podido crear el directorio de NSS por encontrarse un fichero con el mismo nombre: " + AFIRMA_NSS_HOME); //$NON-NLS-1$
		}
		if (!bundledNssDir.canWrite() || !bundledNssDir.canRead()) {
			throw new IOException("No se tienen permisos sobre el directorio de NSS: " + AFIRMA_NSS_HOME); //$NON-NLS-1$
		}
		return bundledNssDir;
	}

    /** Descomprime un fichero ZIP a disco.
     * @param zipFileResourceName Nombre del recurso del fichero ZIP.
     * @param outDir Directorio local en el que descomprimir.
     * @throws IOException Cuando ocurre un error al descomprimir. */
     private static void uncompressZip(final String zipFileResourceName, final File outDir) throws IOException {
        final byte[] buffer = new byte[BUFFER_SIZE];
        try (
	        final InputStream fis = BundledNssHelper.class.getResourceAsStream(zipFileResourceName);
	        final ZipInputStream zipIs = new ZipInputStream(
	            fis
	        );
		) {
	        ZipEntry entry;
	        while ((entry = zipIs.getNextEntry()) != null) {

	        	final File outFile = new File(outDir, entry.getName()).getCanonicalFile();

				 if (!isParent(outDir, outFile)) {
					 zipIs.closeEntry();
					 throw new IOException("Se ha encontrado en el archivo comprimido una ruta que apuntaba fuera del directorio de destino"); //$NON-NLS-1$
				 }

	            if (entry.isDirectory()) {
	                outFile.mkdirs();
	            }
	            else {
	            	try (final FileOutputStream outFis = new FileOutputStream(outFile);) {
	            		int n;
	            		while ((n = zipIs.read(buffer)) > 0) {
	            			outFis.write(buffer, 0, n);
	            		}
	            		outFis.flush();
	            	}
	            	catch (final Exception e) {
	            		// Si el motivo del error por el que no se pudo descomprimir es que el fichero
	            		// esta bloqueado (caso de que exista y no se pueda sustituir), ignoramos el error
	            		if (!new File(outDir, entry.getName()).exists()) {
	            			zipIs.closeEntry();
	            			throw new IOException("No se pudo descomprimir una de las dependencias para la carga de NSS", e); //$NON-NLS-1$
	            		}
					}
	            }
	            zipIs.closeEntry();
	        }
        }
    }

	 /**
	  * Comprueba que el fichero {@code parentFile} es un directorio padre de la
	  * ruta de {@code childFile}.
	  * @param parentDir Directorio padre.
	  * @param childFile Fichero/directorio hijo.
	  * @return {@code true} cuando el directorio forma parte de la ruta de directorio,
	  * {@code false} en caso contrario.
	  * @throws IOException Cuando no se pueda canonizar el nombre de fichero hijo.
	  */
	 private static boolean isParent(final File parentDir, final File childFile) throws IOException {

		 final File parent = parentDir.getCanonicalFile();
		 File intermediateDir = childFile.getCanonicalFile();
		 while (intermediateDir != null && !intermediateDir.equals(parent)) {
			 intermediateDir = intermediateDir.getParentFile();
		 }
		 return intermediateDir != null;
	 }
}
