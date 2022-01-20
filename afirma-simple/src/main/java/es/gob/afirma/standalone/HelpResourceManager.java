/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.standalone.updater.Updater;

/** Gestor de los recursos de las diferentes formas de ayuda de la aplicaci&oacute;n,
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class HelpResourceManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Longitud m&aacute;xima del texto de versi&oacute;n que se procesar&aacute;. */
	private static final int MAX_VERSION_LENGTH = 30;

	private HelpResourceManager() {
		// No permitimos instanciar
	}

    /** Crea el directorio de usuario del programa si no existe, */
    private static void createApplicationDataDir() {
    	final File apDir = new File(SimpleAfirma.APPLICATION_HOME);
    	if (!apDir.exists()) {
    		apDir.mkdirs();
    	}

    	final File helpDir = new File(SimpleAfirma.APPLICATION_HOME + "\\help"); //$NON-NLS-1$
    	if (!helpDir.exists()) {
    		helpDir.mkdirs();
    	}
    }

	static void createWindowsHelpResources(final File filesList, final File helpVersionFile) throws IOException {
		extractResource(
			"help/fileslist.txt", //$NON-NLS-1$
			filesList
		);

		// Creamos un fichero que marca la version del fichero de ayuda utilizando la version de la aplicacion
		try (FileOutputStream fos = new FileOutputStream(helpVersionFile)) {
			fos.write(Updater.getCurrentVersionText().getBytes());
		}
	}

	/** Crea los recursos necesarios para mostrar la ayuda de la aplicaci&oacute;n en formato Apple OS X.
	 * En concreto, copia una biblioteca JNI nativa de OS X que hace de puente entre Java y el subsistema
	 * de ayuda de OS X. Esta biblioteca, llamada <code>libJavaHelpHook.jnilib</code> se crea a partir del
	 * siguiente c&oacute;digo fuente Objective-C (<code>JavaHelpHook.m</code>):
	 * <pre>
	 * #import &lt;JavaVM/jni.h&gt;
     * #import &lt;Cocoa/Cocoa.h&gt;
     *
     * JNIEXPORT void JNICALL Java_es_gob_afirma_standalone_ui_MacHelpHooker_showHelp(JNIEnv *env, jclass clazz)
     * {
	 *   [[NSApplication sharedApplication] performSelectorOnMainThread:@selector(showHelp:) withObject:NULL waitUntilDone:NO];
     * }
	 * </pre>
	 * @throws IOException Si no se pueden crear los recursos de ayuda. */
	public static void createOsxHelpResources() throws IOException {
		final File appleHelpFile = new File(SimpleAfirma.APPLICATION_HOME + "/libJavaHelpHook.jnilib"); //$NON-NLS-1$
		if (!appleHelpFile.exists()) {
			extractResource(
				"help/AppleHelp/libJavaHelpHook.jnilib", //$NON-NLS-1$
				appleHelpFile
			);
		}

	}

	private static void extractResource(final String filesList, final File destination) throws IOException {

		// Creamos el directorio de la aplicacion
		createApplicationDataDir();

		// Copiamos el recurso con la ruta de todos los archivos desde el JAR hasta el destino especificado
    	final byte[] bytes = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(filesList));

        try (final FileOutputStream fos = new FileOutputStream(destination)) {
        	fos.write(bytes);
        }

        try (BufferedReader br = new BufferedReader(new FileReader(destination))) {
            String line;
            while ((line = br.readLine()) != null) {
               final URL url = ClassLoader.getSystemResource("help"+ File.separator + line); //$NON-NLS-1$
               final String rscFileDecoded = URLDecoder.decode(url.getPath(), StandardCharsets.UTF_8.name());
               final File resourceFile = new File(rscFileDecoded);
               if (resourceFile.exists()) {
            	   final File newFile = new File(SimpleAfirma.APPLICATION_HOME + "\\help\\" + line);
            	   if (resourceFile.isDirectory()) {
            		   newFile.mkdir();
            	   } else {
            	    	final byte[] fileData = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("help\\" + line)); //$NON-NLS-1$
            	        try (final FileOutputStream fos = new FileOutputStream(newFile)) {
            	        	fos.write(fileData);
            	        }
            	   }
               } else {
            	   LOGGER.warning("El archivo " + line + " no existe en los recursos");  //$NON-NLS-1$//$NON-NLS-2$
               }
            }
        }
	}

	/**
	 * Indica si un fichero con la versi&oacute;n de la ayuda de la aplicaci&oacute;n
	 * tiene una versi&oacute;n que no se corresponde con la actual.
	 * @param helpVersionFile Fichero con la versi&oacute;n de la ayuda.
	 * @return {@code true} si el fichero de version indicado no se corresponde con el
	 * de la versi&oacute;n actual. {@code false} en caso contrario.
	 */
	public static boolean isDifferentHelpFile(final File helpVersionFile) {
		if (!helpVersionFile.exists()) {
			return true;
		}

		final byte[] fileContent = new byte[(int) Math.min(helpVersionFile.length(), MAX_VERSION_LENGTH)];
		try (FileInputStream fis = new FileInputStream(helpVersionFile);) {
			fis.read(fileContent);
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo leer el fichero con la version de la ayuda. Se generara la nueva ayuda: " + e); //$NON-NLS-1$
			return true;
		}
		final String trimFileVersion = new String(fileContent, StandardCharsets.UTF_8);

		final String currentAppVersion = Updater.getCurrentVersion();
		final String trimAppVersion = currentAppVersion.substring(0,  Math.min(currentAppVersion.length(), MAX_VERSION_LENGTH));

		return !trimFileVersion.equals(trimAppVersion);
	}
}
