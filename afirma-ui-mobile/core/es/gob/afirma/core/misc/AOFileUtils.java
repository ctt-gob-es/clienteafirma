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

    /** Crea un fichero ZIP en disco apto para manejarse.
     * @param zipFileData
     *        Los datos del zip.
     * @return Devuelve un fichero Zip.
     * @throws java.util.zip.ZipException
     *         Cuando los datos no eran realmente un Zip.
     * @throws IOException
     *         Cuando ocurre un error al leer los datos o crear el temporal
     *         para abrir el Zip. */
    public static ZipFile createTempZipFile(final byte[] zipFileData) throws IOException {

        // Creamos un fichero temporal
        final File tempFile = File.createTempFile("afirmazip", null); //$NON-NLS-1$
        final FileOutputStream fos = new FileOutputStream(tempFile);
        fos.write(zipFileData);
        fos.flush();
        fos.close();

        try {
            tempFile.deleteOnExit();
        }
        catch (final Exception e) {
            // Ignoramos los errores, el usuario debe limpiar los temporales regularmente
        }

        return new ZipFile(tempFile);
    }
}
