/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.misc;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

/** Clase con m&eacute;todos para el trabajo con ficheros. */
public final class AOFileUtils {

    /** Crea un fichero ZIP en disco apto para manejarse.
     * @param zipFileData
     *        Los datos del zip.
     * @return Devuelve un fichero Zip.
     * @throws ZipException
     *         Cuando los datos no eran realmente un Zip.
     * @throws IOException
     *         Cuando ocurre un error al leer los datos o crear el temporal
     *         para abrir el Zip. */
    public static ZipFile createTempZipFile(final byte[] zipFileData) throws ZipException, IOException {

        // Creamos un fichero temporal
        final File tempFile = File.createTempFile("afirmazip", null);
        final FileOutputStream fos = new FileOutputStream(tempFile);
        fos.write(zipFileData);
        fos.flush();
        fos.close();

        try {
            tempFile.deleteOnExit();
        }
        catch (final Exception e) {}

        return new ZipFile(tempFile);
    }
}
