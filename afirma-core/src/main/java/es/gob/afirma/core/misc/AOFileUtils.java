/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

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
     * @throws ZipException
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
