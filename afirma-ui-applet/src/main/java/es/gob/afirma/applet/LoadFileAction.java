/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.applet;

import java.io.FileNotFoundException;
import java.io.InputStream;
import java.net.URI;
import java.security.PrivilegedExceptionAction;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;

/** Acci&oacute;n privilegiada que carga el contenido de un fichero y devuelve
 * almacena como resultado su valor en un objeto {@code byte[]}. */
public final class LoadFileAction implements PrivilegedExceptionAction<byte[]> {

    /** Ruta del fichero que se desea cargar. */
    private URI uri = null;

    /** Construye una acci&oacute;n privilegiada para la carga del contenido de
     * un fichero.
     * @param strUri
     *        Ruta del fichero.
     */
    public LoadFileAction(final String strUri) {

        try {
            this.uri = AOUtil.createURI(strUri);
        }
        catch (final AOException e) {
            throw new IllegalArgumentException("La URI '" + strUri + "' no es valida", e); //$NON-NLS-1$ //$NON-NLS-2$
        }
    }

    /** Construye una acci&oacute;n privilegiada para la carga del contenido de
     * un fichero.
     * @param fileUri
     *        Ruta del fichero.
     */
    public LoadFileAction(final URI fileUri) {
        this.uri = fileUri;
    }

    /** {@inheritDoc} */
    public byte[] run() throws FileNotFoundException, Exception {

        InputStream is = null;
        try {
            is = AOUtil.loadFile(this.uri);
            return AOUtil.getDataFromInputStream(is);
        }
        catch (final FileNotFoundException e) {
            throw new FileNotFoundException("El fichero '" + this.uri.toASCIIString() + "' no existe"); //$NON-NLS-1$ //$NON-NLS-2$
        }
        catch (final Exception e) {
            throw new Exception("No se pudo acceder al fichero '" + this.uri.toASCIIString() + "'", e); //$NON-NLS-1$ //$NON-NLS-2$
        }
        finally {
            if (is != null) {
                try {
                    is.close();
                }
                catch (final Exception e) {
                    // Ignoramos los errores en el cierre
                }
            }
        }
    }
}
