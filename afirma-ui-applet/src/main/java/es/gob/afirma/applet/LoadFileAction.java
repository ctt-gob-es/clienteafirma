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

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;

/** Acci&oacute;n privilegiada que carga el contenido de un fichero y devuelve
 * almacena como resultado su valor en un objeto {@code byte[]}. */
public final class LoadFileAction extends BasicPrivilegedAction<Boolean, byte[]> {

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
            this.setError("La URI '" + strUri + "' no es valida", e); //$NON-NLS-1$ //$NON-NLS-2$
            throw new IllegalArgumentException(this.getErrorMessage(), e);
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

    public Boolean run() {

        InputStream is = null;
        try {
            is = AOUtil.loadFile(this.uri);
            this.setResult(AOUtil.getDataFromInputStream(is));
        }
        catch (final FileNotFoundException e) {
            this.setError("El fichero '" + this.uri.toASCIIString() + "' no existe", e); //$NON-NLS-1$ //$NON-NLS-2$
            return Boolean.FALSE;
        }
        catch (final Exception e) {
            this.setError("No se pudo acceder al fichero '" + this.uri.toASCIIString() + "'", e); //$NON-NLS-1$ //$NON-NLS-2$
            return Boolean.FALSE;
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
        return Boolean.TRUE;
    }
}
