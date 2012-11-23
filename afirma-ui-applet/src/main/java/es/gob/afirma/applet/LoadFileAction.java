/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.security.PrivilegedExceptionAction;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;

/** Acci&oacute;n privilegiada que carga el contenido de un fichero y devuelve
 * almacena como resultado su valor en un objeto {@code byte[]}. */
final class LoadFileAction implements PrivilegedExceptionAction<byte[]> {

    /** Ruta del fichero que se desea cargar. */
    private URI uri = null;

    /** Construye una acci&oacute;n privilegiada para la carga del contenido de
     * un fichero.
     * @param strUri
     *        Ruta del fichero.
     */
    LoadFileAction(final String strUri) {

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
    LoadFileAction(final URI fileUri) {
        this.uri = fileUri;
    }

    /** {@inheritDoc} */
    @Override
	public byte[] run() throws IOException {

        InputStream is = null;
        try {
            is = AOUtil.loadFile(this.uri);
            return AOUtil.getDataFromInputStream(is);
        }
        catch (final FileNotFoundException e) {
            throw new FileNotFoundException("El fichero '" + this.uri.toASCIIString() + "' no existe"); //$NON-NLS-1$ //$NON-NLS-2$
        }
        catch (final Exception e) {
            throw new IOException("No se pudo acceder al fichero '" + this.uri.toASCIIString() + "'"); //$NON-NLS-1$ //$NON-NLS-2$
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
