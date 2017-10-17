/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.security.PrivilegedExceptionAction;

import es.gob.afirma.core.misc.AOUtil;

/** Acci&oacute;n privilegiada que carga el contenido de un fichero y devuelve
 * almacena como resultado su valor en un objeto {@code byte[]}. */
final class LoadFileAction implements PrivilegedExceptionAction<byte[]> {

    /** Ruta del fichero que se desea cargar. */
    private URI uri = null;

    /** Construye una acci&oacute;n privilegiada para la carga del contenido de
     * un fichero.
     * @param strUri Ruta del fichero. */
    LoadFileAction(final String strUri) {
        try {
            this.uri = AOUtil.createURI(strUri);
        }
        catch (final URISyntaxException e) {
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
    	final InputStream is = AOUtil.loadFile(this.uri);
        final byte[] data = AOUtil.getDataFromInputStream(is);
        is.close();
        return data;
    }
}
