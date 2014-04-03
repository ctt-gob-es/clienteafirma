/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.envelopers.cms;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.logging.Logger;
import java.util.zip.Deflater;
import java.util.zip.Inflater;

/** Clase de ayuda de compresi&oacute;n. Permite tanto comprimir como
 * descomprimir. */
final class BinaryUtils {

	private static final int COMPRESS_BUFFER_SIZE = 1024;

    private BinaryUtils() {
        // No permitimos la instanciacion
    }

    /** M&eacute;todo que comprime una entrada con un nivel de compresion dado.
     * @param input
     *        Entrada a comrpimir.
     * @return Entrada comprimida. */
    static byte[] compress(final byte[] input) {

        // Creamos el compresor con el maximo nivel de compresion
        final Deflater compressor = new Deflater();
        compressor.setLevel(Deflater.BEST_COMPRESSION);

        // Le pasamos los datos a comprimir
        compressor.setInput(input);
        compressor.finish();

        final ByteArrayOutputStream bos = new ByteArrayOutputStream(input.length);

        // Comprimimos los datos
        final byte[] buf = new byte[COMPRESS_BUFFER_SIZE];
        while (!compressor.finished()) {
            final int count = compressor.deflate(buf);
            bos.write(buf, 0, count);
        }
        try {
            bos.close();
        }
        catch (final IOException e) {
         // Ignoramos los errores en el cierre
        }

        return bos.toByteArray();
    }

    /** M&eacute;todo que descomprime una entrada.
     * @param compressedData Entrada a descomprimir.
     * @return Entrada descomprimida. */
    static byte[] uncompress(final byte[] compressedData) {

        // Creamos el descompresor y le pasamos los datos
        final Inflater decompressor = new Inflater();
        decompressor.setInput(compressedData);

        final ByteArrayOutputStream bos = new ByteArrayOutputStream(compressedData.length);

        // Descomprimimos
        final byte[] buf = new byte[COMPRESS_BUFFER_SIZE];
        while (!decompressor.finished()) {
            try {
                final int count = decompressor.inflate(buf);
                bos.write(buf, 0, count);
            }
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").severe("Error descomprimiendo los datos: " + e); //$NON-NLS-1$ //$NON-NLS-2$
                break;
            }
        }
        try {
            bos.close();
        }
        catch (final IOException e) {
        	// Ignoramos los errores en el cierre
        }

        return bos.toByteArray();

    }

}
