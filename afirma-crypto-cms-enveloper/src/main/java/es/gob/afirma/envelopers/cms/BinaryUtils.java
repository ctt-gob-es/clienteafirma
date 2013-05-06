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
    
    private BinaryUtils() {
        // No permitimos la instanciacion
    }

    /** M&eacute;todo que comprime una entrada con un nivel de compresion dado.
     * @param input
     *        Entrada a comrpimir.
     * @return Entrada comprimida. */
    static byte[] compress(final byte[] input) {

        // Create the compressor with highest level of compression
        final Deflater compressor = new Deflater();
        compressor.setLevel(Deflater.BEST_COMPRESSION);

        // Give the compressor the data to compress
        compressor.setInput(input);
        compressor.finish();

        // Create an expandable byte array to hold the compressed data.
        // You cannot use an array that's the same size as the orginal because
        // there is no guarantee that the compressed data will be smaller than
        // the uncompressed data.
        final ByteArrayOutputStream bos = new ByteArrayOutputStream(input.length);

        // Compress the data
        final byte[] buf = new byte[1024];
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

        // Get the compressed data
        final byte[] compressedData = bos.toByteArray();

        return compressedData;
    }

    /** M&eacute;todo que descomprime una entrada.
     * @param compressedData
     *        Entrada a descomprimir.
     * @return Entrada descomprimida. */
    static byte[] uncompress(final byte[] compressedData) {

        // Create the decompressor and give it the data to compress
        final Inflater decompressor = new Inflater();
        decompressor.setInput(compressedData);

        // Create an expandable byte array to hold the decompressed data
        final ByteArrayOutputStream bos = new ByteArrayOutputStream(compressedData.length);

        // Decompress the data
        final byte[] buf = new byte[1024];
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
            Logger.getLogger("es.gob.afirma").warning("Error cerrando el flujo binario: " + e); //$NON-NLS-1$ //$NON-NLS-2$
        }

        // Get the decompressed data
        final byte[] decompressedData = bos.toByteArray();

        return decompressedData;

    }

}
