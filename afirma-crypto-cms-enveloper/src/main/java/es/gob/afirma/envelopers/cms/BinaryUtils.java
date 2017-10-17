/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.envelopers.cms;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.zip.DataFormatException;
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
     * @return Entrada comprimida.
     * @throws IOException Si hay problemas en el cierre de los datos comprimidos. */
    static byte[] compress(final byte[] input) throws IOException {

        // Creamos el compresor con el maximo nivel de compresion
        final Deflater compressor = new Deflater();
        compressor.setLevel(Deflater.BEST_COMPRESSION);

        // Le pasamos los datos a comprimir
        compressor.setInput(input);
        compressor.finish();

        final byte[] ret;
        try (
    		final ByteArrayOutputStream bos = new ByteArrayOutputStream(input.length);
		) {
            // Comprimimos los datos
            final byte[] buf = new byte[COMPRESS_BUFFER_SIZE];
            while (!compressor.finished()) {
                final int count = compressor.deflate(buf);
                bos.write(buf, 0, count);
            }
            ret = bos.toByteArray();
        }
        return ret;

    }

    /** M&eacute;todo que descomprime una entrada.
     * @param compressedData Entrada a descomprimir.
     * @return Entrada descomprimida.
     * @throws DataFormatException Si los datos de entrada no est&aacute;n comprmidos.
     * @throws IOException Si hay problemas cerrando los datos descomprimidos. */
    static byte[] uncompress(final byte[] compressedData) throws DataFormatException, IOException {

        // Creamos el descompresor y le pasamos los datos
        final Inflater decompressor = new Inflater();
        decompressor.setInput(compressedData);

        final byte[] ret;
        try (
    		final ByteArrayOutputStream bos = new ByteArrayOutputStream(compressedData.length);
		) {
	        // Descomprimimos
	        final byte[] buf = new byte[COMPRESS_BUFFER_SIZE];
	        while (!decompressor.finished()) {
                final int count = decompressor.inflate(buf);
                bos.write(buf, 0, count);
	        }
	        ret = bos.toByteArray();
        }

        return ret;

    }

}
