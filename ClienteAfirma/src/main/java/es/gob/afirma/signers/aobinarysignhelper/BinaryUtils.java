/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.aobinarysignhelper;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.logging.Logger;
import java.util.zip.Deflater;
import java.util.zip.Inflater;

/**
 * Clase de ayuda de compresi&oacute;n. Permite tanto comprimir como
 * descomprimir.
 * 
 */
final class BinaryUtils {

	/**
	 * M&eacute;todo que comprime una entrada con un nivel de compresion dado.
	 * 
	 * @param input
	 *            Entrada a comrpimir.
	 * @return Entrada comprimida.
	 */
	static public byte[] compress(byte[] input) {

		// Create the compressor with highest level of compression
		Deflater compressor = new Deflater();
		compressor.setLevel(Deflater.BEST_COMPRESSION);

		// Give the compressor the data to compress
		compressor.setInput(input);
		compressor.finish();

		// Create an expandable byte array to hold the compressed data.
		// You cannot use an array that's the same size as the orginal because
		// there is no guarantee that the compressed data will be smaller than
		// the uncompressed data.
		ByteArrayOutputStream bos = new ByteArrayOutputStream(input.length);

		// Compress the data
		byte[] buf = new byte[1024];
		while (!compressor.finished()) {
			int count = compressor.deflate(buf);
			bos.write(buf, 0, count);
		}
		try {
			bos.close();
		} catch (IOException e) {
		}

		// Get the compressed data
		byte[] compressedData = bos.toByteArray();

		return compressedData;
	}

	/**
	 * M&eacute;todo que descomprime una entrada.
	 * 
	 * @param compressedData
	 *            Entrada a descomprimir.
	 * @return Entrada descomprimida.
	 */
	static public byte[] uncompress(byte[] compressedData) {

		// Create the decompressor and give it the data to compress
		Inflater decompressor = new Inflater();
		decompressor.setInput(compressedData);

		// Create an expandable byte array to hold the decompressed data
		ByteArrayOutputStream bos = new ByteArrayOutputStream(
				compressedData.length);

		// Decompress the data
		byte[] buf = new byte[1024];
		while (!decompressor.finished()) {
			try {
				int count = decompressor.inflate(buf);
				bos.write(buf, 0, count);
			} catch (final Exception e) {
				Logger.getLogger("es.gob.afirma").severe(
						"Error descomprimiendo los datos: " + e);
				break;
			}
		}
		try {
			bos.close();
		} catch (final IOException e) {
			Logger.getLogger("es.gob.afirma").warning(
					"Error cerrando el flujo binario: " + e);
		}

		// Get the decompressed data
		byte[] decompressedData = bos.toByteArray();

		return decompressedData;

	}

}
