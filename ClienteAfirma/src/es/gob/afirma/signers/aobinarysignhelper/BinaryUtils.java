/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.signers.aobinarysignhelper;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.zip.DataFormatException;
import java.util.zip.Deflater;
import java.util.zip.Inflater;

/**
 * Clase de ayuda de compresi&oacute;n.
 * Permite tanto comprimir como descomprimir.
 *
 */
public class BinaryUtils {

	/**
	 * M&eacute;todo que comprime una entrada con un nivel de compresion dado.
	 * @param input				Entrada a comrpimir.
	 * @return					Entrada comprimida.
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
	 * @param compressedData Entrada a descomprimir.
	 * @return				 Entrada descomprimida.
	 */
	static public byte[] uncompress(byte[] compressedData){
		
		//      Create the decompressor and give it the data to compress
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
			} catch (DataFormatException e) {
				e.printStackTrace();
				break;
			}
		}
		try {
			bos.close();
		} catch (IOException e) {
			e.printStackTrace();
		}

		// Get the decompressed data
		byte[] decompressedData = bos.toByteArray();

		return decompressedData;

	}
	
}
