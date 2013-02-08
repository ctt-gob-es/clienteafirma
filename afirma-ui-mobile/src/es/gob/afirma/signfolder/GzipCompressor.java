package es.gob.afirma.signfolder;

import java.io.IOException;

/** Utilidades de compresi&oacute;n y descompresi&oacute;n de datos en formato GZIP.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public interface GzipCompressor {
	
	/** Comprime datos en formato GZIP.
	 * @param data Datos sin comprimir
	 * @return Datos comprimidos 
	 * @throws IOException Cuando ocurre alg&uacute;n problema durante la compresi&oacute;n */
	byte[] gzip(final byte[] data) throws IOException;
	
	/** Descomprime datos en formato GZIP.
	 * @param compressedData Datos comprimidos
	 * @return Datos sin comprimir 
	 * @throws IOException Cuando ocurre alg&uacute;n problema durante la descompresi&oacute;n */
	byte[] gunzip(final byte[] compressedData) throws IOException;

}
