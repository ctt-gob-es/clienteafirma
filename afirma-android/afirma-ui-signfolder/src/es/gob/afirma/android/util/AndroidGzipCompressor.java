package es.gob.afirma.android.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

/** Implementaci&oacute;n de un compresor de datos.
 * @author Carlos Gamuci */
public final class AndroidGzipCompressor {

	private static final int BUFFER_SIZE = 1024;

	/** Comprime datos en formato GZIP.
	 * @param data Datos sin comprimir
	 * @return Datos comprimidos
	 * @throws IOException Cuando ocurre alg&uacute;n problema durante la compresi&oacute;n */
	public static byte[] gzip(final byte[] data) throws IOException {

		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final GZIPOutputStream gzipOs = new GZIPOutputStream(baos);
		gzipOs.write(data);
		gzipOs.finish();
		final byte[] compressedData = baos.toByteArray();
		try {
			gzipOs.close();
		} catch (final Exception e) {
			// No tratamos el error
		}
		return compressedData;
	}

	/** Descomprime datos en formato GZIP.
	 * @param compressedData Datos comprimidos
	 * @return Datos sin comprimir
	 * @throws IOException Cuando ocurre alg&uacute;n problema durante la descompresi&oacute;n */
	public static byte[] gunzip(final byte[] compressedData) throws IOException {

		int n = 0;
		final byte[] buffer = new byte[BUFFER_SIZE];
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final GZIPInputStream gzipIs = new GZIPInputStream(new ByteArrayInputStream(compressedData));

		while ((n = gzipIs.read(buffer)) > 0) {
			baos.write(buffer, 0, n);
		}
		try {
			gzipIs.close();
		}
		catch (final Exception e) {
			// No tratamos el error
		}
		final byte[] data = baos.toByteArray();
		try {
			baos.close();
		} catch (final Exception e) {
			// No tratamos el error
		}
		return data;
	}
}
