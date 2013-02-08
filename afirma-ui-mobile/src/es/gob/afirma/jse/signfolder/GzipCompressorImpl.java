package es.gob.afirma.jse.signfolder;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import es.gob.afirma.signfolder.GzipCompressor;

/**
 * Implementaci&oacute;n de un compresor de datos.
 * @author Carlos Gamuci
 */
public class GzipCompressorImpl implements GzipCompressor {

	private static final int BUFFER_SIZE = 1024;

	@Override
	public byte[] gzip(final byte[] data) throws IOException {

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

	@Override
	public byte[] gunzip(final byte[] compressedData) throws IOException {

		int n = 0;
		final byte[] buffer = new byte[BUFFER_SIZE];
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final GZIPInputStream gzipIs = new GZIPInputStream(new ByteArrayInputStream(compressedData));

		while ((n = gzipIs.read(buffer)) > 0) {
			baos.write(buffer, 0, n);
		}
		try {
			gzipIs.close();
		} catch (final Exception e) {
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
