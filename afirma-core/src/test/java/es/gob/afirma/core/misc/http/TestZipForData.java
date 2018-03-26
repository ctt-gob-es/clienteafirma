package es.gob.afirma.core.misc.http;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.zip.GZIPOutputStream;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.Base64;

/** Pruebas de compresi&oacute;n de datos en URL con GZIP.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestZipForData {

	private static final String SAMPLE_DATA = "RklSTUFOVEUgREVMIENFUlRJRklDQURPCiAgICAgREFUT1MgREUgSURFTlRJRklDQUNJw5NOCiAgICAgICAgICBOT01CUkU6UEVQRQogICAgICAgICAgUFJJTUVSIEFQRUxMSURPOk5Vw5HDiVoKICAgICAgICAgIFNFR1VORE8gQVBFTExJRE86RE9TCiAgICAgICAgICBOwrogREVMIERPQ1VNRU5UTyBERSBJREVOVElGSUNBQ0nDk046MTIzNDU2NzhaCiAgICAgICAgICBHSUQ6NkQzOTM4QkFBMUI0N0JDMTIwMTIzOUExRjFGRDgwMkI5QjgwNDBEMAogICAgIERBVE9TIERFIENPTlRBQ1RPIFBST0ZFU0lPTkFMRVMKICAgICAgICAgIFBBw41TOkVTCiAgICAgICAgICBESVJFQ0NJw5NOOlBSVUVCQVMKICAgICAgICAgIEPDk0RJR08gUE9TVEFMOjIwMDAxCiAgICAgICAgICBMT0NBTElEQUQ6UFJVRUJBUwogICAgICAgICAgUFJPVklOQ0lBOkdVSVBVWktPQQogICAgIE9UUk9TIERBVE9TIERFIENPTlRBQ1RPCiAgICAgICAgICBURUzDiUZPTk86CiAgICAgICAgICBDT1JSRU8gRUxFQ1RSw5NOSUNPOmpvc2VtcnViaW9AZm5tdC5lcwogICAgIERBVE9TIFBST0ZFU0lPTkFMRVMKICAgICAgICAgIE9SR0FOSVNNTyBTVVNDUklQVE9SOk1JTklTVEVSSUUgVkFOIEJJTk5FTkxBTkRTRSBaQUtFTgogICAgICAgICAgSUQgREFUTyBFWFRFTlNJT04gRU5USURBRDozCiAgICAgICAgICBOSUYgT1JHQU5JU01PIFNVU0NSSVBUT1I6UTI4MjYwMDRKCiAgICAgICAgICBJRCBEQVRPIEVYVEVOU0lPTiBDSUYgU1VTQ1JJUFRPUjo0CiAgICAgICAgICBVTklEQUQgT1JHQU5JWkFUSVZBOgogICAgICAgICAgSUQgREFUTyBFWFRFTlNJT04gVU5JREFEIE9SR0FOSVpBVElWQToyCiAgICAgICAgICBQVUVTVE8gREUgVFJBQkFKTyBPIENBUkdPOgogICAgICAgICAgSUQgREFUTyBFWFRFTlNJT04gQ0FSR086NQogICAgICAgICAgTsK6IERFIElERU5USUZJQ0FDScOTTiBQUk9GRVNJT05BTDoKICAgICAgICAgIElEIERBVE8gRVhURU5TSU9OIE5JUDoxCkRBVE9TIFNPTElDSVRVRAogICAgIEPDk0RJR08gREUgU09MSUNJVFVECiAgICAgICAgICBDw5NESUdPIERFIFNPTElDSVRVRDo0NTgwMDk2NTcKICAgICBEQVRPUyBERUwgQ09OVFJBVE8uCiAgICAgICAgICBOw5pNRVJPIERFIExBIFBFVElDScOTTjozMjY1NAogICAgICAgICAgRkVDSEEgREUgTEEgUEVUSUNJw5NOOjIyMDMyMDE4MTQxNzQyCiAgICAgICAgICBDw5NESUdPIERFIENPTlRSQVRPOk1WQlpfQ0UvTVZCWl9DRTAwMDEvTVZCWl9DRTAwMDFfUDAwMS82LzIyMDMyMDE4MTQxNzQyCiAgICAgICAgICBUSVBPIERFIENPTlRSQVRPOkNfRklT"; //$NON-NLS-1$

	private static final String PARAM ="&gzip=true"; //$NON-NLS-1$

	/** Comprueba las ganancias de tama&ntilde;o al comprimir unos datos de firma de ejemplo.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testSizes() throws Exception {
		System.out.println("Tamano de los datos en Base64: " + SAMPLE_DATA.length()); //$NON-NLS-1$
		final byte[] origBytes = Base64.decode(SAMPLE_DATA, true);
		System.out.println("Tamano de los datos en binario: " + origBytes.length); //$NON-NLS-1$
		final byte[] compressedBytes = gzipBytes(origBytes);
		final String compressedBase64 = Base64.encode(compressedBytes, true);
		System.out.println("Tamano de los datos comprimidos en Base64: " + compressedBase64.length()); //$NON-NLS-1$
		System.out.println("Tamano de los datos comprimidos en binario: " + compressedBytes.length); //$NON-NLS-1$
		System.out.println("Ganancia bruta: " + (SAMPLE_DATA.length() - compressedBase64.length())); //$NON-NLS-1$
		System.out.println("Ganancia neta: " + (SAMPLE_DATA.length() - (compressedBase64.length() + PARAM.length()))); //$NON-NLS-1$
	}

	/** Prueba la funci&oacute;n de descarga de datos indicando que son comprimidos.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testDownloadData() throws Exception {
		final byte[] origBytes = Base64.decode(SAMPLE_DATA, true);
		final byte[] compressedBytes = gzipBytes(origBytes);
		final byte[] downData = DataDownloader.downloadData(
			Base64.encode(compressedBytes, true),
			true
		);
		Assert.assertArrayEquals(origBytes, downData);
	}

	/** Prueba de obtenci&oacute;n del valor del par&aacute;metro <code>gzip</code>
	 * de un mapa de cadenas. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testParamParse() {
		final Map<String, String> noParam = new HashMap<>(0);
		final Map<String, String> noGzip = new HashMap<>(1);
		noGzip.put("gzip", "false"); //$NON-NLS-1$ //$NON-NLS-2$
		final Map<String, String> yesGzip = new HashMap<>(1);
		yesGzip.put("gzip", "true"); //$NON-NLS-1$ //$NON-NLS-2$
		Assert.assertFalse(Boolean.parseBoolean(noParam.get("gzip"))); //$NON-NLS-1$
		Assert.assertFalse(Boolean.parseBoolean(noGzip.get("gzip"))); //$NON-NLS-1$
		Assert.assertTrue(Boolean.parseBoolean(yesGzip.get("gzip"))); //$NON-NLS-1$
	}

	private static byte[] gzipBytes(final byte[] in) throws IOException {
		try (
			final ByteArrayOutputStream byteStream = new ByteArrayOutputStream(in.length);
			final ByteArrayOutputStream baos = new ByteArrayOutputStream();
			final GZIPOutputStream zipStream = new GZIPOutputStream(byteStream);
		) {
			zipStream.write(in);
			zipStream.close();
			byteStream.close();
			return byteStream.toByteArray();
		}
	}

}
