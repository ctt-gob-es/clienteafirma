package es.gob.afirma.core.misc.http;

import org.junit.Test;

/** Prueba de descarga de datos.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestDataDownloader {

	/** Prueba de lectura del URL "file://".
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testDataDownloaderFile() throws Exception {
		final byte[] data = DataDownloader.downloadData(
			"file://c:/Windows/WindowsUpdate.log" //$NON-NLS-1$
		);
		System.out.println(new String(data));
	}

	/** Prueba de lectura de URL inexistente.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testDataDownloaderInvalidUrl() throws Exception {
		final byte[] data = UrlHttpManagerFactory.getInstalledManager().readUrlByPost(
			"http://dasdasdasd.asd?kaka=caca" //$NON-NLS-1$
		);
		System.out.println(new String(data));
	}

}
