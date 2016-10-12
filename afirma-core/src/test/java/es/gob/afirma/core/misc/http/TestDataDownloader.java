package es.gob.afirma.core.misc.http;

import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.Platform.OS;

/** Prueba de descarga de datos.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestDataDownloader {

	private static final String TEST_POST = "https://www.mifirma.com/signatures?" +  //$NON-NLS-1$
		"<ilp-signature>\r\n" + //$NON-NLS-1$
		"    <date-of-birth type=\"date\">1974-09-20</date-of-birth>\r\n" + //$NON-NLS-1$
		"    <dni>31733888Y</dni>\r\n" + //$NON-NLS-1$
		"    <email>sdfg@dsfg.com</email>\r\n" + //$NON-NLS-1$
		"    <name>sdfg</name>\r\n" + //$NON-NLS-1$
		"    <surname>sdfg</surname>\r\n" + //$NON-NLS-1$
		"    <surname2>sdfg</surname2>\r\n" + //$NON-NLS-1$
		"    <terms type=\"boolean\">true</terms>\r\n" + //$NON-NLS-1$
		"    <proposal-id type=\"integer\">57</proposal-id>\r\n" + //$NON-NLS-1$
		"</ilp-signature>\r\n" //$NON-NLS-1$
	;

	private static final String TEST_POST_URL = "https://www.mifirma.com/signatures"; //$NON-NLS-1$
	private static final String TEST_POST_BODY =
		"<ilp-signature>\r\n" + //$NON-NLS-1$
		"    <date-of-birth type=\"date\">1974-09-20</date-of-birth>\r\n" + //$NON-NLS-1$
		"    <dni>31733888Y</dni>\r\n" + //$NON-NLS-1$
		"    <email>sdfg@dsfg.com</email>\r\n" + //$NON-NLS-1$
		"    <name>Javier</name>\r\n" + //$NON-NLS-1$
		"    <proposal-id type=\"integer\">57</proposal-id>\r\n" + //$NON-NLS-1$
		"    <surname>Pe�a</surname>\r\n" + //$NON-NLS-1$
		"    <surname2>Mart�nez</surname2>\r\n" + //$NON-NLS-1$
		"    <terms type=\"boolean\">true</terms>\r\n" + //$NON-NLS-1$
		"</ilp-signature>\r\n" //$NON-NLS-1$
	;

	private static final String TEST_GET = "https://www.mifirma.com/proposals/57"; //$NON-NLS-1$

	/** Prueba manual de HTTP POST.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void doDirectPost() throws Exception {
		final URL u = new URL(TEST_POST_URL);
		final HttpURLConnection conn = (HttpURLConnection) u.openConnection();
		conn.setDoOutput(true);
		conn.setRequestMethod("POST"); //$NON-NLS-1$
		conn.setRequestProperty("Content-Type", "application/xml"); //$NON-NLS-1$ //$NON-NLS-2$ //
		conn.setRequestProperty("Accept", "text/xml"); //$NON-NLS-1$ //$NON-NLS-2$

		final OutputStream os = conn.getOutputStream();
		os.write(TEST_POST_BODY.getBytes("UTF-8")); //$NON-NLS-1$
		os.close();

		conn.connect();

		final int resCode = conn.getResponseCode();
		final String statusCode = Integer.toString(resCode);
		System.out.println("Status code: " + statusCode); //$NON-NLS-1$

		final InputStream is = conn.getInputStream();
		final byte[] data = AOUtil.getDataFromInputStream(is);
		is.close();

		System.out.println(new String(data));
	}


	/** Prueba de lectura del URL "file://".
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testDataDownloaderFile() throws Exception {
		OS os = Platform.getOS();
		String fileName = "file://c:/Windows/WindowsUpdate.log"; //$NON-NLS-1$
		if (Platform.OS.LINUX == os || Platform.OS.SOLARIS == os) {
			fileName = "file:///etc/timezone"; //$NON-NLS-1$
		}
		final byte[] data = DataDownloader.downloadData(fileName);
		System.out.println(new String(data));
	}

	/** Prueba de lectura de URL inexistente.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test(expected=java.net.UnknownHostException.class)
	public void testDataDownloaderInvalidUrl() throws Exception {
		final byte[] data = UrlHttpManagerFactory.getInstalledManager().readUrl(
			"http://dasdasdasd.asd?kaka=caca", //$NON-NLS-1$
			UrlHttpMethod.POST
		);
		System.out.println(new String(data));
	}

	/** Prueba de POST.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testPost() throws Exception {
		final byte[] data = UrlHttpManagerFactory.getInstalledManager().readUrl(
			TEST_POST,
			-1,
			"application/xml", //$NON-NLS-1$
			"text/xml", //$NON-NLS-1$
			UrlHttpMethod.POST
		);
		System.out.println(new String(data));
	}

	/** Prueba de GET.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testGet() throws Exception {
		final byte[] data = UrlHttpManagerFactory.getInstalledManager().readUrl(
			TEST_GET,
			-1,
			"application/xml", //$NON-NLS-1$
			"text/xml", //$NON-NLS-1$
			UrlHttpMethod.GET
		);
		System.out.println(new String(data));
	}

}
