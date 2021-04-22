package es.gob.afirma.signfolder.server.proxy;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.http.HttpError;
import es.gob.afirma.core.misc.http.UrlHttpManager;
import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.misc.http.UrlHttpMethod;

/** Pruebas del servicio de almacenamiento en servidor intermedio.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestStorageService {

	private static final String SERVICE_SCHEME = "http"; //$NON-NLS-1$
	private static final String SERVICE_HOST = "localhost"; //$NON-NLS-1$
	private static final String SERVICE_PORT = "8080"; //$NON-NLS-1$

	private static final String SERVICE_URL = SERVICE_SCHEME + "://" + SERVICE_HOST + ":" + SERVICE_PORT + "/afirma-signature-storage/StorageService"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

	/** Prueba de almac&eacute;n de un texto simple.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // No ejecutamos de forma automatica
	public void testStoreSimpleText() throws Exception {
		final UrlHttpManager mgr = UrlHttpManagerFactory.getInstalledManager();
		byte[] res;
		try {
			res = mgr.readUrl(
				SERVICE_URL,
				UrlHttpMethod.POST
			);
			Assert.assertTrue(new String(res).startsWith("ERR-00")); //$NON-NLS-1$
		}
		catch(final HttpError e) {
			// Es normal que de un 411 en un POST sin datos
			if (e.getResponseCode() != 411) {
				throw e;
			}
			System.out.println("Error 411 lanzado en el POST sin datos"); //$NON-NLS-1$
		}
		res = mgr.readUrl(
			SERVICE_URL + "?op=FAKE_OP", //$NON-NLS-1$
			UrlHttpMethod.POST
		);
		Assert.assertTrue(new String(res).startsWith("ERR-20")); //$NON-NLS-1$
		res = mgr.readUrl(
			SERVICE_URL + "?op=FAKE_OP&v=FAKE_V", //$NON-NLS-1$
			UrlHttpMethod.POST
		);
		Assert.assertTrue(new String(res).startsWith("ERR-01")); //$NON-NLS-1$
		res = mgr.readUrl(
			SERVICE_URL + "?op=put&v=FAKE_V", //$NON-NLS-1$
			UrlHttpMethod.POST
		);
		Assert.assertTrue(new String(res).startsWith("ERR-05")); //$NON-NLS-1$
		res = mgr.readUrl(
			SERVICE_URL + "?op=put&v=FAKE_V&id=ID.txt", //$NON-NLS-1$
			UrlHttpMethod.POST
		);
		Assert.assertEquals("OK", new String(res)); //$NON-NLS-1$
		res = mgr.readUrl(
			SERVICE_URL + "?op=put&v=FAKE_V&id=ID2.txt&dat=" + Base64.encode("DATOS".getBytes(), true), //$NON-NLS-1$ //$NON-NLS-2$
			UrlHttpMethod.POST
		);
		Assert.assertEquals("OK", new String(res)); //$NON-NLS-1$

		// Ahora igual pero con GET

		res = mgr.readUrl(
			SERVICE_URL,
			UrlHttpMethod.GET
		);
		Assert.assertTrue(new String(res).startsWith("ERR-00")); //$NON-NLS-1$
		res = mgr.readUrl(
			SERVICE_URL + "?op=FAKE_OP", //$NON-NLS-1$
			UrlHttpMethod.GET
		);
		Assert.assertTrue(
			"Se esperaba ERR-20, pero se ha obtenido el codigo : " + new String(res), //$NON-NLS-1$
			new String(res).startsWith("ERR-20") //$NON-NLS-1$
		);
		res = mgr.readUrl(
			SERVICE_URL + "?op=FAKE_OP&v=FAKE_V", //$NON-NLS-1$
			UrlHttpMethod.GET
		);
		Assert.assertTrue(new String(res).startsWith("ERR-01")); //$NON-NLS-1$
		res = mgr.readUrl(
			SERVICE_URL + "?op=put&v=FAKE_V", //$NON-NLS-1$
			UrlHttpMethod.GET
		);
		Assert.assertTrue(new String(res).startsWith("ERR-05")); //$NON-NLS-1$
		res = mgr.readUrl(
			SERVICE_URL + "?op=put&v=FAKE_V&id=ID3.txt", //$NON-NLS-1$
			UrlHttpMethod.GET
		);
		Assert.assertEquals("OK", new String(res)); //$NON-NLS-1$
		res = mgr.readUrl(
			SERVICE_URL + "?op=put&v=FAKE_V&id=ID4.txt&dat=" + Base64.encode("DATOS2".getBytes(), true), //$NON-NLS-1$ //$NON-NLS-2$
			UrlHttpMethod.GET
		);
		Assert.assertEquals("OK", new String(res)); //$NON-NLS-1$
	}

}
