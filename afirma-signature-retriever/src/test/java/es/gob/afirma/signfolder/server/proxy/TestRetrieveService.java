package es.gob.afirma.signfolder.server.proxy;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.http.HttpError;
import es.gob.afirma.core.misc.http.UrlHttpManager;
import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.misc.http.UrlHttpMethod;

/** Pruebas del servicio de recuperaci&oacute;n en servidor intermedio.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestRetrieveService {

	//private static final String SERVICE_URL = "http://localhost:8080/afirma-signature-retriever/RetrieveService"; //$NON-NLS-1$
	private static final String SERVICE_URL = "http://localhost:56499/afirma-signature-retriever/RetrieveService"; //$NON-NLS-1$

	/** Prueba de recuperaci&oacute;n de un texto simple.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // No ejecutamos de forma automatica
	public void testRetrieveSimpleText() throws Exception {
		final UrlHttpManager mgr = UrlHttpManagerFactory.getInstalledManager();
		byte[] res = mgr.readUrl(
			SERVICE_URL,
			UrlHttpMethod.GET
		);
		Assert.assertTrue(new String(res).startsWith("ERR-00")); //$NON-NLS-1$
		res = mgr.readUrl(
			SERVICE_URL + "?op=FAKE_OP", //$NON-NLS-1$
			UrlHttpMethod.GET
		);
		Assert.assertTrue(new String(res).startsWith("ERR-20")); //$NON-NLS-1$
		res = mgr.readUrl(
			SERVICE_URL + "?op=FAKE_OP&v=FAKE_V", //$NON-NLS-1$
			UrlHttpMethod.GET
		);
		Assert.assertTrue(new String(res).startsWith("ERR-01")); //$NON-NLS-1$
		res = mgr.readUrl(
			SERVICE_URL + "?op=get&v=FAKE_V&id=ID", //$NON-NLS-1$
			UrlHttpMethod.GET
		);
		Assert.assertTrue(new String(res).startsWith("ERR-06")); //$NON-NLS-1$
		res = mgr.readUrl(
			SERVICE_URL + "?op=get&v=FAKE_V&id=ID.txt", //$NON-NLS-1$
			UrlHttpMethod.GET
		);
		Assert.assertEquals("DATA", new String(res).trim()); //$NON-NLS-1$

		// Ahora todo de nuevo pero con POST
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
			SERVICE_URL + "?op=get&v=PEDO&id=ID2", //$NON-NLS-1$
			UrlHttpMethod.POST
		);
		Assert.assertTrue(new String(res).startsWith("ERR-06")); //$NON-NLS-1$
	}

}
