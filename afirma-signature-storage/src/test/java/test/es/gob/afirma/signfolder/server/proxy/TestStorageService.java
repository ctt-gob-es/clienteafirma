package test.es.gob.afirma.signfolder.server.proxy;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import test.es.gob.afirma.signfolder.server.proxy.copiedclasses.HttpError;
import test.es.gob.afirma.signfolder.server.proxy.copiedclasses.UrlHttpManager;
import test.es.gob.afirma.signfolder.server.proxy.copiedclasses.UrlHttpManagerImpl;
import test.es.gob.afirma.signfolder.server.proxy.copiedclasses.UrlHttpMethod;

/** Pruebas del servicio de almacenamiento en servidor intermedio.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestStorageService {

	//private static final String SERVICE_URL = "http://localhost:8080/afirma-signature-storage/StorageService"; //$NON-NLS-1$
	private static final String SERVICE_URL = "http://localhost:56499/afirma-signature-storage/StorageService"; //$NON-NLS-1$

	/** Prueba de almac&eacute;n de un texto simple.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // No ejecutamos de forma automatica
	public void testStoreSimpleText() throws Exception {
		final UrlHttpManager mgr = new UrlHttpManagerImpl();
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
			SERVICE_URL + "?op=KAKA", //$NON-NLS-1$
			UrlHttpMethod.POST
		);
		Assert.assertTrue(new String(res).startsWith("ERR-20")); //$NON-NLS-1$
		res = mgr.readUrl(
			SERVICE_URL + "?op=KAKA&v=PEDO", //$NON-NLS-1$
			UrlHttpMethod.POST
		);
		Assert.assertTrue(new String(res).startsWith("ERR-01")); //$NON-NLS-1$
		res = mgr.readUrl(
			SERVICE_URL + "?op=put&v=PEDO", //$NON-NLS-1$
			UrlHttpMethod.POST
		);
		Assert.assertTrue(new String(res).startsWith("ERR-05")); //$NON-NLS-1$
		res = mgr.readUrl(
			SERVICE_URL + "?op=put&v=PEDO&id=PIS.txt", //$NON-NLS-1$
			UrlHttpMethod.POST
		);
		Assert.assertEquals("OK", new String(res)); //$NON-NLS-1$
		res = mgr.readUrl(
			SERVICE_URL + "?op=put&v=PEDO&id=CACA.txt&dat=CULO", //$NON-NLS-1$
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
			SERVICE_URL + "?op=KAKA", //$NON-NLS-1$
			UrlHttpMethod.GET
		);
		Assert.assertTrue(new String(res).startsWith("ERR-20")); //$NON-NLS-1$
		res = mgr.readUrl(
			SERVICE_URL + "?op=KAKA&v=PEDO", //$NON-NLS-1$
			UrlHttpMethod.GET
		);
		Assert.assertTrue(new String(res).startsWith("ERR-01")); //$NON-NLS-1$
		res = mgr.readUrl(
			SERVICE_URL + "?op=put&v=PEDO", //$NON-NLS-1$
			UrlHttpMethod.GET
		);
		Assert.assertTrue(new String(res).startsWith("ERR-05")); //$NON-NLS-1$
		res = mgr.readUrl(
			SERVICE_URL + "?op=put&v=PEDO&id=PIS.txt", //$NON-NLS-1$
			UrlHttpMethod.GET
		);
		Assert.assertEquals("OK", new String(res)); //$NON-NLS-1$
		res = mgr.readUrl(
			SERVICE_URL + "?op=put&v=PEDO&id=CACA.txt&dat=CULO", //$NON-NLS-1$
			UrlHttpMethod.GET
		);
		Assert.assertEquals("OK", new String(res)); //$NON-NLS-1$
	}

}
