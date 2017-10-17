package es.gob.afirma.core.misc.http;

import java.io.IOException;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

/** Prueba de las conexiones HTTP y HTTPS. */
public final class TestHttpConnection {

	/** Prueba la conexi&oacute;n Https con un certificado no reconocido por Java.
	 * @throws IOException En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testHttpsConnection() throws IOException {
		final byte[] webPage = new es.gob.afirma.core.misc.http.UrlHttpManagerImpl().readUrl(
			"https://valide.redsara.es/valide/",  //$NON-NLS-1$
			UrlHttpMethod.GET
		);
		Assert.assertNotNull(
			webPage
		);
		System.out.println(new String(webPage));
	}
}
