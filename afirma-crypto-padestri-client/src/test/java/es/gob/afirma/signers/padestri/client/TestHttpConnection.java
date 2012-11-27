package es.gob.afirma.signers.padestri.client;

import java.io.IOException;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.afirma.signers.padestri.client.UrlHttpManager;
import es.gob.afirma.signers.padestri.client.UrlHttpManagerImpl;

/** Prueba de las conexiones HTTP y HTTPS. */
public final class TestHttpConnection {

	/** Prueba la conexi&oacute;n Https con un certificado no reconocido por Java.
	 * @throws IOException */
	@SuppressWarnings("static-method")
	@Test
	public void testHttpsConnection() throws IOException {
		final UrlHttpManager mgr = new UrlHttpManagerImpl();
		Assert.assertNotNull(mgr.readUrl("https://www.google.es")); //$NON-NLS-1$
		Assert.assertNotNull(mgr.readUrl("https://forja-ctt.administracionelectronica.gob.es/")); //$NON-NLS-1$
	}

}
