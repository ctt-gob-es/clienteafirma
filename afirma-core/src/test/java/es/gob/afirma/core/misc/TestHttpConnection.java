package es.gob.afirma.core.misc;

import java.io.IOException;

import junit.framework.Assert;

import org.junit.Test;

/** Prueba de las conexiones HTTP y HTTPS. */
public final class TestHttpConnection {

	/** Prueba la conexi&oacute;n Https con un certificado no reconocido por Java.
	 * @throws IOException */
	@SuppressWarnings("static-method")
	@Test
	public void testHttpsConnection() throws IOException {
		Assert.assertNotNull(UrlHttpManagerImpl.readUrlByGet("https://www.google.es")); //$NON-NLS-1$
		Assert.assertNotNull(UrlHttpManagerImpl.readUrlByPost("https://forja-ctt.administracionelectronica.gob.es/")); //$NON-NLS-1$
	}

}
