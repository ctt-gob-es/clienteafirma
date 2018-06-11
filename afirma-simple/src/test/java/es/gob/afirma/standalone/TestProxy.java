package es.gob.afirma.standalone;

import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.misc.http.UrlHttpMethod;

/** Pruebas de proxy.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestProxy {

	/** Main para pruebas.
	 * @param args No se usa.
	 * @throws Exception En cualquier error. */
	public static void main(final String[] args) throws Exception {
		ProxyUtil.setProxySettings();
		String str;

		// URL con excepcion
		str = new String(UrlHttpManagerFactory.getInstalledManager().readUrl("https://demo.tgm", UrlHttpMethod.GET)); //$NON-NLS-1$
		System.out.println(str);

		// URL general
		str = new String(UrlHttpManagerFactory.getInstalledManager().readUrl("https://google.com", UrlHttpMethod.GET)); //$NON-NLS-1$
		System.out.println(str);

	}

}
