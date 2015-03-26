package es.gob.afirma.test.simple;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.UrlHttpManagerFactory;
import es.gob.afirma.standalone.SimpleAfirma;

/** Pruebas de invocaci&oacute;n por protocolo.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public class TestProtocolInvocation {

	private static final String LINE_NODATA = "afirma://sign?op=sign&id=000954750801&key=70192563&stservlet=http://comparece.dipucr.es:8080/afirma-signature-storage/StorageService&format=CAdES&algorithm=SHA1withRSA&properties=bW9kZT1leHBsaWNpdApzZXJ2ZXJVcmw9aHR0cDovL2NvbXBhcmVjZS5kaXB1Y3IuZXM6ODA4MC9hZmlybWEtc2VydmVyLXRyaXBoYXNlLXNpZ25lci9TaWduYXR1cmVTZXJ2aWNl"; //$NON-NLS-1$
	private static final String LINE_CUSTOM_STORE = "afirma://sign?op=sign&keystore=Mozilla%20%2F%20Firefox%20(unificado)&id=000954750801&key=70192563&stservlet=http://comparece.dipucr.es:8080/afirma-signature-storage/StorageService&format=CAdES&algorithm=SHA1withRSA&properties=bW9kZT1leHBsaWNpdApzZXJ2ZXJVcmw9aHR0cDovL2NvbXBhcmVjZS5kaXB1Y3IuZXM6ODA4MC9hZmlybWEtc2VydmVyLXRyaXBoYXNlLXNpZ25lci9TaWduYXR1cmVTZXJ2aWNl"; //$NON-NLS-1$
	private static final String LINE_SAVE = "afirma://save?dat=bW9kZT1leHBsaWNpdApzZXJ2ZXJVcmw9aHR0cDovL2NvbXBhcmVjZS5kaXB1Y3IuZXM6ODA4MC9hZmlybWEtc2VydmVyLXRyaXBoYXNlLXNpZ25lci9TaWduYXR1cmVTZXJ2aWNl"; //$NON-NLS-1$

	private static final String LINE_SERVICE = "afirma://service?ports=1234,4321&unused=dummy"; //$NON-NLS-1$

	// firma://warmup

	/** Prueba de protocolo sin indicar datos. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita UI
	public void testWithoutData() {
		SimpleAfirma.main(new String[] { LINE_NODATA });
	}

	/** Prueba de protocolo con almac&eacute;n de claves establecido manualmente. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita NSS
	public void testCustomStore() {
		SimpleAfirma.main(new String[] { LINE_CUSTOM_STORE });
	}

	/** Prueba de protocolo para servicio local.
	 * @throws Exception En cualquier error */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita UI
	public void testService() throws Exception {
		new Thread(
			new Runnable() {
				@Override
				public void run() {
					SimpleAfirma.main(new String[] { LINE_SERVICE });
				}
			}
		).start();

		Thread.sleep(4000);

		final byte[] res = UrlHttpManagerFactory.getInstalledManager().readUrlByPost(
			"http://127.0.0.1:1234/kaka?cmd=" + Base64.encode(LINE_SAVE.getBytes(), true) //$NON-NLS-1$
		);

		System.out.println("RES=" + new String(Base64.decode(new String(res)))); //$NON-NLS-1$

	}

}
