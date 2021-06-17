package es.gob.afirma.test.simple;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.standalone.SimpleAfirma;

/** Pruebas de invocaci&oacute;n por protocolo para lotes de firma. */
public class TestProtocolInvocationJSON {

	private static final String SAMPLE_BATCH_JSON =
			"{" + //$NON-NLS-1$
			"\"id\":\"PETICION-TEST-001\"," + //$NON-NLS-1$
			"\"stoponerror\": false," + //$NON-NLS-1$
			"\"format\": \"PAdES\"," + //$NON-NLS-1$
			"\"suboperation\":\"sign\"," + //$NON-NLS-1$
			"\"algorithm\": \"SHA256withRSA\"," + //$NON-NLS-1$
			"\"singlesigns\": [ " + //$NON-NLS-1$
				"{" + //$NON-NLS-1$
					"\"id\":\"7725374e-728d-4a33-9db9-3a4efea4cead\"," + //$NON-NLS-1$
					"\"datareference\":\"cHJ1ZWJhMS54bWw=\"," + //$NON-NLS-1$
					"\"format\":\"XAdES\"," + //$NON-NLS-1$
					"\"suboperation\":\"sign\"," + //$NON-NLS-1$
					"\"extraparams\":\"Iw0KI1RodSBBdWcgMTMgMTY6Mjk6MDUgQ0VTVCAyMDE1DQpTaWduYXR1cmVJZD03NzI1Mzc0ZS03MjhkLTRhMzMtOWRiOS0zYTRlZmVhNGNlYWQNCg==\"" + //$NON-NLS-1$
				"}," + //$NON-NLS-1$
				"{" + //$NON-NLS-1$
					"\"id\":\"93d1531c-cd32-4c8e-8cc8-1f1cfe66f64a\"," + //$NON-NLS-1$
					"\"datareference\":\"dGVzdF9maWNoZXJvMS5wZGY=\"," + //$NON-NLS-1$
					"\"extraparams\":\"Iw0KI1RodSBBdWcgMTMgMTY6Mjk6MDUgQ0VTVCAyMDE1DQpTaWduYXR1cmVJZD05M2QxNTMxYy1jZDMyLTRjOGUtOGNjOC0xZjFjZmU2NmY2NGENCg==\"" + //$NON-NLS-1$
				"}" + //$NON-NLS-1$
			"]" + //$NON-NLS-1$
			"}"; //$NON-NLS-1$

	private static final String LINE = "afirma://batch?" //$NON-NLS-1$
		+ "key=70192563&" //$NON-NLS-1$
		+ "dat=" + Base64.encode(SAMPLE_BATCH_JSON.getBytes(), true) + "&" //$NON-NLS-1$ //$NON-NLS-2$
		+ "batchpostsignerurl=" + "http://appprueba:8080/afirma-server-triphase-signer/JSONBatchPostsigner&" + "&" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		+ "batchpresignerurl=" + "http://appprueba:8080/afirma-server-triphase-signer/JSONBatchPresigner&" //$NON-NLS-1$//$NON-NLS-2$
		+ "jsonBatch=true"  //$NON-NLS-1$
	;

	/** Prueba de firma por lotes. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita UI
	public void testWithoutData() {
		SimpleAfirma.main(new String[] { LINE });
	}

}
