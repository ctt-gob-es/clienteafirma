package es.gob.afirma.test.simple;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.standalone.SimpleAfirma;

/** Pruebas de invocaci&oacute;n por protocolo para lotes de firma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public class TestProtocolInvocationBatch {

	private static final String SAMPLE_BATCH_XML =
			"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\r\n" + //$NON-NLS-1$
			"<signbatch stoponerror=\"false\" algorithm=\"SHA256withRSA\">\r\n" + //$NON-NLS-1$
			" <singlesign Id=\"7725374e-728d-4a33-9db9-3a4efea4cead\">\r\n" + //$NON-NLS-1$
			"  <datasource>QzovcHJ1ZWJhcy9wcnVlYmExLnhtbA==</datasource>\r\n" + //$NON-NLS-1$
			"  <format>XAdES</format>\r\n" + //$NON-NLS-1$
			"  <suboperation>sign</suboperation>\r\n" + //$NON-NLS-1$
			"  <extraparams>Iw0KI1RodSBBdWcgMTMgMTY6Mjk6MDUgQ0VTVCAyMDE1DQpTaWduYXR1cmVJZD03NzI1Mzc0ZS03MjhkLTRhMzMtOWRiOS0zYTRlZmVhNGNlYWQNCg==</extraparams>\r\n" + //$NON-NLS-1$
			"  <signsaver>\r\n" + //$NON-NLS-1$
			"   <class>es.gob.afirma.signers.batch.SignSaverFile</class>\r\n" + //$NON-NLS-1$
			"   <config>QzovcHJ1ZWJhcy9yZXN1bHRhZG8xLnhtbA==</config>\r\n" + //$NON-NLS-1$
			"  </signsaver>\r\n" + //$NON-NLS-1$
			" </singlesign>\r\n" + //$NON-NLS-1$
			" <singlesign Id=\"93d1531c-cd32-4c8e-8cc8-1f1cfe66f64a\">\r\n" + //$NON-NLS-1$
			"  <datasource>QzovcHJ1ZWJhcy9wcnVlYmEyLnhtbA==</datasource>\r\n" + //$NON-NLS-1$
			"  <format>CAdES</format>\r\n" + //$NON-NLS-1$
			"  <suboperation>sign</suboperation>\r\n" + //$NON-NLS-1$
			"  <extraparams>Iw0KI1RodSBBdWcgMTMgMTY6Mjk6MDUgQ0VTVCAyMDE1DQpTaWduYXR1cmVJZD05M2QxNTMxYy1jZDMyLTRjOGUtOGNjOC0xZjFjZmU2NmY2NGENCg==</extraparams>\r\n" + //$NON-NLS-1$
			"  <signsaver>\r\n" + //$NON-NLS-1$
			"   <class>es.gob.afirma.signers.batch.SignSaverFile</class>\r\n" + //$NON-NLS-1$
			"   <config>QzovcHJ1ZWJhcy9yZXN1bHRhZG8yLnhtbA==</config>\r\n" + //$NON-NLS-1$
			"  </signsaver>\r\n" + //$NON-NLS-1$
			" </singlesign>\r\n" + //$NON-NLS-1$
			"</signbatch>"; //$NON-NLS-1$

	private static final String LINE = "afirma://batch?" //$NON-NLS-1$
		+ "key=70192563&" //$NON-NLS-1$
		+ "stservlet=http://appprueba:8080/afirma-signature-storage/StorageService&" //$NON-NLS-1$
		+ "rtservlet=http://appprueba:8080/afirma-signature-storage/RetrieveService&" //$NON-NLS-1$
		+ "dat=" + Base64.encode(SAMPLE_BATCH_XML.getBytes(), true) + "&" //$NON-NLS-1$ //$NON-NLS-2$
		+ "batchpostsignerurl=" + "http://appprueba:8080/afirma-server-triphase-signer/BatchPostsigner" + "&" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		+ "batchpresignerurl=" + "http://appprueba:8080/afirma-server-triphase-signer/BatchPresigner" //$NON-NLS-1$ //$NON-NLS-2$
	;

	/** Prueba de firma por lotes. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita UI
	public void testWithoutData() {
		SimpleAfirma.main(new String[] { LINE });
	}

}
