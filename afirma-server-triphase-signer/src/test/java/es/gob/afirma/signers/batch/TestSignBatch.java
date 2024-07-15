package es.gob.afirma.signers.batch;

import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.signers.TriphaseDataSigner;
import es.gob.afirma.signers.batch.SingleSignConstants.DigestAlgorithm;
import es.gob.afirma.signers.batch.SingleSignConstants.SignFormat;
import es.gob.afirma.signers.batch.SingleSignConstants.SignSubOperation;
import es.gob.afirma.signers.batch.xml.SignBatch;
import es.gob.afirma.signers.batch.xml.SignBatchConcurrent;
import es.gob.afirma.signers.batch.xml.SignBatchSerial;
import es.gob.afirma.signers.batch.xml.SingleSign;

/** Pruebas de firma por lotes.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestSignBatch {

	private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
	private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

	private static final String SAMPLE_BATCH_XML =
		"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\r\n" + //$NON-NLS-1$
		"<signbatch stoponerror=\"false\" algorithm=\"SHA256withRSA\" Id=\"LOTE001\">\r\n" + //$NON-NLS-1$
		" <singlesign Id=\"7725374e-728d-4a33-9db9-3a4efea4cead\">\r\n" + //$NON-NLS-1$
		"  <datasource>https://google.com</datasource>\r\n" + //$NON-NLS-1$
		"  <format>XAdES</format>\r\n" + //$NON-NLS-1$
		"  <suboperation>sign</suboperation>\r\n" + //$NON-NLS-1$
		"  <extraparams>Iw0KI1RodSBBdWcgMTMgMTY6Mjk6MDUgQ0VTVCAyMDE1DQpTaWduYXR1cmVJZD03NzI1Mzc0ZS03MjhkLTRhMzMtOWRiOS0zYTRlZmVhNGNlYWQNCg==</extraparams>\r\n" + //$NON-NLS-1$
		"  <signsaver>\r\n" + //$NON-NLS-1$
		"   <class>es.gob.afirma.signers.batch.SignSaverFile</class>\r\n" + //$NON-NLS-1$
		"   <config>Iw0KI1RodSBBdWcgMTMgMTY6Mjk6MDUgQ0VTVCAyMDE1DQpGaWxlTmFtZT1DXDpcXFVzZXJzXFx0b21hc1xcQXBwRGF0YVxcTG9jYWxcXFRlbXBcXEZJUk1BMS54bWwNCg==</config>\r\n" + //$NON-NLS-1$
		"  </signsaver>\r\n" + //$NON-NLS-1$
		" </singlesign>\r\n" + //$NON-NLS-1$
		" <singlesign Id=\"93d1531c-cd32-4c8e-8cc8-1f1cfe66f64a\">\r\n" + //$NON-NLS-1$
		"  <datasource>SG9sYSBNdW5kbw==</datasource>\r\n" + //$NON-NLS-1$
		"  <format>CAdES</format>\r\n" + //$NON-NLS-1$
		"  <suboperation>sign</suboperation>\r\n" + //$NON-NLS-1$
		"  <extraparams>Iw0KI1RodSBBdWcgMTMgMTY6Mjk6MDUgQ0VTVCAyMDE1DQpTaWduYXR1cmVJZD05M2QxNTMxYy1jZDMyLTRjOGUtOGNjOC0xZjFjZmU2NmY2NGENCg==</extraparams>\r\n" + //$NON-NLS-1$
		"  <signsaver>\r\n" + //$NON-NLS-1$
		"   <class>es.gob.afirma.signers.batch.SignSaverFile</class>\r\n" + //$NON-NLS-1$
		"   <config>Iw0KI1RodSBBdWcgMTMgMTY6Mjk6MDUgQ0VTVCAyMDE1DQpGaWxlTmFtZT1DXDpcXFVzZXJzXFx0b21hc1xcQXBwRGF0YVxcTG9jYWxcXFRlbXBcXEZJUk1BMi54bWwNCg==</config>\r\n" + //$NON-NLS-1$
		"  </signsaver>\r\n" + //$NON-NLS-1$
		" </singlesign>\r\n" + //$NON-NLS-1$
		"</signbatch>"; //$NON-NLS-1$

	private static final String SAMPLE_BATCH_XML_ERROR =
			"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\r\n" + //$NON-NLS-1$
			"<signbatch stoponerror=\"true\" algorithm=\"SHA512withRSA\">\r\n" + //$NON-NLS-1$
			" <singlesign Id=\"7725374e-728d-4a33-9db9-3a4efea4cead\">\r\n" + //$NON-NLS-1$
			"  <datasource>https://google.com</datasource>\r\n" + //$NON-NLS-1$
			"  <format>XAdES</format>\r\n" + //$NON-NLS-1$
			"  <suboperation>sign</suboperation>\r\n" + //$NON-NLS-1$
			"  <extraparams>Iw0KI1RodSBBdWcgMTMgMTY6Mjk6MDUgQ0VTVCAyMDE1DQpTaWduYXR1cmVJZD03NzI1Mzc0ZS03MjhkLTRhMzMtOWRiOS0zYTRlZmVhNGNlYWQNCg==</extraparams>\r\n" + //$NON-NLS-1$
			"  <signsaver>\r\n" + //$NON-NLS-1$
			"   <class>es.gob.afirma.signers.batch.FaultySignSaver</class>\r\n" + //$NON-NLS-1$
			"   <config>Iw0KI1RodSBBdWcgMTMgMTY6Mjk6MDUgQ0VTVCAyMDE1DQpGaWxlTmFtZT1DXDpcXFVzZXJzXFx0b21hc1xcQXBwRGF0YVxcTG9jYWxcXFRlbXBcXEZJUk1BMS54bWwNCg==</config>\r\n" + //$NON-NLS-1$
			"  </signsaver>\r\n" + //$NON-NLS-1$
			" </singlesign>\r\n" + //$NON-NLS-1$
			" <singlesign Id=\"93d1531c-cd32-4c8e-8cc8-1f1cfe66f64a\">\r\n" + //$NON-NLS-1$
			"  <datasource>SG9sYSBNdW5kbw==</datasource>\r\n" + //$NON-NLS-1$
			"  <format>CAdES</format>\r\n" + //$NON-NLS-1$
			"  <suboperation>sign</suboperation>\r\n" + //$NON-NLS-1$
			"  <extraparams>Iw0KI1RodSBBdWcgMTMgMTY6Mjk6MDUgQ0VTVCAyMDE1DQpTaWduYXR1cmVJZD05M2QxNTMxYy1jZDMyLTRjOGUtOGNjOC0xZjFjZmU2NmY2NGENCg==</extraparams>\r\n" + //$NON-NLS-1$
			"  <signsaver>\r\n" + //$NON-NLS-1$
			"   <class>es.gob.afirma.signers.batch.SignSaverFile</class>\r\n" + //$NON-NLS-1$
			"   <config>Iw0KI1RodSBBdWcgMTMgMTY6Mjk6MDUgQ0VTVCAyMDE1DQpGaWxlTmFtZT1DXDpcXFVzZXJzXFx0b21hc1xcQXBwRGF0YVxcTG9jYWxcXFRlbXBcXEZJUk1BMi54bWwNCg==</config>\r\n" + //$NON-NLS-1$
			"  </signsaver>\r\n" + //$NON-NLS-1$
			" </singlesign>\r\n" + //$NON-NLS-1$
			"</signbatch>"; //$NON-NLS-1$

	/** Prueba de creaci&oacute;n de lote de firmas a partir de su definici&oacute;n XML.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore //Requiere servicio servidor
	public void testXmlParse() throws Exception {
		System.out.println(new SignBatchSerial(SAMPLE_BATCH_XML.getBytes()));
	}

	private static SignBatch createSampleBatch(final boolean concurrent) throws InstantiationException,
	                                                                            IllegalAccessException,
	                                                                            ClassNotFoundException,
	                                                                            IllegalArgumentException,
	                                                                            InvocationTargetException,
	                                                                            NoSuchMethodException,
	                                                                            SecurityException {

		final String tempDir = System.getProperty("java.io.tmpdir"); //$NON-NLS-1$

		System.out.println("Las firmas se almacenaran en " + tempDir); //$NON-NLS-1$

		//**************************************************************************************
		//************ CREACION DE LOTE ********************************************************
		final Properties ssvp1 = new Properties();
		ssvp1.put("FileName", tempDir + "\\FIRMA1.xml"); //$NON-NLS-1$ //$NON-NLS-2$
		final SignSaver ssv1 = (SignSaver) Class.forName("es.gob.afirma.signers.batch.SignSaverFile").getDeclaredConstructor().newInstance(); //$NON-NLS-1$
		ssv1.init(ssvp1);

		final SingleSign ss1 = new SingleSign(
			"001-XAdES", //$NON-NLS-1$
			"https://google.com", //$NON-NLS-1$
			SignFormat.XADES,
			SignSubOperation.SIGN,
			null,
			ssv1
		);

		final Properties ssvp2 = new Properties();
		ssvp2.put("FileName", tempDir + "\\FIRMA2.xml"); //$NON-NLS-1$ //$NON-NLS-2$
		final SignSaver ssv2 = (SignSaver) Class.forName("es.gob.afirma.signers.batch.SignSaverFile").getDeclaredConstructor().newInstance(); //$NON-NLS-1$
		ssv2.init(ssvp2);

		final SingleSign ss2 = new SingleSign(
			"002-CAdES", //$NON-NLS-1$
			"SG9sYSBNdW5kbw==", //$NON-NLS-1$
			SignFormat.CADES,
			SignSubOperation.SIGN,
			null,
			ssv2
		);

		final Properties ssvp3 = new Properties();
		ssvp3.put("FileName", tempDir + "\\FIRMA3.xml"); //$NON-NLS-1$ //$NON-NLS-2$
		final SignSaver ssv3 = (SignSaver) Class.forName("es.gob.afirma.signers.batch.SignSaverFile").getDeclaredConstructor().newInstance(); //$NON-NLS-1$
		ssv3.init(ssvp3);

		final SingleSign ss3 = new SingleSign(
			"003-CAdES", //$NON-NLS-1$
			"https://ibm.com", //$NON-NLS-1$
			SignFormat.CADES,
			SignSubOperation.SIGN,
			null,
			ssv3
		);

		final Properties ssvp4 = new Properties();
		ssvp4.put("FileName", tempDir + "\\FIRMA4.xml"); //$NON-NLS-1$ //$NON-NLS-2$
		final SignSaver ssv4 = (SignSaver) Class.forName("es.gob.afirma.signers.batch.SignSaverFile").getDeclaredConstructor().newInstance(); //$NON-NLS-1$
		ssv4.init(ssvp4);

		final Properties extraParams4 = new Properties();
		extraParams4.setProperty("expPolicy", "FirmaAGE"); //$NON-NLS-1$ //$NON-NLS-2$

		final SingleSign ss4 = new SingleSign(
			"004-XAdES", //$NON-NLS-1$
			"https://atos.net", //$NON-NLS-1$
			SignFormat.XADES,
			SignSubOperation.SIGN,
			extraParams4,
			ssv4
		);

		final List<SingleSign> signs = new ArrayList<>();
		signs.add(ss1);
		signs.add(ss2);
		signs.add(ss3);
		signs.add(ss4);

		if (concurrent) {
			return new SignBatchConcurrent(signs, SingleSignConstants.DigestAlgorithm.SHA384, true);
		}
		return new SignBatchSerial(signs, SingleSignConstants.DigestAlgorithm.SHA384, true);


		//************ FIN CREACION DE LOTE ****************************************************
		//**************************************************************************************

	}

	/** Prueba de un lote construido manualmente.
	 * @throws Exception Cuando falla de forma no controlada el test. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore //Requiere servicio servidor
	public void testCustomBatch() throws Exception {

		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

		//**************************************************************************************
		//********** OBTENCION DE CLAVE ********************************************************
		final PrivateKeyEntry pke;
		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try (final InputStream is = ClassLoader.getSystemResourceAsStream(CERT_PATH)) {
			ks.load(is, CERT_PASS.toCharArray());
		}
		pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
		//********** FIN OBTENCION DE CLAVE ****************************************************
		//**************************************************************************************

		final SignBatch batch = createSampleBatch(false);

		System.out.println();
		System.out.println();
		System.out.println(batch);
		System.out.println();
		System.out.println();

		//**************************************************************************************
		//************ PROCESO DEL LOTE ********************************************************

		final String pre = batch.doPreBatch((X509Certificate[]) pke.getCertificateChain());

		// TD1 es lo que recibe el cliente tras su peticion enviando un XML
		final TriphaseData td1 = TriphaseData.parser(pre.getBytes());

		System.out.println();
		System.out.println();
		System.out.println(td1);
		System.out.println();
		System.out.println();

		// El cliente hace los PKCS#1 generando TD2, que envia de nuevo al servidor
		final TriphaseData td2 = TriphaseDataSigner.doSign(
			new AOPkcs1Signer(),
			DigestAlgorithm.SHA512.getName(),
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			td1,
			null
		);

		System.out.println(td2);
		System.out.println();
		System.out.println();

		// El servidor haria este postproceso...
		System.out.println(
			batch.doPostBatch(
				(X509Certificate[]) pke.getCertificateChain(),
				td2
			)
		);

		//************ FIN PROCESO DEL LOTE ****************************************************
		//**************************************************************************************

	}

	/** Prueba de un lote construido manualmente.
	 * @throws Exception Cuando falla de forma no controlada el test. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore //Requiere servicio servidor
	public void testConcurrentCustomBatch() throws Exception {

		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
		final PrivateKeyEntry pke;

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try (final InputStream is = ClassLoader.getSystemResourceAsStream(CERT_PATH)) {
			ks.load(is, CERT_PASS.toCharArray());
		}
		pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final SignBatch batch = createSampleBatch(true);

		final long current = System.currentTimeMillis();

		final String pre = batch.doPreBatch((X509Certificate[]) pke.getCertificateChain());

		// TD1 es lo que recibe el cliente tras su peticion enviando un XML
		final TriphaseData td1 = TriphaseData.parser(pre.getBytes());

		System.out.println();
		System.out.println();
		//System.out.println(td1);
		System.out.println();
		System.out.println();

		// El cliente hace los PKCS#1 generando TD2, que envia de nuevo al servidor
		final TriphaseData td2 = TriphaseDataSigner.doSign(
			new AOPkcs1Signer(),
			DigestAlgorithm.SHA512.getName(),
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			td1,
			null
		);

		//System.out.println(td2);
		System.out.println();
		System.out.println();

		// El servidor haria este postproceso...
		System.out.println(
			batch.doPostBatch(
				(X509Certificate[]) pke.getCertificateChain(),
				td2
			)
		);

		System.out.println("Tiempo tardado en paralelo: " + (System.currentTimeMillis() - current)); //$NON-NLS-1$
	}

	/** Prueba de un lote construido manualmente.
	 * @throws Exception Cuando falla de forma no controlada el test. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore //Requiere servicio servidor
	public void testCustomBatchWithError() throws Exception {

		//Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
		final PrivateKeyEntry pke;

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try (final InputStream is = ClassLoader.getSystemResourceAsStream(CERT_PATH)) {
			ks.load(is, CERT_PASS.toCharArray());
		}
		pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final SignBatch batch = new SignBatchSerial(SAMPLE_BATCH_XML_ERROR.getBytes());

		System.out.println();
		System.out.println();
		System.out.println(batch);
		System.out.println();
		System.out.println();

		final String pre = batch.doPreBatch((X509Certificate[]) pke.getCertificateChain());

		// TD1 es lo que recibe el cliente tras su peticion enviando un XML
		final TriphaseData td1 = TriphaseData.parser(pre.getBytes());

		System.out.println();
		System.out.println();
		System.out.println(td1);
		System.out.println();
		System.out.println();

		// El cliente hace los PKCS#1 generando TD2, que envia de nuevo al servidor
		final TriphaseData td2 = TriphaseDataSigner.doSign(
			new AOPkcs1Signer(),
			DigestAlgorithm.SHA512.getName(),
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			td1,
			null
		);

		System.out.println(td2);
		System.out.println();
		System.out.println();


		// El servidor haria este postproceso...
		System.out.println(batch.doPostBatch(
			(X509Certificate[]) pke.getCertificateChain(),
			td2
		));

	}

}
