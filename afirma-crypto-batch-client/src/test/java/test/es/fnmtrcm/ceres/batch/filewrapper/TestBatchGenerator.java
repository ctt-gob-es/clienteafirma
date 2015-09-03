package test.es.fnmtrcm.ceres.batch.filewrapper;

import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;

import org.junit.Test;

import es.fnmtrcm.ceres.batch.filewrapper.BatchGenerator;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signers.batch.client.BatchSigner;

/** Pruebas de generaci&oacute;n de XML de lote de firma en base a directorios.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestBatchGenerator {

	private static final String INPATH = "C:\\Temp\\caca"; //$NON-NLS-1$
	private static final String OUTPATH = "c:\\temp"; //$NON-NLS-1$

	private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
	private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

	/** Prueba simple de generaci&oacute;n de lote de firma en base a directorios.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testBatchGeneration() throws Exception {
		System.out.println(
			BatchGenerator.generateBatchXml(
				INPATH,
				OUTPATH,
				".pdf", //$NON-NLS-1$
				null,
				"PAdES" //$NON-NLS-1$
			)
		);
	}

	/** Prueba simple de generaci&oacute;n y ekecuci&oacute;nde lote de firma en base a directorios.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testBatchGenerationAndExecution() throws Exception {

		final String xml = BatchGenerator.generateBatchXml(
			INPATH,
			OUTPATH,
			".pdf", //$NON-NLS-1$
			null,
			"PAdES" //$NON-NLS-1$
		);

		System.out.println(xml);

		final PrivateKeyEntry pke;
		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		BatchSigner.sign(
			Base64.encode(xml.getBytes()),
			"http://demo.com:8080/afirma-server-triphase-signer/BatchPresigner", //$NON-NLS-1$
			"http://demo.com:8080/afirma-server-triphase-signer/BatchPostsigner", //$NON-NLS-1$
			pke.getCertificateChain(),
			pke.getPrivateKey()
		);
	}

}
