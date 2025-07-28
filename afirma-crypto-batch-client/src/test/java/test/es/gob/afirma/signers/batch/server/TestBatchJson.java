package test.es.gob.afirma.signers.batch.server;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signers.batch.client.BatchSigner;

public class TestBatchJson {

	private static final String CERT_PATH = "00_colegiado-hsm_revoked.p12"; //$NON-NLS-1$
	private static final String CERT_PASS = "1234"; //$NON-NLS-1$
	private static final String CERT_ALIAS = "nombre apellido1 apellido2  / num:1111"; //$NON-NLS-1$

	private static final String BASE_URL = "https://appprueba:8443/afirma-server-triphase-signer/"; //$NON-NLS-1$

	private Certificate[] certChain;
	private PrivateKey pk;

	@Before
	public void loadPrivateKeyEntry() throws Exception {
		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try (final InputStream is = ClassLoader.getSystemResourceAsStream(CERT_PATH)) {
			ks.load(is, CERT_PASS.toCharArray());
		}
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
		this.certChain = pke.getCertificateChain();
		this.pk = pke.getPrivateKey();
	}


	@Test
	@Ignore
	public void testBatch() throws CertificateEncodingException, IOException, AOException {

		final String batchPreSignerUrl = BASE_URL + "presign"; //$NON-NLS-1$
		final String batchPostSignerUrl = BASE_URL + "postsign"; //$NON-NLS-1$

//		final String batch = "{\"algorithm\":\"SHA256\", \"format\":\"CAdES\", \"singlesigns\":[{\"id\":\"1\", \"datareference\":\"RW50cmFkYS50eHQ=\"}]}";
//		final String batchB64 = Base64.encode(batch.getBytes(StandardCharsets.UTF_8));
		final String batchB64 = "eyJhbGdvcml0aG0iOiJTSEEyNTYiLCJmb3JtYXQiOiJDQWRFUyIsInN1Ym9wZXJhdGlvbiI6InNpZ24iLCJzaW5nbGVzaWducyI6W3siaWQiOiI5YzAyNjY5Ny03MTI0LTRkMGItOGMyOS1kNDllNjJmZjUzMzEiLCJkYXRhcmVmZXJlbmNlIjoiU0c5c1lTQk5kVzVrYnlFaCJ9LHsiaWQiOiJkNzcwMjI4Zi1jMDMyLTQxMTktODYwNS0wYTQzOTcyMjExMDEiLCJkYXRhcmVmZXJlbmNlIjoiU0c5c1lTQk5kVzVrYnlFaElEST0iLCJmb3JtYXQiOiJYQWRFUyIsInN1Ym9wZXJhdGlvbiI6InNpZ24iLCJleHRyYXBhcmFtcyI6IlptOXliV0YwUFZoQlpFVlRJRVJsZEdGamFHVmtDbVY0Y0ZCdmJHbGplVDFHYVhKdFlVRkhSUT09In1dLCJzdG9wb25lcnJvciI6ZmFsc2V9"; //$NON-NLS-1$

		final String result = BatchSigner.signJSON(batchB64, batchPreSignerUrl, batchPostSignerUrl, this.certChain, this.pk);

		System.out.println("Resultado:\n" + result); //$NON-NLS-1$
	}

	@Test
	@Ignore
	public void testBatchWithCounterSigns() throws CertificateEncodingException, IOException, AOException {

		final String batchPreSignerUrl = BASE_URL + "presign"; //$NON-NLS-1$
		final String batchPostSignerUrl = BASE_URL + "postsign"; //$NON-NLS-1$


		//final byte[] dataRef = readFile(new File("C:\\Users\\carlos.gamuci\\Desktop\\Entrada\\cofirma.csig"));
		final byte[] dataRef = "cofirma.csig".getBytes();
		final String dataRefB64 = Base64.encode(dataRef);

		final String batch = "{\"algorithm\":\"SHA256\", \"format\":\"CAdES\", \"suboperation\":\"countersign\", \"singlesigns\":[{\"id\":\"1\", \"datareference\":\"" + dataRefB64 + "\"}, "
				+ "{\"id\":\"2\", \"datareference\":\"" + dataRefB64 + "\", \"format\":\"XAdES\", \"suboperation\":\"sign\", \"extraParams\":\"format=XAdES Detached\"}]}";
		final String batchB64 = Base64.encode(batch.getBytes(StandardCharsets.UTF_8));

		final String result = BatchSigner.signJSON(batchB64, batchPreSignerUrl, batchPostSignerUrl, this.certChain, this.pk);

		System.out.println("Resultado:\n" + result); //$NON-NLS-1$
	}

	private static final byte[] readFile(final File dataFile) {
		byte[] data;
		try (InputStream is = new FileInputStream(dataFile)) {
			data = AOUtil.getDataFromInputStream(is);
		}
		catch (final Exception e) {
			e.printStackTrace();
			data = null;
		}
		return data;
	}
}
