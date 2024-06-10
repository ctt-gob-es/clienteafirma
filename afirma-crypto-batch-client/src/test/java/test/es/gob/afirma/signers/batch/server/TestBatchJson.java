package test.es.gob.afirma.signers.batch.server;

import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.AOException;
import es.gob.afirma.signers.batch.client.BatchSigner;

public class TestBatchJson {

	private static final String CERT_PATH = "PFActivoFirSHA1.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
	private static final String CERT_ALIAS = "fisico activo prueba"; //$NON-NLS-1$

	private static final String BASE_URL = "http://appprueba:8080/afirma-server-triphase-signer/"; //$NON-NLS-1$

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
}
