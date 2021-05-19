package test.es.gob.afirma.signers.batch.server;

import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signers.batch.client.BatchSigner;

/** Pruebas del cliente de firma por lote.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestBatchSignerWithCounterSign {

	private static final String CERT_PATH = "PFActivoFirSHA1.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
	private static final String CERT_ALIAS = "fisico activo prueba"; //$NON-NLS-1$

	/** Prueba simple del cliente de firma por lote con contrafirma.
	 * @param args No se usa.
	 * @throws Exception En cualquier error. */
	public static void main(final String[] args) throws Exception {

		final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

		final PrivateKeyEntry pke;
		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try(
			final InputStream is = ClassLoader.getSystemResourceAsStream(CERT_PATH)
		) {
			ks.load(
				is,
				CERT_PASS.toCharArray()
			);
		}
		pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		final String res;
		try (
			final InputStream is = TestBatchSignerWithCounterSign.class.getResourceAsStream(
				"/batch-with-countersign.xml" //$NON-NLS-1$
			)
		) {
			res = BatchSigner.signXML(
				Base64.encode(
					AOUtil.getDataFromInputStream(
						is
					),
					true
				),
				"http://localhost:8080/afirma-server-triphase-signer/BatchPresigner", //$NON-NLS-1$
				"http://localhost:8080/afirma-server-triphase-signer/BatchPostsigner", //$NON-NLS-1$
				pke.getCertificateChain(),
				pke.getPrivateKey()
			);
		}
		LOGGER.info(res);
	}

}
