package es.gob.afirma.test.cades;

import java.io.File;
import java.io.FileOutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Before;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signers.cades.AOCAdESSigner;

/** Pruebas de firma de huellas. */
public class TestSignHash {

	private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
	private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

	private static final String DATA_FILE = "txt"; //$NON-NLS-1$

	PrivateKeyEntry pke = null;

	/** Carga el almac&eacute;n de claves.
	 * @throws Exception En cualquier error. */
	@Before
	public void loadResources() throws Exception {

		Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(TestSignHash.CERT_PATH), TestSignHash.CERT_PASS.toCharArray());
		this.pke = (PrivateKeyEntry) ks.getEntry(TestSignHash.CERT_ALIAS, new KeyStore.PasswordProtection(TestSignHash.CERT_PASS.toCharArray()));
	}

	/** Prueba de firma de huella SHA-1.
	 * @throws Exception en cualquier error. */
	@Test
	public void testSignHashSHA1() throws Exception {

		final String HASH_ALGORITHM = "SHA1"; //$NON-NLS-1$

		final byte[] data = AOUtil.getDataFromInputStream(TestCAdES.class.getResourceAsStream(TestSignHash.DATA_FILE));
		final byte[] hash = MessageDigest.getInstance(HASH_ALGORITHM).digest(data);

		final Properties config = new Properties();
		config.setProperty("precalculatedHashAlgorithm", HASH_ALGORITHM); //$NON-NLS-1$

		final AOSigner signer = new AOCAdESSigner();
		final byte[] signature = signer.sign(
				hash,
				AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
				this.pke.getPrivateKey(),
				this.pke.getCertificateChain(),
				config);

		final File outFile = File.createTempFile("signHash", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final FileOutputStream fos = new FileOutputStream(outFile);
		) {
			fos.write(signature);
		}
		System.out.println("La firma de hash " + HASH_ALGORITHM + " se ha guardado en el fichero: " + outFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Prueba de firma de huella SHA-512.
	 * @throws Exception en cualquier error. */
	@Test
	public void testSignHashSHA512() throws Exception {

		final String HASH_ALGORITHM = "SHA-512"; //$NON-NLS-1$

		final byte[] data = AOUtil.getDataFromInputStream(TestCAdES.class.getResourceAsStream(TestSignHash.DATA_FILE));
		final byte[] hash = MessageDigest.getInstance(HASH_ALGORITHM).digest(data);

		final Properties config = new Properties();
		config.setProperty("precalculatedHashAlgorithm", HASH_ALGORITHM); //$NON-NLS-1$

		final AOSigner signer = new AOCAdESSigner();
		final byte[] signature = signer.sign(
			hash,
			AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
			this.pke.getPrivateKey(),
			this.pke.getCertificateChain(),
			config
		);

		final File outFile = File.createTempFile("signHash", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final FileOutputStream fos = new FileOutputStream(outFile);
		) {
			fos.write(signature);
		}

		System.out.println("La firma de hash " + HASH_ALGORITHM + " se ha guardado en el fichero: " + outFile.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$
	}
}
