package es.gob.afirma.standalone;

import java.io.File;
import java.io.FileOutputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.signers.xades.AOXAdESSigner;

/** Clase de prueba de firma con JMulticard a trav&eacute;s del almac&eacute;n de Firefox. Se incluye
 * en este proyecto por disponer de todas las bibliotecas necesarias. */
public final class TestJMulticardSignature {

	/** Realiza una firma XAdES con el certificado del DNIe a trav&eacute;s del
	 * almac&eacute;n de Mozilla Firefox.
	 * @throws Exception Cuando ocurre cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore // Necesita DNIe
	public void testFirmaXAdESJMulticardSignature() throws Exception {

		final AOKeyStore ks = AOKeyStore.MOZ_UNI;
		final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
			ks,
			null,
			null,
			ks.getStorePasswordCallback(null),
			null
		);

		String selectedAlias = null;
		for (final String alias : ksm.getAliases()) {
			System.out.println("Alias: " + alias); //$NON-NLS-1$
			if (alias.contains("CertFirmaDigital")) { //$NON-NLS-1$
				selectedAlias = alias;
				break;
			}
		}

		Assert.assertNotNull("No se ha encontrado el certificado de firma del DNIe", selectedAlias); //$NON-NLS-1$

		final Properties config = new Properties();
		config.setProperty("includeOnlySigningCertificate", "false"); //$NON-NLS-1$ //$NON-NLS-2$

		final PrivateKeyEntry pke = ksm.getKeyEntry(selectedAlias);

		final AOSigner signer = new AOXAdESSigner();
		final byte[] signature = signer.sign(
				"Hola Mundo".getBytes(), //$NON-NLS-1$
				AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				config
		);

		final File tempFile = File.createTempFile("test-xades-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
		try (final FileOutputStream fos = new FileOutputStream(tempFile)) {
			fos.write(signature);
		}

		System.out.println("La firma XAdES con certificado de DNIe se ha guardado en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
	}
}
