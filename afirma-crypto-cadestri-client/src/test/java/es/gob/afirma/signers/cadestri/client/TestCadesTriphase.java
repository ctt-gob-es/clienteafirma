package es.gob.afirma.signers.cadestri.client;

import java.io.IOException;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;
import java.util.logging.Logger;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSigner;

/** Pruebas de firma CAdES trif&aacute;sica.
 * @author Tom&acute;s Garc&iacute;a-Mer&aacute;s */
public final class TestCadesTriphase {

	/** Nombre de la propiedad de URL del servidor de firma trif&aacute;sica. */
	private static final String PROPERTY_NAME_SIGN_SERVER_URL = "serverUrl"; //$NON-NLS-1$
	private static final String PROPERTY_VALUE_SIGN_SERVER_URL = "http://localhost:8080/TriPhaseSignerServer/SignatureService"; //$NON-NLS-1$

	// ID del documento, en este caso el documento en si
	private static final String PROPERTY_NAME_DOC_ID = "documentId"; //$NON-NLS-1$
	//private static final String PROPERTY_VALUE_DOC_ID = "SG9sYSBNdW5kbw=="; //$NON-NLS-1$
	private static final String PROPERTY_VALUE_DOC_ID = "Entrada.pdf"; //$NON-NLS-1$


	// Almacen de pruebas
	private static final String CERT_PATH = "ANF PFISICA ACTIVO.pfx"; //$NON-NLS-1$
	private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
	private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

	private Properties serverConfig;
	private PrivateKeyEntry pke;

	/** Prueba de firma CAdES trif&aacute;sica.
	 * @throws AOException
	 * @throws IOException */
	@Test
	@Ignore // Necesita el servidor
	public void firma() throws AOException, IOException {
		final AOSigner signer = new AOCAdESTriPhaseSigner();

		final Properties config = new Properties(this.serverConfig);
		for (final String key : this.serverConfig.keySet().toArray(new String[this.serverConfig.size()])) {
			config.setProperty(key, this.serverConfig.getProperty(key));
		}

		final byte[] result = signer.sign(null, "SHA512withRSA", this.pke.getPrivateKey(), this.pke.getCertificateChain(), config); //$NON-NLS-1$

		Assert.assertNotNull("Error durante el proceso de firma, resultado nulo", result); //$NON-NLS-1$

		Logger.getLogger("es.gob.afirma").info("El resultado de la firma es: " + new String(result)); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Carga el almac&acute;n de pruebas.
	 * @throws Exception */
	@Before
	public void loadKeystore() throws Exception {

		// Cargamos la referencia a la clave privada
		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		this.pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		// Establecemos la configuracion
		this.serverConfig = new Properties();
		this.serverConfig.setProperty(PROPERTY_NAME_SIGN_SERVER_URL, PROPERTY_VALUE_SIGN_SERVER_URL);
		this.serverConfig.setProperty(PROPERTY_NAME_DOC_ID, PROPERTY_VALUE_DOC_ID);
	}

}
