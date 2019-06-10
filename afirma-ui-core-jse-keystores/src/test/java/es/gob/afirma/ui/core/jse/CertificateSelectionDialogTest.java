package es.gob.afirma.ui.core.jse;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.callbacks.CachePasswordCallback;


/**
 * Di&aacute;logo de selecci&oacute;n de certificados con est&eacute;tica similar al de
 * Windows 7.
 * @author Carlos Gamuci
 */
public class CertificateSelectionDialogTest {


    private static final String CERT_PATH = "multi_almacen.p12"; //$NON-NLS-1$
    private static final String CERT_PASS = "1111"; //$NON-NLS-1$

	/** Prueba de di&aacute;logo de selecci&oacute;n de certificados.
	 * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
	@Ignore
	public void showCertDialogTest() throws Exception {

		final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
				AOKeyStore.PKCS12,
				ClassLoader.getSystemResource(CERT_PATH).toString().replace("file:/", ""), //$NON-NLS-1$ //$NON-NLS-2$
				null,
				new CachePasswordCallback(CERT_PASS.toCharArray()),
				null);

		final AOKeyStoreDialog dialog = new AOKeyStoreDialog(ksm, null, true, true, false);
		String alias;
		try {
			alias = dialog.show();
		}
		catch (final Exception e) {
			System.err.println("Error: " + e); //$NON-NLS-1$
			e.printStackTrace();
			return;
		}

		System.out.println("Certificado:\n" + ksm.getCertificate(alias)); //$NON-NLS-1$
	}

}
