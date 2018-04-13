package es.gob.afirma.ui.core.jse;

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

	/** Para pruebas sin JUnit
	 * @param args No se usa.
	 * @throws Exception En cualquier error. */
	public static void main(final String[] args) throws Exception {

		final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
				AOKeyStore.PKCS12,
				ClassLoader.getSystemResource(CERT_PATH).toString().replace("file:/", ""), //$NON-NLS-1$ //$NON-NLS-2$
				null,
				new CachePasswordCallback(CERT_PASS.toCharArray()),
				null);

//		final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
//				AOKeyStore.WINDOWS,
//				null,
//				null,
//				null,
//				null);

		final AOKeyStoreDialog dialog = new AOKeyStoreDialog(ksm, null, true, false, false);
		String alias;
		try {
			alias = dialog.show();
		}
		catch (final Exception e) {
			System.err.println("Error: " + e);
			e.printStackTrace();
			return;
		}

		System.out.println("Certificado:\n" + ksm.getCertificate(alias)); //$NON-NLS-1$
	}

//	/** Para pruebas sin JUnit
//	 * @param args
//	 * @throws Exception */
//	public static void main(final String[] args) throws Exception {
//
//		final AOKeyStoreManager ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
//				AOKeyStore.WINDOWS,
//				null,
//				null,
//				AOKeyStore.WINDOWS.getStorePasswordCallback(null),
//				null);
//
//		List<CertificateFilter> filters = new ArrayList<CertificateFilter>();
//		filters.add(new RFC2254CertificateFilter("(CN=NOMBRE*)", "(OU=FNMT*)"));
//
//		AOKeyStoreDialog dialog = new AOKeyStoreDialog(ksm, null, false, false, false, filters, true);
//
//
//		System.out.println(AOUtil.getCN((X509Certificate) dialog.show().getCertificate()));
//	}
}
