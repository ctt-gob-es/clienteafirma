package es.gob.afirma.ui.core.jse;


/**
 * Di&aacute;logo de selecci&oacute;n de certificados con est&eacute;tica similar al de
 * Windows 7.
 * @author Carlos Gamuci
 */
public class CertificateSelectionDialogTest {


    private static final String CERT_PATH = "multi_almacen.p12"; //$NON-NLS-1$
    private static final String CERT_PASS = "1111"; //$NON-NLS-1$

//	/** Para pruebas sin JUnit
//	 * @param args
//	 * @throws Exception */
//	public static void main(final String[] args) throws Exception {
//
//		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
//		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
//
//		final java.util.Enumeration<String> al = ks.aliases();
//
//		final ArrayList<NameCertificateBean> elements = new ArrayList<NameCertificateBean>();
//		while (al.hasMoreElements()) {
//			final String alias = al.nextElement();
//			elements.add(new NameCertificateBean(alias, alias, (X509Certificate) ks.getCertificate(alias)));
//		}
//
//		final CertificateSelectionDialog dialog = new CertificateSelectionDialog(null, 
//				elements.toArray(new NameCertificateBean[elements.size()]), null);
//
//		final String selectedAlias = dialog.showDialog();
//
//		System.out.println("Certificado selecionado: " + selectedAlias); //$NON-NLS-1$
//	}
    
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
