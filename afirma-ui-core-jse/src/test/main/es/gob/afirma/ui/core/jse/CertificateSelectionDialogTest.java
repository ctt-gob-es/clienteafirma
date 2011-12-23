package es.gob.afirma.ui.core.jse;

import java.awt.Color;
import java.awt.Component;
import java.security.KeyStore;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Hashtable;

import javax.swing.JDialog;
import javax.swing.JOptionPane;

import es.gob.afirma.core.ui.NameCertificateBean;

/**
 * Di&aacute;logo de selecci&oacute;n de certificados con est&eacute;tica similar al de
 * Windows 7.
 * @author Carlos Gamuci
 */
public class CertificateSelectionDialogTest {

	
    private static final String CERT_PATH = "multi_almacen.p12"; //$NON-NLS-1$
    private static final String CERT_PASS = "1111"; //$NON-NLS-1$
    
	public static void main(String[] args) throws Exception {

		KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
		
		java.util.Enumeration<String> al = ks.aliases();
		
		ArrayList<NameCertificateBean> elements = new ArrayList<NameCertificateBean>();
		while (al.hasMoreElements()) {
			String alias = al.nextElement();
			elements.add(new NameCertificateBean(alias, (X509Certificate) ks.getCertificate(alias)));
		}
		
		CertificateSelectionDialog dialog = new CertificateSelectionDialog(
				elements.toArray(new NameCertificateBean[0]), null);
		
		String selectedAlias = dialog.showDialog();
		
		System.out.println("Certificado selecionado: " + selectedAlias);
	}
}
