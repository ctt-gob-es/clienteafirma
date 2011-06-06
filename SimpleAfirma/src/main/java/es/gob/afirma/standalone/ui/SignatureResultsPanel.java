package es.gob.afirma.standalone.ui;

import java.security.cert.X509Certificate;

import javax.swing.JPanel;
import javax.swing.SwingUtilities;

/**
 * Panel para mostrar un informe resumen sobre el resultado de una firma electr&oacute;nica. 
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class SignatureResultsPanel extends JPanel {

	private static final long serialVersionUID = -1581326925023691437L;
	
	/**
	 * Crea un panel para mostrar un informe resumen sobre el resultado de una firma electr&oacute;nica.
	 * @param sign Firma o fichero firmado sobre el que queremos mostrar un resumen
	 * @param fileName Nombre del fichero en el que se ha guardado la firma o el fichero firmado
	 * @param signingCert Certificado usado para la firma
	 */
	public SignatureResultsPanel(final byte[] sign, final String fileName, final X509Certificate signingCert) {
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				createUI(sign, fileName, signingCert);
			}
		});
	}
	
	private void createUI(final byte[] sign, final String fileName, final X509Certificate signingCert) {
		
	}
	
}
