package es.gob.afirma.ui.core.jse;

import java.awt.Color;
import java.awt.Component;

import javax.swing.JDialog;
import javax.swing.JOptionPane;

import es.gob.afirma.core.ui.NameCertificateBean;

/**
 * Di&aacute;logo de selecci&oacute;n de certificados con est&eacute;tica similar al de
 * Windows 7.
 * @author Carlos Gamuci
 */
public class CertificateSelectionDialog {

	private final JOptionPane optionPane;
	
	private final Component parent;
	
	/**
	 * Construye el di&aacute;logo de selecci&oacute;n de certificados a partir del listado con
	 * sus nombres y los propios certificados. 
	 * @param el Listado de certificados.
	 * @param parent Componente sobre el que se mostrar&aacute; el di&aacute;logo.
	 */
	public CertificateSelectionDialog(final NameCertificateBean[] el, final Component parent) {

		this.parent = parent;
		this.optionPane = new JOptionPane();
		CertificateSelectionPanel csd = new CertificateSelectionPanel(el, this.optionPane);
		this.optionPane.setMessage(csd);
		this.optionPane.setMessageType(JOptionPane.PLAIN_MESSAGE);
		this.optionPane.setOptionType(JOptionPane.OK_CANCEL_OPTION);
	}
	
	/**
	 * Muestra el di&aacute;logo de selecci&oacute;n de certificados.
	 * @return Alias del certificado seleccionado o {@code null} si el usuario
	 * lo cancela o cierra sin seleccionar.
	 */
	public String showDialog() {
		
		JDialog certDialog = this.optionPane.createDialog(this.parent, JSEUIMessages.getString("CertificateSelectionDialog.0")); //$NON-NLS-1$
		certDialog.setBackground(Color.WHITE);
		certDialog.setModal(true);
		certDialog.setVisible(true);
		
		if (this.optionPane.getValue() == null ||
				((Integer) this.optionPane.getValue()).intValue() != JOptionPane.OK_OPTION) {
			certDialog.dispose();
			return null;
		}
		String selectedAlias = (String) this.optionPane.getInputValue();
		certDialog.dispose();
		return selectedAlias;
	}
}
