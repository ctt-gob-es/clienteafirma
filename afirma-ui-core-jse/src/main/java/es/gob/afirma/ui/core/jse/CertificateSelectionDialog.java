package es.gob.afirma.ui.core.jse;

import java.awt.Color;
import java.awt.Component;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JDialog;
import javax.swing.JOptionPane;

import es.gob.afirma.core.ui.NameCertificateBean;

/** Di&aacute;logo de selecci&oacute;n de certificados con est&eacute;tica similar al de
 * Windows 7.
 * @author Carlos Gamuci */
final class CertificateSelectionDialog implements MouseListener {

	private final CertificateSelectionPanel csd;
	
	private final JOptionPane optionPane;
	
	private final Component parent;
	
	/** Construye el di&aacute;logo de selecci&oacute;n de certificados a partir del listado con
	 * sus nombres y los propios certificados. 
	 * @param el Listado de certificados.
	 * @param parent Componente sobre el que se mostrar&aacute; el di&aacute;logo. */
	CertificateSelectionDialog(final NameCertificateBean[] el, final Component parent) {

		this.csd = new CertificateSelectionPanel(el);
		this.parent = parent;
		this.optionPane = (el.length > 1) ?
				new CertOptionPane(this.csd) : new JOptionPane();
		
		this.csd.addCertificateListMouseListener(this);
		
		this.optionPane.setMessage(this.csd);
		this.optionPane.setMessageType(JOptionPane.PLAIN_MESSAGE);
		this.optionPane.setOptionType(JOptionPane.OK_CANCEL_OPTION);

	}
	

	
	/** Muestra el di&aacute;logo de selecci&oacute;n de certificados.
	 * @return Alias del certificado seleccionado o {@code null} si el usuario
	 * lo cancela o cierra sin seleccionar. */
	String showDialog() {
		final JDialog certDialog = this.optionPane.createDialog(this.parent, JSEUIMessages.getString("CertificateSelectionDialog.0")); //$NON-NLS-1$
		certDialog.setBackground(Color.WHITE);
		certDialog.setModal(true);
		certDialog.setVisible(true);
		
		if (this.optionPane.getValue() == null ||
				((Integer) this.optionPane.getValue()).intValue() != JOptionPane.OK_OPTION) {
			certDialog.dispose();
			return null;
		}
		final String selectedAlias = this.csd.getSelectedCertificate();
		certDialog.dispose();
		return selectedAlias;
	}

	/** {@inheritDoc} */
	public void mouseClicked(MouseEvent me) {
		if (me.getClickCount() == 2 && this.optionPane != null) {
			this.optionPane.setValue(Integer.valueOf(JOptionPane.OK_OPTION));
		}
	}

	/** {@inheritDoc} */
	public void mouseReleased(MouseEvent me) {
		/* No hacemos nada */
	}
	
	/** {@inheritDoc} */
	public void mousePressed(MouseEvent me) {
		/* No hacemos nada */
	}
	
	/** {@inheritDoc} */
	public void mouseExited(MouseEvent me) {
		/* No hacemos nada */
	}
	
	/** {@inheritDoc} */
	public void mouseEntered(MouseEvent me) {
		/* No hacemos nada */
	}
	
	private static final class CertOptionPane extends JOptionPane {
		
		private static final long serialVersionUID = 1L;

		private final CertificateSelectionPanel selectionPanel;
		
		CertOptionPane(final CertificateSelectionPanel csd) {
			this.selectionPanel = csd;
		}
		
		/** {@inheritDoc} */
		@Override
        public void selectInitialValue() {
			this.selectionPanel.selectCertificateList();
        }		
	}
	
}
