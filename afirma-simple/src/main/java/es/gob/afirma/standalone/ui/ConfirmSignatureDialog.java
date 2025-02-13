package es.gob.afirma.standalone.ui;

import java.awt.Component;
import java.awt.Dimension;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import es.gob.afirma.standalone.DesktopUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

public class ConfirmSignatureDialog {

	private final JOptionPane optionPane;
	private final JDialog dialog;
	private JCheckBox confirmCb;

	private Boolean result = null;

	public ConfirmSignatureDialog(final Component parent, final int numDocuments) {

		this.optionPane = new JOptionPane();
		this.optionPane.setMessageType(JOptionPane.PLAIN_MESSAGE);
		this.optionPane.setMessage(createPanel(numDocuments));
		this.optionPane.setOptions(new String[] {
				SimpleAfirmaMessages.getString("ConfirmSignatureDialog.6"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("ConfirmSignatureDialog.7") //$NON-NLS-1$
		});
		this.optionPane.setOptionType(JOptionPane.OK_CANCEL_OPTION);

		this.dialog = this.optionPane.createDialog(
				getParentComponent(parent),
				SimpleAfirmaMessages.getString("ConfirmSignatureDialog.0")); //$NON-NLS-1$
		this.dialog.setIconImages(DesktopUtil.getIconImages());

		this.optionPane.addPropertyChangeListener(
			    e -> {
				    final boolean optionPaneIsSource = e.getSource() == ConfirmSignatureDialog.this.optionPane;
				    final boolean valueProperty = e.getPropertyName().equals(JOptionPane.VALUE_PROPERTY);

				    if (optionPaneIsSource && valueProperty &&
				    		SimpleAfirmaMessages.getString("ConfirmSignatureDialog.6").equals(e.getNewValue())) { //$NON-NLS-1$
				    	this.result = Boolean.valueOf(this.confirmCb.isSelected());
				    }
				});
	}

	private Component createPanel(final int numDocuments) {

		final JPanel panel = new JPanel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

		// Indicacion de documentos a firmar
		if (numDocuments == 1) {
			panel.add(new JLabel(SimpleAfirmaMessages.getString("ConfirmSignatureDialog.1"))); //$NON-NLS-1$
		}
		else {
			panel.add(new JLabel(SimpleAfirmaMessages.getString(
					"ConfirmSignatureDialog.2", Integer.toString(numDocuments)))); //$NON-NLS-1$
		}

		// Intrucciones
		panel.add(Box.createRigidArea(new Dimension(0, 8)));
		panel.add(new JLabel(SimpleAfirmaMessages.getString("ConfirmSignatureDialog.4"))); //$NON-NLS-1$

		// No volver a mostrar
		panel.add(Box.createRigidArea(new Dimension(0, 12)));
		this.confirmCb = new JCheckBox(SimpleAfirmaMessages.getString("ConfirmSignatureDialog.5")); //$NON-NLS-1$
		panel.add(this.confirmCb);


		return panel;
	}

	/**
	 * Obtiene el componente padre de m&aacute;ximo nivel de un componente.
	 * @param component Componente del que se desea obtener el padre.
	 * @return Padre &uacute;ltimo del componente o &eacute;l mismo si no tiene padre.
	 */
	private static Component getParentComponent(final Component component) {

		Component parent = component;
		while (parent != null && parent.getParent() != null && parent != parent.getParent()) {
			parent = parent.getParent();
		}
		return parent;
	}

	/**
	 * Muestra y oculta el di&aacute;logo.
	 * @param visible {@code true} para mostrar el di&aacute;logo,
	 * {@code false} para ocultarlo.
	 */
	public void setVisible(final boolean visible) {
		this.dialog.setVisible(visible);
	}

	/**
	 * Devuelve la elecci&oacute;n realizada por el usuario.
	 * @return {@code True} si se quiere firmar y que se recuerde esta respuesta,
	 * {@code False} si se quiere firmar y que no se recurde (que se pregunte cada vez)
	 * y {@code null} si no se desea firmar.
	 */
	public Boolean getResult() {
		return this.result;
	}
}
