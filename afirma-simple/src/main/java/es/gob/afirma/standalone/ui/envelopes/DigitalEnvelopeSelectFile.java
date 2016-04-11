package es.gob.afirma.standalone.ui.envelopes;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.logging.Logger;

import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/**
 * @author Mariano Mart&iacute;nez
 *
 * Panel para seleccionar un fichero para ensobrar y el tipo de sobre a realizar.
 */
public class DigitalEnvelopeSelectFile extends JPanel {

	private static final long serialVersionUID = -5430415718507253691L;
	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final JComboBox<EnvelopesTypeResources> envelopeTypes = new JComboBox<>(
		EnvelopesTypeResources.getAllEnvelopesTypeResources()
	);
	EnvelopesTypeResources getSelectedType() {
		return (EnvelopesTypeResources) this.envelopeTypes.getSelectedItem();
	}

	private final JTextField selectedFile = new JTextField();
	void setSelectedFile(final String file) {
		this.selectedFile.setText(file);
	}
	String getSelectedFile() {
		return this.selectedFile.getText();
	}

	private final JButton nextButton = new JButton(SimpleAfirmaMessages.getString("MenuDigitalEnvelope.21")); //$NON-NLS-1$
	private final JButton cancelButton = new JButton(SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.15")); //$NON-NLS-1$
	private final JButton backButton = new JButton(SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.16") ); //$NON-NLS-1$
	private final JButton examineButton = new JButton(SimpleAfirmaMessages.getString("MenuDigitalEnvelope.20")); //$NON-NLS-1$

	JButton getNextButton() {
		return this.nextButton;
	}

	private final JDialog dialog;
	JDialog getDialog() {
		return this.dialog;
	}

	private final JPanel panelCentral = new JPanel();
	JPanel getPanelCentral() {
		return this.panelCentral;
	}

	private final JPanel panel = new JPanel();
	JPanel getPanel() {
		return this.panel;
	}

	DigitalEnvelopeSelectFile(final JDialog dl) {
		this.dialog = dl;
		createUI();
	}

	/** Crea una ventana con opciones de ensobrado. */
	void createUI() {

		// Panel con el contenido
        final GridBagLayout gbLayout = new GridBagLayout();
        this.panelCentral.setBackground(Color.WHITE);
        this.panelCentral.setLayout(gbLayout);
        this.panelCentral.setBorder(BorderFactory.createEmptyBorder());
        this.panelCentral.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("MenuDigitalEnvelope.23") //$NON-NLS-1$
    	);

    	final JLabel infoLabel = new JLabel(
			SimpleAfirmaMessages.getString("MenuDigitalEnvelope.27") //$NON-NLS-1$
		);

		// Eleccion fichero a ensobrar
		final JLabel envelopeFilesLabel = new JLabel(
			SimpleAfirmaMessages.getString("MenuDigitalEnvelope.14") //$NON-NLS-1$
		);
		envelopeFilesLabel.setLabelFor(this.selectedFile);
		this.selectedFile.setEditable(false);
		this.selectedFile.setFocusable(false);

		// Boton de examinar
		this.examineButton.setMnemonic('X');
		this.examineButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("MenuDigitalEnvelope.20") //$NON-NLS-1$
		);
		this.examineButton.addActionListener(
			new ActionListener() {
				/** {@inheritDoc} */
				@Override
				public void actionPerformed(final ActionEvent ae) {
					Image certificateIcon = null;
					try {
						certificateIcon = ImageIO.read(
							MenuDigitalEnvelope.class.getResource("/resources/certificate_16.png") //$NON-NLS-1$
						);
					}
					catch (final IOException e) {
						LOGGER.warning(
							"No ha podido cargarse el icono del dialogo: " + e //$NON-NLS-1$
						);
					}
					final File file;
					try {
						file = AOUIFactory.getLoadFiles(
							SimpleAfirmaMessages.getString("MenuDigitalEnvelope.14"), //$NON-NLS-1$
							null,
							null,
							new String[] { },
							SimpleAfirmaMessages.getString("MenuDigitalEnvelope.22"), //$NON-NLS-1$
							false,
							false,
							certificateIcon,
							getDialog()
						)[0];
					}
					catch (final AOCancelledOperationException e) {
						LOGGER.warning(
							"Operacion cancelada por el usuario: " + e//$NON-NLS-1$
						);
						return;
					}
					if (!file.canRead()) {
						LOGGER.warning(
							"No ha podido cargarse el fichero para envolver: " //$NON-NLS-1$
						);
						AOUIFactory.showErrorMessage(
							getDialog(),
							SimpleAfirmaMessages.getString("MenuValidation.6"), //$NON-NLS-1$
							SimpleAfirmaMessages.getString("MenuValidation.5"), //$NON-NLS-1$
							JOptionPane.ERROR_MESSAGE
						);
						return;
					}
					setSelectedFile(file.getAbsolutePath());
					getNextButton().setEnabled(true);
				}
			}
		);
		this.examineButton.setEnabled(true);

		// Label con los tipos de ensobrado
		final JLabel typeFilesLabel = new JLabel(
			SimpleAfirmaMessages.getString("MenuDigitalEnvelope.15") //$NON-NLS-1$
		);
		typeFilesLabel.setLabelFor(this.envelopeTypes);

		// Botono de siguiente
		this.nextButton.setMnemonic('S');
		this.nextButton.getAccessibleContext().setAccessibleDescription(
 			SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.17") //$NON-NLS-1$
		);
		this.nextButton.addActionListener(
			new ActionListener() {
			/** {@inheritDoc} */
			@Override
			public void actionPerformed(final ActionEvent ae) {
				getDialog().remove(getPanelCentral());
				getDialog().remove(getPanel());
				new DigitalEnvelopeRecipients(getDialog(), getSelectedFile(), getSelectedType());
			}
		}
		);
		this.nextButton.setEnabled(false);

		// Boton cancelar
		this.cancelButton.setMnemonic('C');
		this.cancelButton.getAccessibleContext().setAccessibleDescription(
 			SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.18") //$NON-NLS-1$
		);
		this.cancelButton.addActionListener(
			new ActionListener() {
				/** {@inheritDoc} */
				@Override
				public void actionPerformed(final ActionEvent ae) {
					getDialog().setVisible(false);
					getDialog().dispose();
				}
			}
		);

		// Boton de volver
		this.backButton.setMnemonic('A');
		this.backButton.getAccessibleContext().setAccessibleDescription(
 			SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.19") //$NON-NLS-1$
		);
		this.backButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				getDialog().remove(getPanelCentral());
				getDialog().remove(getPanel());
				DigitalEnvelopePresentation.startDigitalEnvelopePresentation(
					(Frame)  getDialog().getParent()
				);
				getDialog().setVisible(false);
				getDialog().dispose();
			}
		});
		this.cancelButton.setEnabled(true);
		this.backButton.setEnabled(true);

		this.panel.setLayout(new FlowLayout(FlowLayout.RIGHT));

		// En Mac OS X el orden de los botones es distinto
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			this.panel.add(this.cancelButton);
			this.panel.add(this.backButton);
			this.panel.add(this.nextButton);
		}
		else {
			this.panel.add(this.backButton);
			this.panel.add(this.nextButton);
			this.panel.add(this.cancelButton);
		}

		final JPanel emptyPanel = new JPanel();
		emptyPanel.setBackground(Color.WHITE);

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 1.0;
		c.insets = new Insets(20, 20, 20, 20);
		this.panelCentral.add(infoLabel, c);
		c.insets = new Insets(20, 20, 0, 20);
		c.gridy++;
		c.gridy++;
		this.panelCentral.add(envelopeFilesLabel, c);
		c.insets = new Insets(5, 20, 0, 10);
		c.gridy++;
		c.gridy++;
		this.panelCentral.add(this.selectedFile, c);
		c.gridwidth = GridBagConstraints.REMAINDER;
		c.weightx = 0.0;
		this.panelCentral.add(this.examineButton, c);
		c.insets = new Insets(20, 20, 0, 10);
		c.gridy++;
		c.weightx = 1.0;
		this.panelCentral.add(typeFilesLabel, c);
		c.insets = new Insets(5, 20, 0, 10);
		c.gridy++;
		c.fill = GridBagConstraints.NONE;
		c.anchor = GridBagConstraints.LINE_START;
		this.panelCentral.add(this.envelopeTypes, c);
		c.weighty = 1.0;
		c.gridy++;
		this.panelCentral.add(emptyPanel, c);
		this.dialog.getContentPane().add(this.panelCentral);
		this.dialog.getContentPane().add(this.panel, BorderLayout.PAGE_END);
        this.dialog.revalidate();
        this.dialog.repaint();
	}
}

