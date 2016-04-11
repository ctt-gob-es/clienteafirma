package es.gob.afirma.standalone.ui.envelopes;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;

import es.gob.afirma.standalone.SimpleAfirmaMessages;

public final class DigitalEnvelopeEnd extends JDialog {

	private static final long serialVersionUID = 4673856930142046569L;
	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
	final JDialog dialog;
	JDialog getDialog() {
		return this.dialog;
	}
	static BufferedImage icon;
	final JPanel panel = new JPanel();
	final JButton endButton = new JButton(SimpleAfirmaMessages.getString("DigitalEnvelopeEnd.11") ); //$NON-NLS-1$

	/** Crea el di&aacute;logo de finalizaci&oacute;n de los sobres digitales.
	 * @param parent Componente padre para la modalidad. */
	public DigitalEnvelopeEnd(final JDialog parent) {
		this.dialog = parent;
		createUI();
	}

	void createUI() {

        final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.gridwidth = GridBagConstraints.REMAINDER;
		c.insets = new Insets(20, 10, 0, 20);
		c.weightx = 1.0;
        c.gridx = 0;
        c.gridy = 0;

        // Panel con el contenido
        final JPanel panelCentral = new JPanel();
        final GridBagLayout gbLayout = new GridBagLayout();
        panelCentral.setBackground(Color.WHITE);
        panelCentral.setLayout(gbLayout);
        panelCentral.setBorder(BorderFactory.createEmptyBorder());
        panelCentral.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("DigitalEnvelopeSender.0") //$NON-NLS-1$
    	);

     // Etiqueta con el texto "Felicidades!..."

        final JLabel labelsb1 = new JLabel(
    		SimpleAfirmaMessages.getString("DigitalEnvelopeEnd.0"), //$NON-NLS-1$
    		SwingConstants.CENTER
        );
        labelsb1.setFont(new java.awt.Font ("Century Schoolbook L", 0, 13)); //$NON-NLS-1$

        // Imagen del dni electronico
        final JLabel labelDNIe = new JLabel();
        labelDNIe.setIcon(new javax.swing.ImageIcon(getClass().getResource("/resources/dnie.png"))); //$NON-NLS-1$

        // Etiqueta con el texto "Recuerde..."
        final JLabel label = new JLabel(
    		SimpleAfirmaMessages.getString("DigitalEnvelopeEnd.10"), //$NON-NLS-1$
    		SwingConstants.CENTER);
        label.setFont(new java.awt.Font ("Century Schoolbook L", 0, 12)); //$NON-NLS-1$

     // Boton cancelar
 		this.endButton.setMnemonic('C');
 		this.endButton.getAccessibleContext().setAccessibleDescription(
 			SimpleAfirmaMessages.getString("DigitalEnvelopeEnd.2") //$NON-NLS-1$
		);
 		this.endButton.addActionListener(
 			new ActionListener() {
	 			/** {@inheritDoc} */
	 			@Override
	 			public void actionPerformed(final ActionEvent ae) {
	 				getDialog().setVisible(false);
					getDialog().dispose();
 				}
 			}
 		);
 		this.endButton.setEnabled(true);

 		this.panel.setLayout(new FlowLayout(FlowLayout.RIGHT));
		this.panel.add(this.endButton);

		final JPanel emptyPanel = new JPanel();
		emptyPanel.setBackground(Color.WHITE);

		c.gridwidth = GridBagConstraints.REMAINDER;
        panelCentral.add(labelsb1, c);
        c.gridwidth = GridBagConstraints.RELATIVE;
        c.insets = new Insets(20, 0, 0, 20);
        c.weightx = 0.0;
		c.gridy++;
		panelCentral.add(labelDNIe, c);
		c.gridx = 1;
		panelCentral.add(label, c);
		c.weighty = 1.0;
		c.gridy++;
		panelCentral.add(emptyPanel, c);

		this.dialog.getContentPane().add(panelCentral);
		this.dialog.getContentPane().add(this.panel, BorderLayout.PAGE_END);
		this.dialog.revalidate();
        this.dialog.repaint();
	}

	/**
	 * @return the icon
	 */
	public static BufferedImage getIcon() {
		return icon;
	}

	/**
	 * @param iconBI the icon to set
	 */
	public static void setIcon(final BufferedImage iconBI) {
		icon = iconBI;
	}
}
