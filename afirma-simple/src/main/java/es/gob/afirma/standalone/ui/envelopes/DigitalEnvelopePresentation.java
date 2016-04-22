package es.gob.afirma.standalone.ui.envelopes;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/**
 * Di&aacute;logo con el panel inicial del asistente de generaci&oacute;n de sobres digitales.
 * @author Juliana Marulanda.
 */
public class DigitalEnvelopePresentation extends JDialog implements KeyListener{

	private static final long serialVersionUID = -7464675019212378413L;
	private static final int PREFERRED_WIDTH = 650;
	private static final int PREFERRED_HEIGHT = 550;

	private final JButton nextButton = new JButton(SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.3")); //$NON-NLS-1$
	private final JButton cancelButton = new JButton(SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.4")); //$NON-NLS-1$

	private final JPanel panelCentral = new JPanel();
	JPanel getPanelCentral() {
		return this.panelCentral;
	}

	private final JPanel panel = new JPanel();
	JPanel getPanel() {
		return this.panel;
	}

	private JPanel filePanel = new JPanel();
	JPanel getFilePanel() {
		return this.filePanel;
	}
	void setFilePanel(final JPanel p) {
		this.filePanel = p;
	}

	private JPanel recipientsPanel = new JPanel();
	JPanel getRecipientsPanel() {
		return this.recipientsPanel;
	}
	void setRecipientsPanel(final JPanel p) {
		this.recipientsPanel = p;
	}

	private JPanel sendersPanel = new JPanel();
	JPanel getSendersPanel() {
		return this.sendersPanel;
	}
	void setSendersPanel(final JPanel p) {
		this.sendersPanel = p;
	}

	/**
	 * Crea el di&aacute;logo y lo hace visible.
	 * @param parent  Frame padre del di&aacute;logo.
	 */
	public static void startDigitalEnvelopePresentation(final Frame parent) {
		final DigitalEnvelopePresentation de = new DigitalEnvelopePresentation(parent);
		de.setSize(PREFERRED_WIDTH, PREFERRED_HEIGHT);
		de.setResizable(false);
		de.setLocationRelativeTo(parent);
		de.setVisible(true);
	}

	/** Crea el panel de creaaci&oacute;n de un sobre digital.
	 * @param parent Frame padre del di&aacute;logo de la presentaci&oacute;n. */
	public DigitalEnvelopePresentation(final Frame parent) {
		super(parent);
		createUI();
	}

	/**
	 * Crea una ventana con un mensaje de bienvenida.
	 */
	public void createUI() {

		setTitle(SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.0")); //$NON-NLS-1$

		getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.2") //$NON-NLS-1$
		);

		// Icono de la ventana
		setIconImage(AutoFirmaUtil.getDefaultDialogsIcon());

		// Imagen lateral
        final ImagenLateral panelIzdo = new ImagenLateral();
        getContentPane().add(panelIzdo, BorderLayout.WEST);

        // Panel con el contenido
        final GridBagLayout gbLayout = new GridBagLayout();
        this.panelCentral.setBackground(Color.WHITE);
        this.panelCentral.setLayout(gbLayout);
        this.panelCentral.setBorder(BorderFactory.createEmptyBorder());

        // Label con la informacion de bienvenida
        final JLabel presentationLabel = new JLabel(
    		SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.1"), //$NON-NLS-1$
    		SwingConstants.CENTER
        );
        presentationLabel.setFont(new java.awt.Font ("Century Schoolbook L", 0, 13)); //$NON-NLS-1$

        // Boton de siguiente
 		this.nextButton.setMnemonic('S');
 		this.nextButton.getAccessibleContext().setAccessibleDescription(
 			SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.6") //$NON-NLS-1$
		);
 		this.nextButton.addActionListener(
 			new ActionListener() {
 				/** {@inheritDoc} */
 				@Override
 				public void actionPerformed(final ActionEvent ae) {
 					remove(getPanelCentral());
 					remove(getPanel());
 					setFilePanel(new DigitalEnvelopeSelectFile(DigitalEnvelopePresentation.this));
 					add(getFilePanel());
 				}
 			}
 		);
 		this.nextButton.addKeyListener(this);

 		// Boton cancelar
		this.cancelButton.setMnemonic('C');
		this.cancelButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("DigitalEnvelopePresentation.7") //$NON-NLS-1$
		);
		this.cancelButton.addActionListener(
			new ActionListener() {
				/** {@inheritDoc} */
				@Override
				public void actionPerformed(final ActionEvent ae) {
					setVisible(false);
					dispose();
				}
			}
		);
		this.cancelButton.addKeyListener(this);

		this.panel.setLayout(new FlowLayout(FlowLayout.RIGHT));

		// En Mac OS X el orden de los botones es distinto
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			this.panel.add(this.cancelButton);
			this.panel.add(this.nextButton);
		}
		else {
			this.panel.add(this.nextButton);
			this.panel.add(this.cancelButton);
		}

		final JPanel emptyPanel = new JPanel();
		emptyPanel.setBackground(Color.WHITE);

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.insets = new Insets(20, 20, 0, 20);
        c.weightx = 1.0;
        c.weighty = 1.0;
        c.gridx = 0;
        c.gridy = 0;
        c.anchor = GridBagConstraints.NORTHWEST;
        this.panelCentral.add(presentationLabel, c);
		c.gridy++;
		this.panelCentral.add(emptyPanel, c);
        getContentPane().add(this.panelCentral);
        getContentPane().add(this.panel, BorderLayout.PAGE_END);
	}

	/** {@inheritDoc} */
	@Override
	public void keyTyped(final KeyEvent e) { /* Vacio */ }

	/** {@inheritDoc} */
	@Override
	public void keyPressed(final KeyEvent e) { /* Vacio */ }

	/** {@inheritDoc} */
	@Override
	public void keyReleased(final KeyEvent ke) {
		// En Mac no cerramos los dialogos con Escape
		if (ke != null && ke.getKeyCode() == KeyEvent.VK_ESCAPE) {
			setVisible(false);
			dispose();
		}
	}
}
