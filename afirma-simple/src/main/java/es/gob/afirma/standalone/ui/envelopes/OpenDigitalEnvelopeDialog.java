package es.gob.afirma.standalone.ui.envelopes;

import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.logging.Logger;

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
import es.gob.afirma.keystores.KeyStoreConfiguration;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

public class OpenDigitalEnvelopeDialog extends JDialog {

	private static final long serialVersionUID = -5949140119173965513L;
	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
	private static final int PREFERRED_WIDTH = 650;
	private static final int PREFERRED_HEIGHT = 400;

	private final JTextField selectedFilePath = new JTextField();
	void setSelectedFilePath(final String path) {
		this.selectedFilePath.setText(path);
	}

	private final JTextField selectedCertPath = new JTextField();
	void setSelectedCertPath(final String path) {
		this.selectedCertPath.setText(path);
	}

	private final JComboBox<KeyStoreConfiguration> comboBox = new JComboBox<>();
	JComboBox<KeyStoreConfiguration> getComboBox() {
		return this.comboBox;
	}

	private final JButton examineFileButton = new JButton(SimpleAfirmaMessages.getString("OpenDigitalEnvelope.2")); //$NON-NLS-1$
	private final JButton examineCertButton = new JButton(SimpleAfirmaMessages.getString("OpenDigitalEnvelope.2")); //$NON-NLS-1$
	private final JButton openButton = new JButton(SimpleAfirmaMessages.getString("OpenDigitalEnvelope.3")); //$NON-NLS-1$
	private final JButton cancelButton = new JButton(SimpleAfirmaMessages.getString("OpenDigitalEnvelope.4")); //$NON-NLS-1$

	public static void startOpenDigitalEnvelopeDialog(final Frame parent) {

		final OpenDigitalEnvelopeDialog ode = new OpenDigitalEnvelopeDialog(parent);
		ode.setSize(PREFERRED_WIDTH, PREFERRED_HEIGHT);
		ode.setResizable(false);
		ode.setLocationRelativeTo(parent);
		ode.setVisible(true);
	}



	/** Crea el panel de apertura de un sobre digital. */
	public OpenDigitalEnvelopeDialog(final Frame parent) {
		super(parent);
		createUI();
	}

	public void createUI() {

		setTitle(SimpleAfirmaMessages.getString("OpenDigitalEnvelope.0")); //$NON-NLS-1$

		getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("OpenDigitalEnvelope.2") //$NON-NLS-1$
		);

		// Icono de la ventana
		setIconImage(AutoFirmaUtil.getDefaultDialogsIcon());

		// Eleccion fichero a desensobrar
		final JLabel envelopeFilesLabel = new JLabel(
			SimpleAfirmaMessages.getString("OpenDigitalEnvelope.5") //$NON-NLS-1$
		);
		envelopeFilesLabel.setLabelFor(this.selectedFilePath);
		this.selectedFilePath.setEditable(false);
		this.selectedFilePath.setFocusable(false);

		// Eleccion certificado para desensobrar
		final JLabel envelopeCertLabel = new JLabel(
			SimpleAfirmaMessages.getString("OpenDigitalEnvelope.6") //$NON-NLS-1$
		);
		envelopeCertLabel.setLabelFor(this.selectedCertPath);
		this.selectedCertPath.setEditable(false);
		this.selectedCertPath.setFocusable(false);

		// Boton de examinar
		this.examineFileButton.setMnemonic('X');
		this.examineFileButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("OpenDigitalEnvelope.7") //$NON-NLS-1$
		);
		this.examineFileButton.addActionListener(
			new ActionListener() {
				/** {@inheritDoc} */
				@Override
				public void actionPerformed(final ActionEvent ae) {
					final File envFile;
					try {
						envFile = AOUIFactory.getLoadFiles(
							SimpleAfirmaMessages.getString(""),
							null,
							null,
							new String[] { },
							SimpleAfirmaMessages.getString(""),
							false,
							false,
							null,
							OpenDigitalEnvelopeDialog.this
						)[0];
					}
					catch (final AOCancelledOperationException e) {
						LOGGER.warning(
							"Operacion cancelada por el usuario: " + e //$NON-NLS-1$
						);
						return;
					}
					if (!envFile.canRead()) {
						LOGGER.warning(
							"No ha podido cargarse el fichero para envolver: "
						);
						AOUIFactory.showErrorMessage(
							OpenDigitalEnvelopeDialog.this,
							SimpleAfirmaMessages.getString(""),
							SimpleAfirmaMessages.getString(""),
							JOptionPane.ERROR_MESSAGE
						);
						return;
					}
					setSelectedFilePath(envFile.getAbsolutePath());
					enableOpenbutton();
				}
			}
		);
		this.examineFileButton.setEnabled(true);

		// Boton de examinar
		this.examineCertButton.setMnemonic('E');
		this.examineCertButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("OpenDigitalEnvelope.8") //$NON-NLS-1$
		);
		this.examineCertButton.addActionListener(
			new ActionListener() {
				/** {@inheritDoc} */
				@Override
				public void actionPerformed(final ActionEvent ae) {
					final File fileCert;
					try {
						fileCert = AOUIFactory.getLoadFiles(
							SimpleAfirmaMessages.getString(""),
							null,
							null,
							new String[] { },
							SimpleAfirmaMessages.getString(""),
							false,
							false,
							null,
							OpenDigitalEnvelopeDialog.this
						)[0];
					}
					catch (final AOCancelledOperationException e) {
						LOGGER.warning(
							"Operacion cancelada por el usuario: " + e//$NON-NLS-1$
						);
						return;
					}
					if (!fileCert.canRead()) {
						LOGGER.warning(
							"No ha podido cargarse el fichero para envolver: "
						);
						AOUIFactory.showErrorMessage(
							OpenDigitalEnvelopeDialog.this,
							SimpleAfirmaMessages.getString(""),
							SimpleAfirmaMessages.getString(""),
							JOptionPane.ERROR_MESSAGE
						);
						return;
					}
					setSelectedCertPath(fileCert.getAbsolutePath());
					enableOpenbutton();
				}

			}
		);
		this.examineCertButton.setEnabled(true);

		// Boton de examinar
		this.openButton.setMnemonic('A');
		this.openButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("OpenDigitalEnvelope.9") //$NON-NLS-1$
		);
		this.openButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent arg0) {
					open();
				}
			}
		);

		this.cancelButton.setMnemonic('C');
		this.cancelButton.getAccessibleContext().setAccessibleDescription(
 			SimpleAfirmaMessages.getString("OpenDigitalEnvelope.10") //$NON-NLS-1$
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

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.RIGHT));

		// En Mac OS X el orden de los botones es distinto
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			panel.add(this.cancelButton);
			panel.add(this.openButton);
		}
		else {
			panel.add(this.openButton);
			panel.add(this.cancelButton);
		}

		setLayout(new GridBagLayout());
		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.insets = new Insets(20, 20, 0, 20);
        c.weightx = 1.0;
        add(envelopeFilesLabel, c);
        c.gridy++;
        c.gridy++;
        add(this.selectedFilePath, c);
        add(this.examineFileButton, c);
        c.gridy++;
        add(envelopeCertLabel, c);
        c.gridy++;
        add(this.selectedCertPath, c);
        add(this.examineCertButton, c);
        c.gridy++;
        add(panel, c);
		enableOpenbutton();
	}

	void enableOpenbutton() {
		if (!this.selectedCertPath.getText().trim().isEmpty()
				&& !this.selectedFilePath.getText().trim().isEmpty()) {
			this.openButton.setEnabled(true);
		}
		else {
			this.openButton.setEnabled(false);
		}
	}

	void open() {
		/*
		byte[] data = null;
    	AOKeyStoreManager keyStoreManager;
		try {
			keyStoreManager = FileBasedKeyStoreManagerFactory.getKeyStoreManager(
				new File(this.selectedCertPath.getText()),
				this
			);

	        final CertificateDestiny certDest = new CertificateDestiny(keyStoreManager, this);
	        final AOCMSEnveloper enveloper = new AOCMSEnveloper();
	        data = enveloper.recoverData(
	    		EnvelopesUtils.readFile(this.selectedFilePath.getText()),
	    		keyStoreManager.getKeyEntry(certDest.getAlias())
	        );
		} catch (final Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

       	File savedFile;
		try {
			savedFile = AOUIFactory.getSaveDataToFile(
			    data,
			    SimpleAfirmaMessages.getString("DigitalEnvelopeSender.32"), //$NON-NLS-1$
			    null,
			    new File(this.selectedFilePath.getText()).getName().split(".enveloped")[0], //$NON-NLS-1$
			    null,
			    null,
			    this
			);
		} catch (final IOException e) {
			LOGGER.severe("No se ha posido guardar el sobre: " + e); //$NON-NLS-1$
			AOUIFactory.showMessageDialog(
        		this,
        		SimpleAfirmaMessages.getString("DigitalEnvelopeSender.33"), //$NON-NLS-1$
        		SimpleAfirmaMessages.getString("DigitalEnvelopeSender.31"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
		}*/
	}
}