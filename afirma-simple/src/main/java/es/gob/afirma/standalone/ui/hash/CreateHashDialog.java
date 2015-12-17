package es.gob.afirma.standalone.ui.hash;

import java.awt.Frame;
import java.awt.GridBagLayout;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileInputStream;
import java.io.InputStream;
import java.security.MessageDigest;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.PreferencesManager;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/** Di&aacute;logo para la creaci&oacute;n de huellas digitales.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CreateHashDialog extends JDialog {

	private static final long serialVersionUID = 3581001930027153381L;

	private static final String PREFERENCE_BASE64 = "createHashAsBase64"; //$NON-NLS-1$
	private static final String PREFERENCE_ALGORITHM = "createHashAlgorithm"; //$NON-NLS-1$

	private static final String[] HASH_ALGOS = new String[] {
		"SHA-1", //$NON-NLS-1$
		"SHA-256", //$NON-NLS-1$
		"SHA-384", //$NON-NLS-1$
		"SHA-512" //$NON-NLS-1$
	};

	private final JComboBox<String> hashAlgorithms = new JComboBox<>(HASH_ALGOS);
	String getSelectedHashAlgorithm() {
		return this.hashAlgorithms.getSelectedItem().toString();
	}

	private final JTextField fileTextField = new JTextField();
	JTextField getFileTextField() {
		return this.fileTextField;
	}

	private final JCheckBox base64ChechBox = new JCheckBox(
		SimpleAfirmaMessages.getString("CreateHashDialog.0") //$NON-NLS-1$
	);
	boolean isBase64Checked() {
		return this.base64ChechBox.isSelected();
	}

	/** Inicia el proceso de creaci&oacute;n de huella digital.
	 * @param parent Componente padre para la modalidad. */
	public static void startHashCreation(final Frame parent) {
		new CreateHashDialog(parent).setVisible(true);
	}

	/** Crea un di&aacute;logo para la creaci&oacute;n de huellas digitales.
	 * @param parent Componente padre para la modalidad. */
	private CreateHashDialog(final Frame parent) {
		super(parent);
		setTitle("Creacion de huella digital"); //$NON-NLS-1$
		setModalityType(ModalityType.APPLICATION_MODAL);
		SwingUtilities.invokeLater(
			new Runnable() {
				@Override
				public void run() {
					createUI(parent);
				}
			}
		);
	}

	void createUI(final Frame parent) {

		setLayout(new GridBagLayout());
		setSize(300, 300);
		setLocationRelativeTo(parent);
		setIconImage(
			Toolkit.getDefaultToolkit().getImage(this.getClass().getResource("/resources/afirma_ico.png")) //$NON-NLS-1$
		);
		getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("CreateHashDialog.1") //$NON-NLS-1$
		);

		final JLabel hashAlgorithmsLabels = new JLabel(
			SimpleAfirmaMessages.getString("CreateHashDialog.2") //$NON-NLS-1$
		);
		hashAlgorithmsLabels.setLabelFor(this.hashAlgorithms);
		add(hashAlgorithmsLabels);
		this.hashAlgorithms.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent e) {
					PreferencesManager.put(
						PREFERENCE_ALGORITHM,
						getSelectedHashAlgorithm()
					);
				}
			}
		);
		this.hashAlgorithms.setSelectedItem(
			PreferencesManager.get(
				PREFERENCE_ALGORITHM,
				"SHA-512" //$NON-NLS-1$
			)
		);
		add(this.hashAlgorithms);

		this.base64ChechBox.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent e) {
					PreferencesManager.putBoolean(
						PREFERENCE_BASE64,
						isBase64Checked()
					);
				}
			}
		);
		this.base64ChechBox.setSelected(
			PreferencesManager.getBoolean(
				PREFERENCE_BASE64,
				false
			)
		);

		final JLabel fileTextFieldLabel = new JLabel(
			SimpleAfirmaMessages.getString("CreateHashDialog.3") //$NON-NLS-1$
		);
		fileTextFieldLabel.setLabelFor(this.fileTextField);
		this.fileTextField.setEditable(false);
		this.fileTextField.setFocusable(false);
		this.fileTextField.setColumns(80);
		add(fileTextFieldLabel);
		add(this.fileTextField);

		final JButton generateButton = new JButton(
			SimpleAfirmaMessages.getString("CreateHashDialog.4") //$NON-NLS-1$
		);

		final JButton fileButton = new JButton(
			SimpleAfirmaMessages.getString("CreateHashDialog.5") //$NON-NLS-1$
		);
		fileButton.setMnemonic('x');
		fileButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent ae) {
					try {
						getFileTextField().setText(
							AOUIFactory.getLoadFiles(
								SimpleAfirmaMessages.getString("CreateHashDialog.6"), //$NON-NLS-1$,
								null,
								null,
								null,
								SimpleAfirmaMessages.getString("CreateHashDialog.7"), //$NON-NLS-1$,,
								false,
								false,
								null,
								CreateHashDialog.this
							)[0].getAbsolutePath()
						);
						generateButton.setEnabled(true);
					}
					catch(final AOCancelledOperationException ex) {
						// Operacion cancelada por el usuario
					}
				}
			}
		);
		fileButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("CreateHashDialog.12") //$NON-NLS-1$
		);
		add(fileButton);

		generateButton.setEnabled(false);
		generateButton.setMnemonic('G');
		generateButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent e) {
					try ( final InputStream is = new FileInputStream(getFileTextField().getText()); ) {
						final byte[] data = AOUtil.getDataFromInputStream(is);
						final byte[] hash = MessageDigest.getInstance(
							getSelectedHashAlgorithm()
						).digest(data);
						AOUIFactory.getSaveDataToFile(
							isBase64Checked() ? Base64.encode(hash).getBytes() : hash,
							SimpleAfirmaMessages.getString("CreateHashDialog.8"), //$NON-NLS-1$,,,
							null,
							null,
							isBase64Checked() ? new String[] { "txt" } : new String[] { "hash" }, //$NON-NLS-1$ //$NON-NLS-2$
							SimpleAfirmaMessages.getString("CreateHashDialog.9") + " (*." + (isBase64Checked() ? "txt" : "hash") + ")",  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
							parent
						);
						CreateHashDialog.this.setVisible(false);
						CreateHashDialog.this.dispose();
					}
					catch(final AOCancelledOperationException aocoe) {
						return;
					}
					catch (final Exception ioe) {
						AOUIFactory.showErrorMessage(
							CreateHashDialog.this,
							SimpleAfirmaMessages.getString("CreateHashDialog.13"), //$NON-NLS-1$
							SimpleAfirmaMessages.getString("CreateHashDialog.14"), //$NON-NLS-1$
							JOptionPane.ERROR_MESSAGE
						);
						Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
							"Error generando o guardando la huella digital: " + e //$NON-NLS-1$
						);
						return;
					}

				}
			}
		);
		generateButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("CreateHashDialog.11") //$NON-NLS-1$
		);

		add(this.base64ChechBox);

		add(generateButton);
	}

}
