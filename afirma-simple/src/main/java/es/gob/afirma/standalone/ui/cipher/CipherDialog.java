package es.gob.afirma.standalone.ui.cipher;

import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_CIPHER_ALGORITHM;

import java.awt.Container;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.preferences.PreferencesManager;

/** Di&aacute;logo para el cifrado de ficheros.
 * @author Mariano Mart&iacute;nez. */
public final class CipherDialog extends JDialog implements KeyListener{

	private static final long serialVersionUID = -9133887916481572642L;

	private static final String[] CIPHER_ALGOS = new String[] {
			"Algoritmo 1", //$NON-NLS-1$
			"Algoritmo 2", //$NON-NLS-1$
			"Algoritmo 3", //$NON-NLS-1$
	};

	private final JComboBox<String> cipherAlgorithms = new JComboBox<>(CIPHER_ALGOS);
	String getSelectedCipherAlgorithm() {
		return this.cipherAlgorithms.getSelectedItem().toString();
	}

	private final JButton cipherButton = new JButton(
			SimpleAfirmaMessages.getString("CipherDialog.5") //$NON-NLS-1$
	);

	JButton getCipherButton() {
		return this.cipherButton;
	}

	private final JTextField textFieldData = new JTextField();

	private final JPasswordField passwordField = new JPasswordField(10);
	private char[] password;

	JPasswordField getPasswordField() {
		return this.passwordField;
	}

	void setPassword(final char[] text) {
		this.password = text;
	}

	String getPassword() {
			return String.valueOf(this.password);
	}

	void setTextFieldDataText(final String text) {
		this.textFieldData.setText(text);
	}
	String getTextFieldDataText() {
		return this.textFieldData.getText();
	}

	/** Inicia el proceso de cifrado del fichero.
	 * @param parent Componente padre para la modalidad. */
	public static void startCipher(final Frame parent) {
		new CipherDialog(parent).setVisible(true);
	}

	/** Crea un di&aacute;logo para el cifrado de ficheros.
	 * @param parent Componente padre para la modalidad. */
	private CipherDialog(final Frame parent) {
		super(parent);
		setTitle(SimpleAfirmaMessages.getString("CipherDialog.0")); //$NON-NLS-1$
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

		final Container c = getContentPane();
		final GridBagLayout gbl = new GridBagLayout();
		c.setLayout(gbl);
		final GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.gridy = 0;
        gbc.insets = new Insets(10,15,0,10);
        setIconImage(
			AutoFirmaUtil.getDefaultDialogsIcon()
		);
        getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("CipherDialog.1") //$NON-NLS-1$
		);

		final JLabel cipherAlgorithmsLabels = new JLabel(
				SimpleAfirmaMessages.getString("CipherDialog.2") //$NON-NLS-1$
		);
		cipherAlgorithmsLabels.addKeyListener(this);
		cipherAlgorithmsLabels.setLabelFor(this.cipherAlgorithms);

		this.cipherAlgorithms.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent e) {
					PreferencesManager.put(
							PREFERENCE_CIPHER_ALGORITHM,
						getSelectedCipherAlgorithm()
					);
				}
			}
		);
		this.cipherAlgorithms.setSelectedItem(
			PreferencesManager.get(
					PREFERENCE_CIPHER_ALGORITHM,
					getSelectedCipherAlgorithm()
			)
		);
		this.cipherAlgorithms.addKeyListener(this);

		final JLabel fileTextFieldLabel = new JLabel(
				SimpleAfirmaMessages.getString("CipherDialog.3") //$NON-NLS-1$
		);
		fileTextFieldLabel.addKeyListener(this);
		fileTextFieldLabel.setLabelFor(this.textFieldData);
		this.textFieldData.addKeyListener(this);
		this.textFieldData.setEditable(false);
		this.textFieldData.setFocusable(false);
		this.textFieldData.setColumns(10);

		final JButton textFieldDataButton =  new JButton(SimpleAfirmaMessages.getString("CheckHashDialog.12")); //$NON-NLS-1$
		textFieldDataButton.addKeyListener(this);
		textFieldDataButton.setMnemonic('E');
		textFieldDataButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent ae) {
					try {
						setTextFieldDataText(
							AOUIFactory.getLoadFiles(
								SimpleAfirmaMessages.getString("CipherDialog.6"), //$NON-NLS-1$
								null,
								null,
								null,
								SimpleAfirmaMessages.getString("CipherDialog.11"), //$NON-NLS-1$
								false,
								false,
								AutoFirmaUtil.getDefaultDialogsIcon(),
								CipherDialog.this
							)[0].getAbsolutePath()
						);
						final String cipherFile = getTextFieldDataText();
						setPassword(getPasswordField().getPassword());
						final String pass = getPassword();
						if (!(cipherFile == null) && !cipherFile.isEmpty() && pass != null && !pass.isEmpty()) {
							getCipherButton().setEnabled(true);
							getCipherButton().requestFocus();
						}
					}
					catch(final AOCancelledOperationException ex) {
						// Operacion cancelada por el usuario
					}
				}
			}
		);
		textFieldDataButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("CipherDialog.7") //$NON-NLS-1$
		);
		textFieldDataButton.addKeyListener(this);

		final JLabel jlabelPassword = new JLabel(SimpleAfirmaMessages.getString("CipherDialog.8")); //$NON-NLS-1$
		jlabelPassword.addKeyListener(this);
		jlabelPassword.setLabelFor(this.passwordField);
		this.passwordField.setEchoChar('*');
		this.passwordField.addKeyListener(this);
		this.passwordField.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				final String cipherFile = getTextFieldDataText();
				setPassword(getPasswordField().getPassword());
				final String pass = getPassword();
				if (!(cipherFile == null) && !cipherFile.isEmpty() && pass != null && !pass.isEmpty()) {
					getCipherButton().setEnabled(true);
					getCipherButton().requestFocus();
				}
			}
		});

		this.cipherButton.addKeyListener(this);
		this.cipherButton.setEnabled(false);
		this.cipherButton.setMnemonic('C');
		this.cipherButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent e) {
					setPassword(getPasswordField().getPassword());
					final String pass = getPassword();
					if (pass != null && !pass.isEmpty()){
					//logica de cifrado
					}
				}
			}
		);
		this.cipherButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("CipherDialog.9") //$NON-NLS-1$
		);

		final JButton cancelButton = new JButton(
				SimpleAfirmaMessages.getString("CipherDialog.4") //$NON-NLS-1$
		);

		cancelButton.setMnemonic('A');
		cancelButton.addActionListener( new ActionListener () {
			@Override
			public void actionPerformed( final ActionEvent e ) {
				CipherDialog.this.setVisible(false);
				CipherDialog.this.dispose();
			}
		});
		cancelButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("CipherDialog.10") //$NON-NLS-1$
		);
		cancelButton.addKeyListener(this);

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.RIGHT));

		// En Mac OS X el orden de los botones es distinto
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			panel.add(cancelButton);
			panel.add(this.cipherButton);
		}
		else {
			panel.add(this.cipherButton);
			panel.add(cancelButton);
		}

		c.add(fileTextFieldLabel, gbc);
		gbc.insets = new Insets(5,10,0,10);
		gbc.weightx = 1.0;
		gbc.gridy++;
		c.add(this.textFieldData, gbc);
		gbc.weightx = 0.0;
		gbc.fill = GridBagConstraints.NONE;
		gbc.anchor = GridBagConstraints.LINE_END;
		c.add(textFieldDataButton, gbc);
		gbc.insets = new Insets(30,15,0,10);
		gbc.fill = GridBagConstraints.BOTH;
		gbc.gridy++;
		c.add(cipherAlgorithmsLabels, gbc);
		gbc.insets = new Insets(5,10,0,10);
		gbc.gridy++;
		c.add(this.cipherAlgorithms, gbc);
		gbc.insets = new Insets(30,15,0,10);
		gbc.gridy++;
		c.add(jlabelPassword, gbc);
		gbc.insets = new Insets(5,10,0,10);
		gbc.gridy++;
		c.add(this.passwordField, gbc);
		gbc.insets = new Insets(30,10,0,10);
		gbc.gridy++;
		gbc.gridwidth = GridBagConstraints.REMAINDER;
		c.add(panel, gbc);
		pack();
        setSize(600, 320);
		setResizable(false);
		setLocationRelativeTo(parent);
	}

	public static void main(final String[] args) {
		CipherDialog.startCipher(null);
	}

	@Override
	public void keyTyped(final KeyEvent e) { /* Vacio */ }

	@Override
	public void keyPressed(final KeyEvent e) { /* Vacio */ }

	@Override
	public void keyReleased(final KeyEvent ke) {
		// En Mac no cerramos los dialogos con Escape
		if (ke != null && ke.getKeyCode() == KeyEvent.VK_ESCAPE) {
			CipherDialog.this.setVisible(false);
			CipherDialog.this.dispose();
		}
		else{
			final String cipherFile = getTextFieldDataText();
			setPassword(getPasswordField().getPassword());
			final String pass = getPassword();
			if (!(cipherFile == null) && !cipherFile.isEmpty() && pass != null && !pass.isEmpty()) {
				getCipherButton().setEnabled(true);
			}
		}
	}

}
