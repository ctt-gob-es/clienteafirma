package es.gob.afirma.standalone.ui.cipher;

import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_DECIPHER_ALGORITHM;

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
 * @author  Mariano Mart&iacute;nez. */
public final class DecipherDialog extends JDialog implements KeyListener{

	private static final long serialVersionUID = 1L;

	private static final String[] DECIPHER_ALGOS = new String[] {
			"Algoritmo 1", //$NON-NLS-1$
			"Algoritmo 2", //$NON-NLS-1$
	};

	private final JComboBox<String> decipherAlgorithms = new JComboBox<>(DECIPHER_ALGOS);
	String getSelectedDecipherAlgorithm() {
		return this.decipherAlgorithms.getSelectedItem().toString();
	}

	private final JButton decipherButton = new JButton(
			SimpleAfirmaMessages.getString("DecipherDialog.5") //$NON-NLS-1$
	);

	JButton getDecipherButton() {
		return this.decipherButton;
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
		if (this.password != null && this.password.length > 0) {
			return String.valueOf(this.password);
		}
		return null;
	}

	void setTextFieldDataText(final String text) {
		this.textFieldData.setText(text);
	}
	String getTextFieldDataText() {
		return this.textFieldData.getText();
	}

	/** Inicia el proceso de cifrado del fichero.
	 * @param parent Componente padre para la modalidad. */
	public static void startDecipher(final Frame parent) {
		new DecipherDialog(parent).setVisible(true);
	}

	/** Crea un di&aacute;logo para el cifrado de ficheros.
	 * @param parent Componente padre para la modalidad. */
	private DecipherDialog(final Frame parent) {
		super(parent);
		setTitle("DecipherDialog.0"); //$NON-NLS-1$
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
        setLocationRelativeTo(parent);
        setIconImage(
			AutoFirmaUtil.getDefaultDialogsIcon()
		);
		setTitle(SimpleAfirmaMessages.getString("DecipherDialog.1")); //$NON-NLS-1$

		final JLabel decipherAlgorithmsLabels = new JLabel(
				SimpleAfirmaMessages.getString("DecipherDialog.2") //$NON-NLS-1$
		);
		decipherAlgorithmsLabels.addKeyListener(this);
		decipherAlgorithmsLabels.setLabelFor(this.decipherAlgorithms);

		this.decipherAlgorithms.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent e) {
					PreferencesManager.put(
							PREFERENCE_DECIPHER_ALGORITHM,
						getSelectedDecipherAlgorithm()
					);
				}
			}
		);
		this.decipherAlgorithms.setSelectedItem(
			PreferencesManager.get(
					PREFERENCE_DECIPHER_ALGORITHM,
				"Algoritmo 1" //$NON-NLS-1$
			)
		);
		this.decipherAlgorithms.addKeyListener(this);

		final JLabel fileTextFieldLabel = new JLabel(
				SimpleAfirmaMessages.getString("DecipherDialog.3") //$NON-NLS-1$
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
								SimpleAfirmaMessages.getString("DecipherDialog.6"), //$NON-NLS-1$
								null,
								null,
								null,
								SimpleAfirmaMessages.getString("DecipherDialog.11"), //$NON-NLS-1$
								false,
								false,
								AutoFirmaUtil.getDefaultDialogsIcon(),
								DecipherDialog.this
							)[0].getAbsolutePath()
						);
						final String decipherFile = getTextFieldDataText();
						setPassword(getPasswordField().getPassword());
						final String pass = getPassword();
						if (!(decipherFile == null) && !decipherFile.isEmpty() && pass != null && !pass.isEmpty()) {
							getDecipherButton().setEnabled(true);
							getDecipherButton().requestFocus();
						}
					}
					catch(final AOCancelledOperationException ex) {
						// Operacion cancelada por el usuario
					}
				}
			}
		);
		textFieldDataButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("DecipherDialog.7") //$NON-NLS-1$
		);
		textFieldDataButton.addKeyListener(this);

		final JLabel jlabelPassword = new JLabel(SimpleAfirmaMessages.getString("DecipherDialog.8")); //$NON-NLS-1$
		jlabelPassword.addKeyListener(this);
		jlabelPassword.setLabelFor(this.passwordField);
		this.passwordField.setEchoChar('*');
		this.passwordField.addKeyListener(this);
		this.passwordField.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				final String decipherFile = getTextFieldDataText();
				setPassword(getPasswordField().getPassword());
				final String pass = getPassword();
				if (!(decipherFile == null) && !decipherFile.isEmpty() && pass != null && !pass.isEmpty()) {
					getDecipherButton().setEnabled(true);
					getDecipherButton().requestFocus();
				}
			}
		});

		this.decipherButton.addKeyListener(this);
		this.decipherButton.setEnabled(false);
		this.decipherButton.setMnemonic('D');
		this.decipherButton.addActionListener(
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
		this.decipherButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("DecipherDialog.9") //$NON-NLS-1$
		);

		final JButton cancelButton = new JButton(
				SimpleAfirmaMessages.getString("DecipherDialog.4") //$NON-NLS-1$
		);

		cancelButton.setMnemonic('A');
		cancelButton.addActionListener( new ActionListener () {
			@Override
			public void actionPerformed( final ActionEvent e ) {
				DecipherDialog.this.setVisible(false);
				DecipherDialog.this.dispose();
			}
		});
		cancelButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("DecipherDialog.10") //$NON-NLS-1$
		);
		cancelButton.addKeyListener(this);

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.RIGHT));

		// En Mac OS X el orden de los botones es distinto
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			panel.add(cancelButton);
			panel.add(this.decipherButton);
		}
		else {
			panel.add(this.decipherButton);
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
		c.add(decipherAlgorithmsLabels, gbc);
		gbc.insets = new Insets(5,10,0,10);
		gbc.gridy++;
		c.add(this.decipherAlgorithms, gbc);
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
		DecipherDialog.startDecipher(null);
	}

	@Override
	public void keyTyped(final KeyEvent e) { /* Vacio */ }

	@Override
	public void keyPressed(final KeyEvent e) { /* Vacio */ }

	@Override
	public void keyReleased(final KeyEvent ke) {
		// En Mac no cerramos los dialogos con Escape
		if (ke != null && ke.getKeyCode() == KeyEvent.VK_ESCAPE) {
			DecipherDialog.this.setVisible(false);
			DecipherDialog.this.dispose();
		}
		else {
			final String decipherFile = getTextFieldDataText();
			setPassword(getPasswordField().getPassword());
			final String pass = getPassword();
			if (!(decipherFile == null) && !decipherFile.isEmpty() && pass != null && !pass.isEmpty()) {
				getDecipherButton().setEnabled(true);
			}
		}
	}

}