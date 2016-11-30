package es.gob.afirma.standalone.ui.hash;

import java.awt.Container;
import java.awt.Cursor;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingWorker;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.CommonWaitDialog;
import es.gob.afirma.standalone.ui.preferences.PreferencesManager;

/** Di&aacute;logo para la creaci&oacute;n de huellas digitales.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CreateHashDialog extends JDialog implements KeyListener {

	private static final long serialVersionUID = 3581001930027153381L;

	private static final int SIZE_WAIT = 50000000; //Tamano en bytes

	private static final String[] HASH_ALGOS = new String[] {
		"SHA-256", //$NON-NLS-1$
		"SHA-1", //$NON-NLS-1$
		"SHA-384", //$NON-NLS-1$
		"SHA-512" //$NON-NLS-1$
	};

	enum HashFormat {

		HEX(SimpleAfirmaMessages.getString("CreateHashDialog.21")), //$NON-NLS-1$
		BASE64(SimpleAfirmaMessages.getString("CreateHashDialog.22")), //$NON-NLS-1$
		BINARY(SimpleAfirmaMessages.getString("CreateHashDialog.23")); //$NON-NLS-1$

		private final String name;

		private HashFormat(final String n) {
			this.name = n;
		}

		@Override
		public String toString() {
			return this.name;
		}

		static HashFormat[] getHashFormats() {
			return new HashFormat[] {
				HEX, BASE64, BINARY
			};
		}

		static HashFormat getDefaultFormat() {
			return HEX;
		}

		static HashFormat fromString(final String name) {
			if (HEX.toString().equals(name)) {
				return HEX;
			}
			if (BASE64.toString().equals(name)) {
				return BASE64;
			}
			if (BINARY.toString().equals(name)) {
				return BINARY;
			}
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
				"Formato de huella desconocido, se usara el por defecto: " + name //$NON-NLS-1$
			);
			return getDefaultFormat();
		}
	}

	private final JComboBox<String> hashAlgorithms = new JComboBox<>(HASH_ALGOS);
	String getSelectedHashAlgorithm() {
		return this.hashAlgorithms.getSelectedItem().toString();
	}

	private final JComboBox<HashFormat> hashFormats = new JComboBox<>(HashFormat.getHashFormats());
	HashFormat getSelectedHashFormat() {
		return (HashFormat) this.hashFormats.getSelectedItem();
	}

	private final JTextField fileTextField = new JTextField();
	JTextField getFileTextField() {
		return this.fileTextField;
	}

	final JCheckBox copyToClipBoardCheckBox = new JCheckBox(
		SimpleAfirmaMessages.getString("CreateHashDialog.19") //$NON-NLS-1$
	);
	boolean isCopyToClipBoardChecked() {
		return this.copyToClipBoardCheckBox.isSelected();
	}

	/** Inicia el proceso de creaci&oacute;n de huella digital.
	 * @param parent Componente padre para la modalidad. */
	public static void startHashCreation(final Frame parent) {
		final CreateHashDialog chd = new CreateHashDialog(parent);
		chd.setSize(600, 350);
		chd.setResizable(false);
		chd.setLocationRelativeTo(parent);
		chd.setVisible(true);
	}

	/** Crea un di&aacute;logo para la creaci&oacute;n de huellas digitales.
	 * @param parent Componente padre para la modalidad. */
	private CreateHashDialog(final Frame parent) {
		super(parent);
		setTitle(SimpleAfirmaMessages.getString("CreateHashDialog.15")); //$NON-NLS-1$
		setModalityType(ModalityType.APPLICATION_MODAL);
		createUI(parent);
	}

	void createUI(final Frame parent) {

		final Container c = getContentPane();
		final GridBagLayout gbl = new GridBagLayout();
		c.setLayout(gbl);
		final GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.gridy = 0;
        gbc.insets = new Insets(5,15,0,10);
		setIconImage(
			AutoFirmaUtil.getDefaultDialogsIcon()
		);
		getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("CreateHashDialog.1") //$NON-NLS-1$
		);

		final JLabel hashAlgorithmsLabel = new JLabel(
			SimpleAfirmaMessages.getString("CreateHashDialog.2") //$NON-NLS-1$
		);
		hashAlgorithmsLabel.setLabelFor(this.hashAlgorithms);

		this.hashAlgorithms.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent e) {
					PreferencesManager.put(
						PreferencesManager.PREFERENCE_CREATE_HASH_ALGORITHM,
						getSelectedHashAlgorithm()
					);
				}
			}
		);
		this.hashAlgorithms.setSelectedItem(
			PreferencesManager.get(
				PreferencesManager.PREFERENCE_CREATE_HASH_ALGORITHM,
				"SHA-256" //$NON-NLS-1$
			)
		);
		this.hashAlgorithms.addKeyListener(this);

		final JLabel hashFormatLabel = new JLabel(
			SimpleAfirmaMessages.getString("CreateHashDialog.0") //$NON-NLS-1$
		);
		hashFormatLabel.setLabelFor(this.hashFormats);

		this.hashFormats.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent e) {
					PreferencesManager.put(
						PreferencesManager.PREFERENCE_CREATE_HASH_FORMAT,
						getSelectedHashFormat().toString()
					);
				}
			}
		);
		this.hashFormats.setSelectedItem(
			HashFormat.fromString(
				PreferencesManager.get(
					PreferencesManager.PREFERENCE_CREATE_HASH_FORMAT,
					HashFormat.getDefaultFormat().toString()
				)
			)
		);
		this.hashFormats.addKeyListener(this);

		this.copyToClipBoardCheckBox.addActionListener(
				new ActionListener() {
					@Override
					public void actionPerformed(final ActionEvent e) {
						PreferencesManager.putBoolean(
								PreferencesManager.PREFERENCE_CREATE_HASH_CLIPBOARD,
								isCopyToClipBoardChecked()
						);
					}
				}
		);
		this.copyToClipBoardCheckBox.setSelected(
			PreferencesManager.getBoolean(
				PreferencesManager.PREFERENCE_CREATE_HASH_CLIPBOARD,
				true
			)
		);
		this.copyToClipBoardCheckBox.addKeyListener(this);

		final JLabel fileTextFieldLabel = new JLabel(
			SimpleAfirmaMessages.getString("CreateHashDialog.3") //$NON-NLS-1$
		);
		fileTextFieldLabel.addKeyListener(this);
		fileTextFieldLabel.setLabelFor(this.fileTextField);
		this.fileTextField.addKeyListener(this);
		this.fileTextField.setEditable(false);
		this.fileTextField.setFocusable(false);
		this.fileTextField.setColumns(10);

		final JButton generateButton = new JButton(
			SimpleAfirmaMessages.getString("CreateHashDialog.4") //$NON-NLS-1$
		);
		generateButton.addKeyListener(this);

		final JButton fileButton = new JButton(
			SimpleAfirmaMessages.getString("CreateHashDialog.5") //$NON-NLS-1$
		);
		fileButton.addKeyListener(this);
		fileButton.setMnemonic('E');
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
								AutoFirmaUtil.getDefaultDialogsIcon(),
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
		fileButton.addKeyListener(this);

		generateButton.setEnabled(false);
		generateButton.setMnemonic('G');
		generateButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent e) {
					doHashProcess(
						parent,
						getFileTextField().getText(),
						getSelectedHashAlgorithm(),
						getSelectedHashFormat(),
						isCopyToClipBoardChecked(),
						CreateHashDialog.this
					);
					CreateHashDialog.this.setVisible(false);
					CreateHashDialog.this.dispose();
				}
			}
		);
		generateButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("CreateHashDialog.11") //$NON-NLS-1$
		);

		final JButton exitButton = new JButton(
			SimpleAfirmaMessages.getString("CreateHashDialog.16") //$NON-NLS-1$
		);

		exitButton.setMnemonic('C');
		exitButton.addActionListener(
			new ActionListener () {
				@Override
				public void actionPerformed( final ActionEvent e ) {
					CreateHashDialog.this.setVisible(false);
					CreateHashDialog.this.dispose();
				}
			}
		);
		exitButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("CreateHashDialog.17") //$NON-NLS-1$
		);
		exitButton.addKeyListener(this);

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.RIGHT));

		// En Mac OS X el orden de los botones es distinto
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			panel.add(exitButton);
			panel.add(generateButton);
		}
		else {
			panel.add(generateButton);
			panel.add(exitButton);
		}

		c.add(fileTextFieldLabel, gbc);

		gbc.insets = new Insets(5,10,0,10);
		gbc.gridy++;
		c.add(this.fileTextField, gbc);

		gbc.weightx = 0;
		c.add(fileButton, gbc);

		gbc.insets = new Insets(20,15,0,10);
		gbc.weightx = 1.0;
		gbc.gridy++;
		c.add(hashAlgorithmsLabel, gbc);

		gbc.insets = new Insets(5,10,0,10);
		gbc.gridy++;
		c.add(this.hashAlgorithms, gbc);

		gbc.insets = new Insets(20,10,0,10);
		gbc.gridy++;
		c.add(hashFormatLabel, gbc);

		gbc.insets = new Insets(5,10,0,10);
		gbc.gridy++;
		c.add(this.hashFormats, gbc);

		gbc.insets = new Insets(20,10,0,10);
		gbc.gridy++;
		c.add(this.copyToClipBoardCheckBox, gbc);

		gbc.insets = new Insets(20,10,0,10);
		gbc.gridy++;
		gbc.gridwidth = GridBagConstraints.REMAINDER;
		c.add(panel, gbc);
	}

	static void doHashProcess(final Frame parent,
                              final String file,
                              final String hashAlgorithm,
                              final HashFormat format,
                              final boolean copyToClipboard,
                              final Window currentFrame) {

		final CommonWaitDialog dialog = new CommonWaitDialog(
			null,
			SimpleAfirmaMessages.getString("CreateHashFiles.18"), //$NON-NLS-1$
			SimpleAfirmaMessages.getString("CreateHashFiles.20") //$NON-NLS-1$
		);

		// Arrancamos el proceso en un hilo aparte
		final SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
			@Override
			protected Void doInBackground() {
				try ( final InputStream is = new FileInputStream(file); ) {

					if (currentFrame != null) {
						currentFrame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
					}

					final byte[] hash = HashUtil.getFileHash(hashAlgorithm, file);

					final String ext;
					final byte[] dataToSave;
					switch(format) {
						case HEX:
							ext = ".hexhash"; //$NON-NLS-1$
							dataToSave = AOUtil.hexify(hash, false).getBytes();
							break;
						case BASE64:
							ext = ".hashb64"; //$NON-NLS-1$
							dataToSave = Base64.encode(hash).getBytes();
							break;
						case BINARY:
							dataToSave = hash;
							ext = ".hash"; //$NON-NLS-1$
							break;
						default:
							throw new IllegalStateException(
								"Formato de huella no contemplado: " + format //$NON-NLS-1$
							);
					}

					dialog.dispose();
					AOUIFactory.getSaveDataToFile(
						dataToSave,
						SimpleAfirmaMessages.getString("CreateHashDialog.8"), //$NON-NLS-1$
						null,
						AutoFirmaUtil.getCanonicalFile(new File(file)).getName() + ext,
						new String[] { ext },
						SimpleAfirmaMessages.getString("CreateHashDialog.9") + " (*" + ext + ")",  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
						parent
					);
					// Si se selecciona Base64 se usa Base64 en portapapeles, en cualquier otro caso, HEX pasado a ASCII.
					if (copyToClipboard) {
						copyToClipBoard(
							HashFormat.BASE64.equals(format) ? new String(dataToSave) : AOUtil.hexify(hash, false)
						);
					}
				}
				catch(final OutOfMemoryError ooe) {
					AOUIFactory.showErrorMessage(
						parent,
						SimpleAfirmaMessages.getString("CreateHashDialog.18"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("CreateHashDialog.14"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE
					);
					Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
						"Fichero demasiado grande: " + ooe //$NON-NLS-1$
					);
					return null;
				}
				catch(final AOCancelledOperationException aocoe) {
					// Operacion cancelada
					return null;
				}
				catch (final Exception ioe) {
					AOUIFactory.showErrorMessage(
						parent,
						SimpleAfirmaMessages.getString("CreateHashDialog.13"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("CreateHashDialog.14"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE
					);
					Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
						"Error generando o guardando la huella digital: " + ioe //$NON-NLS-1$
					);
					return null;
				}
				finally {
					if (currentFrame != null) {
						currentFrame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
					}
				}

				return null;
			}
			@Override
			protected void done() {
				super.done();
				dialog.dispose();
			}
		};
		worker.execute();

		if (new File(file).length() > SIZE_WAIT) {
			// Se muestra la ventana de espera
			dialog.setVisible(true);
		}
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
			CreateHashDialog.this.setVisible(false);
			CreateHashDialog.this.dispose();
		}
	}

	/** Copia un texto al portapapeles del sistema.
	 * @param text Contenido a copiar. */
	static void copyToClipBoard(final String text) {
		final StringSelection stringSelection = new StringSelection(text);
		final Clipboard clpbrd = Toolkit.getDefaultToolkit().getSystemClipboard();
		clpbrd.setContents(stringSelection, null);
	}
}
