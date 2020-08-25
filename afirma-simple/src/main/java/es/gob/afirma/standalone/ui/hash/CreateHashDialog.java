/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.hash;

import java.awt.Container;
import java.awt.Dialog;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.security.NoSuchAlgorithmException;
import java.util.Collections;
import java.util.Date;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;
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
import es.gob.afirma.core.ui.GenericFileFilter;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.CommonWaitDialog;
import es.gob.afirma.standalone.ui.preferences.PreferencesManager;

/** Di&aacute;logo para la creaci&oacute;n de huellas digitales.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CreateHashDialog extends JDialog implements KeyListener {

	private static final long serialVersionUID = 3581001930027153381L;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

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

		private static final String HEX_ALT_NAME = "hex"; //$NON-NLS-1$
		private static final String BASE64_ALT_NAME = "b64"; //$NON-NLS-1$
		private static final String BINARY_ALT_NAME = "bin"; //$NON-NLS-1$

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
			if (HEX.toString().equals(name) || HEX_ALT_NAME.equalsIgnoreCase(name)) {
				return HEX;
			}
			if (BASE64.toString().equals(name) || BASE64_ALT_NAME.equalsIgnoreCase(name)) {
				return BASE64;
			}
			if (BINARY.toString().equals(name) || BINARY_ALT_NAME.equalsIgnoreCase(name)) {
				return BINARY;
			}
			LOGGER.warning(
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
			e -> PreferencesManager.put(
				PreferencesManager.PREFERENCE_CREATE_HASH_ALGORITHM,
				getSelectedHashAlgorithm()
			)
		);
		this.hashAlgorithms.setSelectedItem(
			PreferencesManager.get(PreferencesManager.PREFERENCE_CREATE_HASH_ALGORITHM)
		);
		this.hashAlgorithms.addKeyListener(this);

		final JLabel hashFormatLabel = new JLabel(
			SimpleAfirmaMessages.getString("CreateHashDialog.0") //$NON-NLS-1$
		);
		hashFormatLabel.setLabelFor(this.hashFormats);

		this.hashFormats.addActionListener(
			e -> PreferencesManager.put(
				PreferencesManager.PREFERENCE_CREATE_HASH_FORMAT,
				getSelectedHashFormat().toString()
			)
		);
		this.hashFormats.setSelectedItem(
			HashFormat.fromString(
				PreferencesManager.get(PreferencesManager.PREFERENCE_CREATE_HASH_FORMAT)
			)
		);
		this.hashFormats.addKeyListener(this);

		this.copyToClipBoardCheckBox.addActionListener(
				e -> PreferencesManager.putBoolean(
						PreferencesManager.PREFERENCE_CREATE_HASH_CLIPBOARD,
						isCopyToClipBoardChecked()
				)
		);
		this.copyToClipBoardCheckBox.setSelected(
			PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_CREATE_HASH_CLIPBOARD)
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
			ae -> {
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
		);
		fileButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("CreateHashDialog.12") //$NON-NLS-1$
		);
		fileButton.addKeyListener(this);

		generateButton.setEnabled(false);
		generateButton.setMnemonic('G');
		generateButton.addActionListener(
			e -> {
				try {
					doHashProcess(
							parent,
							new File(getFileTextField().getText()),
							null,
							getSelectedHashAlgorithm(),
							getSelectedHashFormat(),
							isCopyToClipBoardChecked());
				}
				catch (final AOCancelledOperationException ex) {
					// El usuario cancelo el dialogo de guardado. Salimos del proceso para evitar que
					// se cierre la pantalla.
					return;
				}
				CreateHashDialog.this.setVisible(false);
				CreateHashDialog.this.dispose();
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
			e -> {
				CreateHashDialog.this.setVisible(false);
				CreateHashDialog.this.dispose();
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
                              final File file,
                              final File outputFile,
                              final String hashAlgorithm,
                              final HashFormat format,
                              final boolean copyToClipboard) throws AOCancelledOperationException {

		final CommonWaitDialog dialog = new CommonWaitDialog(
			null,
			SimpleAfirmaMessages.getString("CreateHashFiles.18"), //$NON-NLS-1$
			SimpleAfirmaMessages.getString("CreateHashFiles.20") //$NON-NLS-1$
		);

		// Esperamos al calculo del resultado
		byte[] calculatedHash;
		try {
			calculatedHash = calculateHash(file, hashAlgorithm, dialog);
		}
		catch (final Exception ioe) {
			LOGGER.log(Level.SEVERE, "Error generando o guardando la huella digital", ioe); //$NON-NLS-1$
			AOUIFactory.showErrorMessage(
				parent,
				SimpleAfirmaMessages.getString("CreateHashDialog.13"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("CreateHashDialog.14"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
			return;
		}

		// Identificamos el formato de guardado
		final String ext;
		final byte[] dataToSave;
		switch(format) {
			case HEX:
				ext = "hexhash"; //$NON-NLS-1$
				dataToSave = (AOUtil.hexify(calculatedHash, false) + "h").getBytes(); //$NON-NLS-1$
				break;
			case BASE64:
				ext = "hashb64"; //$NON-NLS-1$
				dataToSave = Base64.encode(calculatedHash).getBytes();
				break;
			case BINARY:
				ext = "hash"; //$NON-NLS-1$
				dataToSave = calculatedHash;
				break;
			default:
				throw new IllegalStateException(
					"Formato de huella no contemplado: " + format //$NON-NLS-1$
				);
		}

		// Guardamos los datos
		try {
			AOUIFactory.getSaveDataToFile(
					dataToSave,
					SimpleAfirmaMessages.getString("CreateHashDialog.8"), //$NON-NLS-1$
					outputFile != null && outputFile.getParentFile() != null ?
							outputFile.getParentFile().getAbsolutePath() :
							null,
					outputFile != null ?
							outputFile.getName() :
							AutoFirmaUtil.getCanonicalFile(file).getName() + "." + ext, //$NON-NLS-1$
					Collections.singletonList(
							new GenericFileFilter(
									new String[] { ext },
									SimpleAfirmaMessages.getString("CreateHashDialog.9") + " (*." + ext + ")"  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
									)
							),
					parent
					);
			// Si se selecciona Base64 se usa Base64 en portapapeles, en cualquier otro caso, HEX pasado a ASCII.
			if (copyToClipboard) {
				copyToClipBoard(
						HashFormat.BASE64.equals(format) ? new String(dataToSave) : AOUtil.hexify(dataToSave, false)
						);
			}
		}
		catch (final AOCancelledOperationException e) {
			throw e;
		}
		catch (final Exception e) {
			LOGGER.log(Level.SEVERE, "Error durante el guardado del hash", e); //$NON-NLS-1$
			AOUIFactory.showErrorMessage(
				parent,
				SimpleAfirmaMessages.getString("CreateHashDialog.27"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("CreateHashDialog.14"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
		}
	}

	public static byte[] calculateHash(final File file,
			final String hashAlgorithm, final Dialog waitingDialog) throws InterruptedException, ExecutionException {

		// Arrancamos el proceso en un hilo aparte
		final SwingWorker<byte[], Void> worker = new SwingWorker<byte[], Void>() {
			@Override
			protected byte[] doInBackground() throws FileNotFoundException, IOException, NoSuchAlgorithmException {

				final long startTime = new Date().getTime();

				byte[] calculatedHash;
				try ( final InputStream is = new FileInputStream(file); ) {
					calculatedHash = HashUtil.getFileHash(hashAlgorithm, file);

					final long processTime = new Date().getTime() - startTime;
					LOGGER.info("Tiempo total de generacion del hash: " + processTime / 1000.0 + " seg"); //$NON-NLS-1$ //$NON-NLS-2$

					return calculatedHash;
				}
			}
			@Override
			protected void done() {
				super.done();
				if (waitingDialog != null) {
					waitingDialog.dispose();
				}
			}
		};
		worker.execute();

		// Se muestra la ventana de progreso
		if (waitingDialog != null) {
			waitingDialog.setVisible(true);
		}
		// Esperamos al calculo del resultado
		return worker.get();
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
		if (ke != null && ke.getKeyCode() == KeyEvent.VK_ESCAPE && !Platform.OS.MACOSX.equals(Platform.getOS())) {
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
