/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.plugin.hash;

import java.awt.Container;
import java.awt.Dialog;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.Window;
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
import java.util.Properties;
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
import es.gob.afirma.standalone.plugins.UIFactory;

/** Di&aacute;logo para la creaci&oacute;n de huellas digitales.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CreateHashFileDialog extends JDialog implements KeyListener {

	private static final long serialVersionUID = 3581001930027153381L;

	static final Logger LOGGER = Logger.getLogger(CreateHashFileDialog.class.getName());

	private static final String[] HASH_ALGOS = new String[] {
		"SHA-256", //$NON-NLS-1$
		"SHA-1", //$NON-NLS-1$
		"SHA-384", //$NON-NLS-1$
		"SHA-512" //$NON-NLS-1$
	};

	enum HashFormat {

		HEX(Messages.getString("CreateHashDialog.21")), //$NON-NLS-1$
		BASE64(Messages.getString("CreateHashDialog.22")), //$NON-NLS-1$
		BINARY(Messages.getString("CreateHashDialog.23")); //$NON-NLS-1$

		private static final String HEX_ALT_NAME = "hex"; //$NON-NLS-1$
		private static final String BASE64_ALT_NAME = "b64"; //$NON-NLS-1$
		private static final String BINARY_ALT_NAME = "bin"; //$NON-NLS-1$

		private final String name;

		HashFormat(final String n) {
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

	private final Properties config;

	private final JComboBox<String> hashAlgorithms = new JComboBox<>(HASH_ALGOS);
	String getSelectedHashAlgorithm() {
		Object selectedItem = this.hashAlgorithms.getSelectedItem();
		if (selectedItem == null) {
			selectedItem = HASH_ALGOS[0];
		}
		return selectedItem.toString();
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
		Messages.getString("CreateHashDialog.19") //$NON-NLS-1$
	);
	boolean isCopyToClipBoardChecked() {
		return this.copyToClipBoardCheckBox.isSelected();
	}

	/**
	 * Inicia el proceso de creaci&oacute;n de huella digital.
	 * @param parent Componente padre para la modalidad.
	 * @param config Configuraci&oacute;n para la creaci&oacute;n de hash.
	 */
	public static void startHashCreation(final Window parent, final Properties config) {
		final CreateHashFileDialog chd = new CreateHashFileDialog(parent, config);
		chd.setSize(600, 350);
		chd.setResizable(false);
		chd.setLocationRelativeTo(parent);
		chd.setVisible(true);
	}

	/**
	 * Crea un di&aacute;logo para la creaci&oacute;n de huellas digitales.
	 * @param parent Componente padre para la modalidad.
	 * @param config Configuraci&oacute;n para la creaci&oacute;n de hash.
	 */
	private CreateHashFileDialog(final Window parent, final Properties config) {
		super(parent);

		this.config = config;

		setTitle(Messages.getString("CreateHashDialog.15")); //$NON-NLS-1$
		setModalityType(ModalityType.APPLICATION_MODAL);
		createUI(parent);
	}

	void createUI(final Window parent) {

		final Container c = getContentPane();
		final GridBagLayout gbl = new GridBagLayout();
		c.setLayout(gbl);
		final GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.gridy = 0;
        gbc.insets = new Insets(5,15,0,10);
		try {
			setIconImage(UIFactory.getDefaultDialogIcon());
		}
		catch (final Exception e) {
			LOGGER.warning("No se ha podido cargar el icono del dialogo: " + e); //$NON-NLS-1$
		}
		getAccessibleContext().setAccessibleDescription(
			Messages.getString("CreateHashDialog.1") //$NON-NLS-1$
		);

		final JLabel hashAlgorithmsLabel = new JLabel(
			Messages.getString("CreateHashDialog.2") //$NON-NLS-1$
		);
		hashAlgorithmsLabel.setLabelFor(this.hashAlgorithms);

		this.hashAlgorithms.setSelectedItem(
				this.config.getProperty(
						HashPreferences.PREFERENCE_CREATE_HASH_ALGORITHM,
						HASH_ALGOS[0]));

		this.hashAlgorithms.addActionListener(
			e -> this.config.setProperty(
				HashPreferences.PREFERENCE_CREATE_HASH_ALGORITHM,
				getSelectedHashAlgorithm()
			)
		);

		this.hashAlgorithms.addKeyListener(this);

		final JLabel hashFormatLabel = new JLabel(
			Messages.getString("CreateHashDialog.0") //$NON-NLS-1$
		);
		hashFormatLabel.setLabelFor(this.hashFormats);

		this.hashFormats.setSelectedItem(
				HashFormat.fromString(
						this.config.getProperty(
								HashPreferences.PREFERENCE_CREATE_HASH_FORMAT)));

		this.hashFormats.addActionListener(
			e -> this.config.setProperty(
				HashPreferences.PREFERENCE_CREATE_HASH_FORMAT,
				getSelectedHashFormat().toString()
			)
		);

		this.hashFormats.addKeyListener(this);

		this.copyToClipBoardCheckBox.addActionListener(
				e -> this.config.setProperty(
						HashPreferences.PREFERENCE_CREATE_HASH_CLIPBOARD,
						Boolean.toString(isCopyToClipBoardChecked())
				)
		);
		this.copyToClipBoardCheckBox.setSelected(
			Boolean.parseBoolean(this.config.getProperty(HashPreferences.PREFERENCE_CREATE_HASH_CLIPBOARD))
		);
		this.copyToClipBoardCheckBox.addKeyListener(this);

		final JLabel fileTextFieldLabel = new JLabel(
			Messages.getString("CreateHashDialog.3") //$NON-NLS-1$
		);
		fileTextFieldLabel.addKeyListener(this);
		fileTextFieldLabel.setLabelFor(this.fileTextField);
		this.fileTextField.addKeyListener(this);
		this.fileTextField.setEditable(false);
		this.fileTextField.setFocusable(false);
		this.fileTextField.setColumns(10);

		final JButton generateButton = new JButton(
			Messages.getString("CreateHashDialog.4") //$NON-NLS-1$
		);
		generateButton.addKeyListener(this);

		final JButton fileButton = new JButton(
			Messages.getString("CreateHashDialog.5") //$NON-NLS-1$
		);
		fileButton.addKeyListener(this);
		fileButton.setMnemonic('E');
		fileButton.addActionListener(
			ae -> {
				try {
					getFileTextField().setText(
						AOUIFactory.getLoadFiles(
							Messages.getString("CreateHashDialog.6"), //$NON-NLS-1$,
							null,
							null,
							null,
							Messages.getString("CreateHashDialog.7"), //$NON-NLS-1$,,
							false,
							false,
							getDialogIcon(),
							CreateHashFileDialog.this
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
			Messages.getString("CreateHashDialog.12") //$NON-NLS-1$
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
				CreateHashFileDialog.this.setVisible(false);
				CreateHashFileDialog.this.dispose();
			}
		);
		generateButton.getAccessibleContext().setAccessibleDescription(
			Messages.getString("CreateHashDialog.11") //$NON-NLS-1$
		);

		final JButton exitButton = new JButton(
			Messages.getString("CreateHashDialog.16") //$NON-NLS-1$
		);

		exitButton.setMnemonic('C');
		exitButton.addActionListener(
			e -> {
				CreateHashFileDialog.this.setVisible(false);
				CreateHashFileDialog.this.dispose();
			}
		);
		exitButton.getAccessibleContext().setAccessibleDescription(
			Messages.getString("CreateHashDialog.17") //$NON-NLS-1$
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

	static void doHashProcess(final Window parent,
                              final File file,
                              final File outputFile,
                              final String hashAlgorithm,
                              final HashFormat format,
                              final boolean copyToClipboard) throws AOCancelledOperationException {

		// Se crea la ventana de espera
		JDialog dialog;
		try {
			dialog = UIFactory.getWaitingDialog(
					parent,
					Messages.getString("CreateHashFiles.18"), //$NON-NLS-1$
					Messages.getString("CreateHashFiles.20") //$NON-NLS-1$
					);
		}
		catch (final Exception e) {
			LOGGER.warning("No se ha podido cargar el dialogo de espera: " + e); //$NON-NLS-1$
			dialog = null;
		}

		// Esperamos al calculo del resultado
		byte[] calculatedHash;
		try {
			calculatedHash = calculateHash(file, hashAlgorithm, dialog);
		}
		catch (final Exception ioe) {
			LOGGER.log(Level.SEVERE, "Error generando o guardando la huella digital", ioe); //$NON-NLS-1$
			AOUIFactory.showErrorMessage(
				Messages.getString("CreateHashDialog.13"), //$NON-NLS-1$
				Messages.getString("CreateHashDialog.14"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE,
				ioe
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

		// Guardamos en el Portapapeles si es necesario
		if (copyToClipboard) {
			// Se guardara la huella en el portapapeles en el formato seleccionado, salvo que
			// se seleccione el formato binario, en cuyo caso se guardara en hexadecimal
			copyToClipBoard(
					HashFormat.BINARY.equals(format) ? AOUtil.hexify(dataToSave, false) + "h" : new String(dataToSave) //$NON-NLS-1$
					);
		}

		// Guardamos los datos
		try {
			AOUIFactory.getSaveDataToFile(
					dataToSave,
					Messages.getString("CreateHashDialog.8"), //$NON-NLS-1$
					outputFile != null && outputFile.getParentFile() != null ?
							outputFile.getParentFile().getAbsolutePath() :
							null,
					outputFile != null ?
							outputFile.getName() :
							FileUtils.getCanonicalFile(file).getName() + "." + ext, //$NON-NLS-1$
					Collections.singletonList(
							new GenericFileFilter(
									new String[] { ext },
									Messages.getString("CreateHashDialog.9") + " (*." + ext + ")"  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
									)
							),
					parent
					);
		}
		catch (final AOCancelledOperationException e) {
			throw e;
		}
		catch (final Exception e) {
			LOGGER.log(Level.SEVERE, "Error durante el guardado del hash", e); //$NON-NLS-1$
			AOUIFactory.showErrorMessage(
				Messages.getString("CreateHashDialog.27"), //$NON-NLS-1$
				Messages.getString("CreateHashDialog.14"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE,
				e
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
			CreateHashFileDialog.this.setVisible(false);
			CreateHashFileDialog.this.dispose();
		}
	}

	/** Copia un texto al portapapeles del sistema.
	 * @param text Contenido a copiar. */
	static void copyToClipBoard(final String text) {
		final StringSelection stringSelection = new StringSelection(text);
		final Clipboard clpbrd = Toolkit.getDefaultToolkit().getSystemClipboard();
		clpbrd.setContents(stringSelection, null);
	}

	private static Image getDialogIcon() {
		Image icon;
		try {
			icon = UIFactory.getDefaultDialogIcon();
		}
		catch (final Exception e) {
			LOGGER.warning("No se ha podido cargar el icono del dialogo: " + e); //$NON-NLS-1$
			icon = null;
		}
		return icon;
	}
}
