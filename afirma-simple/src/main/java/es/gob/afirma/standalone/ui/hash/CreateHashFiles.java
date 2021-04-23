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
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.RecursiveAction;
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
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.ui.GenericFileFilter;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.CommonWaitDialog;
import es.gob.afirma.standalone.ui.preferences.PreferencesManager;

/** Genera huellas digitales de directorios (conjunto de ficheros).
 * @author Juliana Marulanda. */
public final class CreateHashFiles extends JDialog implements KeyListener {

	private static final long serialVersionUID = -7224732001218823361L;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String[] HASH_ALGOS = new String[] {
		"SHA-256", //$NON-NLS-1$
		"SHA-1", //$NON-NLS-1$
		"SHA-384", //$NON-NLS-1$
		"SHA-512" //$NON-NLS-1$
	};

	private static final String FILEEXT_XML = "hashfiles"; //$NON-NLS-1$
	private static final String FILEEXT_TXT = "txthashfiles"; //$NON-NLS-1$
	private static final String FILEEXT_CSV = "csv"; //$NON-NLS-1$

	private final JComboBox<String> hashAlgorithms = new JComboBox<>(HASH_ALGOS);
	private final JTextField selectedFile = new JTextField();
	private final JButton examineButton = new JButton();
	private final JButton generateButton = new JButton();
	private final JCheckBox recursive = new JCheckBox(
		SimpleAfirmaMessages.getString("CreateHashFiles.16") //$NON-NLS-1$
	);

	/** Inicia el proceso de creaci&oacute;n de huella digital de directorios.
	 * @param parent Componente padre para la modalidad. */
	public static void startHashCreation(final Frame parent) {
		final JDialog dialog = new CreateHashFiles(parent);
		dialog.setSize(600, 250);
		dialog.setResizable(false);
		dialog.setLocationRelativeTo(parent);
		dialog.setVisible(true);
	}

	/** Crea un di&aacute;logo para la creaci&oacute;n de huellas digitales de
	 * directorios.
	 * @param parent Componente padre para la modalidad. */
	private CreateHashFiles(final Frame parent) {
		super(parent);
		setTitle(SimpleAfirmaMessages.getString("CreateHashFiles.0")); //$NON-NLS-1$
		setModalityType(ModalityType.APPLICATION_MODAL);
		createUI(parent);
	}

	/** Crea todos los elementos necesarios para generar una huella digital de
	 * directorios.
	 * @param parent Componente padre para la modalidad. */
	void createUI(final Frame parent) {

		final Container c = getContentPane();
		final GridBagLayout gbl = new GridBagLayout();
		c.setLayout(gbl);
		final GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.weightx = 1.0;
		gbc.gridy = 0;
		gbc.insets = new Insets(10, 10, 0, 10);

		setLayout(new GridBagLayout());
		setLocationRelativeTo(parent);
		setIconImage(
			AutoFirmaUtil.getDefaultDialogsIcon()
		);
		getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("CreateHashFiles.1") //$NON-NLS-1$
		);

		// Label con el nombre del fichero
		final JLabel label = new JLabel(SimpleAfirmaMessages.getString("CreateHashFiles.11")); //$NON-NLS-1$
		this.examineButton.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("CreateHashFiles.10")); //$NON-NLS-1$

		// Boton examinar
		this.examineButton.setText(SimpleAfirmaMessages.getString("CreateHashFiles.10")); //$NON-NLS-1$
		this.examineButton.setMnemonic('X');
		this.examineButton.addKeyListener(this);
		this.examineButton.addActionListener(
			ae -> openSelectedFile()
		);
		this.examineButton.setEnabled(true);
		this.selectedFile.setEditable(false);
		this.selectedFile.setFocusable(false);
		this.selectedFile.addKeyListener(this);

		// Label con el algoritmo
		final JLabel labelAlg = new JLabel(SimpleAfirmaMessages.getString("CreateHashDialog.2")); //$NON-NLS-1$

		// ComboBox con los algoritmos de generacion
		this.hashAlgorithms.setSelectedItem(
			PreferencesManager.get(PreferencesManager.PREFERENCE_CREATE_HASH_DIRECTORY_ALGORITHM)
		);
		this.hashAlgorithms.addKeyListener(this);
		this.hashAlgorithms.addActionListener(
			e -> PreferencesManager.put(
				PreferencesManager.PREFERENCE_CREATE_HASH_DIRECTORY_ALGORITHM,
				getSelectedHashAlgorithm()
			)
		);

		// CheckBox que indica si debe calcularse el hash de los ficheros en los subdirectorios
		this.recursive.setSelected(
			PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_CREATE_HASH_DIRECTORY_RECURSIVE)
		);

		this.recursive.addKeyListener(this);
		this.recursive.addActionListener(actionEvent ->
			PreferencesManager.putBoolean(
				PreferencesManager.PREFERENCE_CREATE_HASH_DIRECTORY_RECURSIVE,
				isRecursive())
		);
		this.recursive.setMnemonic('R');

		// Boton de generacion de la huella
		this.generateButton.setText(SimpleAfirmaMessages.getString("CreateHashDialog.4")); //$NON-NLS-1$
		this.generateButton.setMnemonic('G');
		this.generateButton.addKeyListener(this);
		this.generateButton.addActionListener(ae -> {

			try {
				doHashProcess(
						parent,
						new File(getFileTextField().getText()),
						null,
						getSelectedHashAlgorithm(),
						null,
						isRecursive()
						);
			}
			catch (final AOCancelledOperationException e) {
				// Operacion cancelada. Terminamos la ejecucion para no cerrar la ventana
				return;
			}

			CreateHashFiles.this.setVisible(false);
			CreateHashFiles.this.dispose();
		}
		);
		this.generateButton.setEnabled(false);

		// Panel donde se anade el boton de generar
		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.CENTER));

		final JButton exitButton = new JButton(
			SimpleAfirmaMessages.getString("CreateHashDialog.16") //$NON-NLS-1$
		);
		exitButton.setMnemonic('C');
		exitButton.addKeyListener(this);
		exitButton.addActionListener(
			e -> {
				CreateHashFiles.this.setVisible(false);
				CreateHashFiles.this.dispose();
			}
		);
		exitButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("CreateHashDialog.17") //$NON-NLS-1$
		);

		// En Mac OS X el orden de los botones es distinto
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			panel.add(exitButton);
			panel.add(this.generateButton);
		}
		else {
			panel.add(this.generateButton);
			panel.add(exitButton);
		}

		add(label, gbc);
		gbc.gridy++;
		add(this.selectedFile, gbc);
		gbc.weightx = 0.0;
		add(this.examineButton, gbc);
		gbc.gridy++;
		add(labelAlg, gbc);
		gbc.weightx = 1.0;
		gbc.gridy++;
		add(this.hashAlgorithms, gbc);
		gbc.gridy++;
		add(this.recursive, gbc);
		gbc.gridy++;
		gbc.gridwidth = GridBagConstraints.REMAINDER;
		add(panel, gbc);
	}

	static void doHashProcess(final Frame parent,
			                  final File dir,
			                  final File outputFile,
			                  final String hashAlgorithm,
			                  final String defaultHashFormat,
			                  final boolean recursive) throws AOCancelledOperationException {

		// Se crea la ventana de espera.
		final CommonWaitDialog dialog = new CommonWaitDialog(
			parent,
			SimpleAfirmaMessages.getString("CreateHashFiles.18"), //$NON-NLS-1$
			SimpleAfirmaMessages.getString("CreateHashFiles.20") //$NON-NLS-1$
		);

		try {
			final Map<String, byte[]> hashs = calculateHashes(dir, recursive, hashAlgorithm, dialog);

			final String defaultExtension = getDefaultExtension(defaultHashFormat);

			// El fichero de huellas se almacenaria en la carpeta que eligiese el usuario.
			final File saveFile = AOUIFactory.getSaveDataToFile(
				null,
				SimpleAfirmaMessages.getString("CreateHashFiles.19"), //$NON-NLS-1$
				outputFile != null && outputFile.getParentFile() != null ?
						outputFile.getParentFile().getAbsolutePath() :
						null,
				outputFile != null ?
						outputFile.getName() :
						AutoFirmaUtil.getCanonicalFile(dir).getName() + "." + defaultExtension, //$NON-NLS-1$
				buildFilterList(defaultExtension),
				parent
			);

			// El formato del fichero guardado vendra finalmente dado por la
			// extension indicada por el usuario en el dialogo de seleccion
			String format;
			final String ext = saveFile.getName().indexOf('.') == -1 ?
					"" : //$NON-NLS-1$
					saveFile.getName().substring(saveFile.getName().lastIndexOf('.'));
			if (ext.equalsIgnoreCase("." + FILEEXT_CSV) || ext.equalsIgnoreCase("." + HashDocumentFactory.FORMAT_CSV)) { //$NON-NLS-1$ //$NON-NLS-2$
//				format = HashDocumentFactory.FORMAT_CSV;
				format = HashDocumentFactory.FORMAT_XML;
			}
			else if (ext.equalsIgnoreCase("." + FILEEXT_TXT) || ext.equalsIgnoreCase("." + HashDocumentFactory.FORMAT_TXT)) { //$NON-NLS-1$ //$NON-NLS-2$
				format = HashDocumentFactory.FORMAT_TXT;
			}
			else {
				format = HashDocumentFactory.FORMAT_XML;
			}

			// Configuramos el documento de hashes
			final HashDocument hashDocument = HashDocumentFactory.getHashDocument(format);
			hashDocument.setHashes(hashs);
			hashDocument.setRecursive(recursive);
			hashDocument.setAlgorithm(hashAlgorithm);
			hashDocument.setCharset(StandardCharsets.UTF_8);

			final byte[] dataToSave = hashDocument.generate();

            try (final OutputStream fos = new FileOutputStream(saveFile);) {
                fos.write(dataToSave);
            }
            catch (final Exception ex) {
                LOGGER.warning("No se pudo guardar la informacion en el fichero indicado: " + ex); //$NON-NLS-1$
                AOUIFactory.showErrorMessage(
            		parent,
            		SimpleAfirmaMessages.getString("CreateHashFiles.3"), //$NON-NLS-1$
            		SimpleAfirmaMessages.getString("CreateHashFiles.19"), //$NON-NLS-1$
                    JOptionPane.ERROR_MESSAGE
                );
            }
		}
		catch (final AOCancelledOperationException e) {
			throw e;
		}
		catch (final Exception e) {
			if (e.getCause() instanceof java.lang.OutOfMemoryError) {
				AOUIFactory.showErrorMessage(
					parent,
					SimpleAfirmaMessages.getString("CreateHashFiles.2"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("CreateHashDialog.14"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE
				);
				LOGGER.severe(
					"Fichero demasiado grande: " + e.getCause() //$NON-NLS-1$
				);
			}
			else {
				AOUIFactory.showErrorMessage(
					parent,
					SimpleAfirmaMessages.getString("CreateHashDialog.13"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("CreateHashDialog.14"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE
				);
				LOGGER.log(
					Level.SEVERE, "Error generando o guardando la huella digital", e//$NON-NLS-1$
				);
			}
		}

	}

	// ------------ Metodos listeners.
	/** Pide al usuario que seleccione un fichero. */
	void openSelectedFile() {
		setIconImage(AutoFirmaUtil.getDefaultDialogsIcon());
		final File file;
		try {
			file = AOUIFactory.getLoadFiles(
				SimpleAfirmaMessages.getString("CreateHashFiles.11"), //$NON-NLS-1$
				null,
				null,
				new String[] {},
				SimpleAfirmaMessages.getString("CreateHashDialog.7"), //$NON-NLS-1$
				true,
				false,
				AutoFirmaUtil.getDefaultDialogsIcon(),
				CreateHashFiles.this
			)[0];
		}
		catch (final AOCancelledOperationException e) {
			return;
		}
		if (!file.canRead()) {
			AOUIFactory.showErrorMessage(
				null,
				SimpleAfirmaMessages.getString("MenuValidation.6"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("MenuValidation.5"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
			return;
		}
		this.selectedFile.setText(file.getAbsolutePath());
		this.generateButton.setEnabled(true);
	}

	/**
	 * Calcula los hashes de los ficheros de un directorio.
	 * @param dir Directorio.
	 * @param recursive {@code true} para procesar los ficheros de los subdirectorios,
	 * {@code false} en caso contrario.
	 * @param hashAlgorithm Algoritmo de hash.
	 * @param waitingDialog  Di&aacute;logo de espera o {@code null} para no requerir
	 * interfaz gr&aacute;fica.
	 * @return Conjunto de referencias a los ficheros procesados y sus hashes.
	 * @throws InterruptedException Cuando no se interrumpe la operaci&oacute;n.
	 * @throws ExecutionException Cuando falla la operaci&oacute;n.
	 */
	public static Map<String, byte[]> calculateHashes(final File dir, final boolean recursive,
			final String hashAlgorithm, final Dialog waitingDialog)
					throws InterruptedException, ExecutionException {

		// Arrancamos el proceso en un hilo aparte
		final SwingWorker<Map<String, byte[]>, Void> worker = new SwingWorker<Map<String, byte[]>, Void>() {

			@Override
			protected java.util.Map<String, byte[]> doInBackground() throws Exception {

				final Map<String, byte[]> result = Collections.synchronizedMap(new HashMap<>());

				final long startTime = new Date().getTime();

				final ForkJoinPool pool = new ForkJoinPool(10);
				final RecursiveAction recursiveAction = new CreateHashAction(dir.toPath(), dir, recursive, hashAlgorithm, result, pool);
				pool.invoke(recursiveAction);
				recursiveAction.join();

				final long processTime = new Date().getTime() - startTime;
				LOGGER.info("Tiempo total de generacion en la hashes: " + processTime / 1000.0 + " seg"); //$NON-NLS-1$ //$NON-NLS-2$
				LOGGER.info("Numero de ficheros procesados: " + result.size()); //$NON-NLS-1$

				return result;
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

		return worker.get();
	}

	/**
	 * Recupera la extension de fichero asociada al formato indicado.
	 * @param hashFormat Formato en el que se guardaran los hashes.
	 * @return Extensi&oacute;n por defecto del nombre de fichero que
	 * deber&iacute;a almacenar esos hashes.
	 */
	private static String getDefaultExtension(final String hashFormat) {
		switch (hashFormat) {
		case HashDocumentFactory.FORMAT_XML:
			return FILEEXT_XML;
		case HashDocumentFactory.FORMAT_TXT:
			return FILEEXT_TXT;
		default:
			return null;
		}
	}

	/**
	 * Construye el listado de filtros de fichero permitidos para el guardado
	 * de los hashes del directorio. Se configura por defecto (el primero de la
	 * lista) el filtro correspondiente al formato del fichero de salida
	 * indicado.
	 * @param extension Extensi&oacute;n preferente de fichero.
	 * @return Listado de fitros de fichero.
	 */
	private static List<GenericFileFilter> buildFilterList(final String extension) {

		final GenericFileFilter[] filters = new GenericFileFilter[] {
				new GenericFileFilter(
					new String[] { FILEEXT_XML },
					SimpleAfirmaMessages.getString("CreateHashDialog.26") + " (*." + FILEEXT_XML + ")" //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
				),
//				new GenericFileFilter(
//					new String[] { FILEEXT_CSV },
//					SimpleAfirmaMessages.getString("CreateHashDialog.24") + " (*." + FILEEXT_CSV + ")" //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
//				),
				new GenericFileFilter(
					new String[] { FILEEXT_TXT },
					SimpleAfirmaMessages.getString("CreateHashDialog.25") + " (*." + FILEEXT_TXT + ")" //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
				)
			};

		List<GenericFileFilter> filterList;
		if (extension == null) {
			filterList = Arrays.asList(filters);
		}
		else {

			// Primero, agregamos al listado el filtro correspondiente a su formato
			filterList = new ArrayList<>();
			for (final GenericFileFilter filter : filters) {
				if (extension.equals(filter.getExtensions()[0])) {
					filterList.add(filter);
				}
			}
			// Despues agregamos los filtros correspondientes al resto de formatos
			for (final GenericFileFilter filter : filters) {
				if (!extension.equals(filter.getExtensions()[0])) {
					filterList.add(filter);
				}
			}
		}

		return filterList;
	}

	/** Obtiene el tipo de algoritmo seleccionado por el usuario.
	 * @return Algoritmo seleccionado por el usuario. */
	String getSelectedHashAlgorithm() {
		return this.hashAlgorithms.getSelectedItem().toString();
	}

	/** Indica si se encuentra seleccionado el calculo de hashes en los subdirectorios.
	 * @return {@code true} si se encuentra seleccionado, {@code false} en caso contrario. */
	boolean isRecursive() {
		return this.recursive.isSelected();
	}

	/** Obtiene el nombre del directorio seleccionado por el usuario.
	 * @return Nombre del directorio seleccionado por el usuario. */
	JTextField getFileTextField() {
		return this.selectedFile;
	}

	@Override
	public void keyTyped(final KeyEvent e) { /* Vacio */ }

	@Override
	public void keyPressed(final KeyEvent e) { /* Vacio */ }

	@Override
	public void keyReleased(final KeyEvent ke) {
		// En Mac no cerramos los dialogos con Escape
		if (ke != null && ke.getKeyCode() == KeyEvent.VK_ESCAPE && !Platform.OS.MACOSX.equals(Platform.getOS())) {
			setVisible(false);
			dispose();
		}
	}
}
