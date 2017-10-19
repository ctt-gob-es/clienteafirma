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
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Stream;

import javax.swing.AbstractButton;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingWorker;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.CommonWaitDialog;
import es.gob.afirma.standalone.ui.preferences.PreferencesManager;

/** Genera huellas digitales de directorios (conjunto de ficheros).
 * @author Juliana Marulanda. */
public final class CreateHashFiles extends JDialog implements KeyListener {

	private static final long serialVersionUID = -7224732001218823361L;
	private static final int SIZE_WAIT = 50000000; //Tamano en bytes

	private static final String[] HASH_ALGOS = new String[] {
		"SHA-256", //$NON-NLS-1$
		"SHA-1", //$NON-NLS-1$
		"SHA-384", //$NON-NLS-1$
		"SHA-512" //$NON-NLS-1$
	};
	private final JComboBox<String> hashAlgorithms = new JComboBox<>(HASH_ALGOS);
	private final JTextField selectedFile = new JTextField();
	private final JButton examineButton = new JButton();
	private final JButton generateButton = new JButton();
	private final JCheckBox recursive = new JCheckBox(
		SimpleAfirmaMessages.getString("CreateHashFiles.16") //$NON-NLS-1$
	);

	/** Fichero a evitar. */
	private final static Set<String> FILES_TO_AVOID = new HashSet<>(
		Arrays.asList(
			".fseventsd", //$NON-NLS-1$
			".Spotlight-V100", //$NON-NLS-1$
			".Trashes", //$NON-NLS-1$
			"._.Trashes", //$NON-NLS-1$
			".DS_Store", //$NON-NLS-1$
			".desktop", //$NON-NLS-1$
			"thumbs.db", //$NON-NLS-1$
			"$Recycle.Bin" //$NON-NLS-1$
		)
	);

	boolean isRecursiveSelected = false;

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

		this.recursive.addKeyListener(this);
		this.recursive.addActionListener(actionEvent -> {
			final AbstractButton abstractButton = (AbstractButton) actionEvent.getSource();
			CreateHashFiles.this.isRecursiveSelected = abstractButton.getModel().isSelected();
		}
		);
		this.recursive.setMnemonic('R');

		// Boton de generacion de la huella
		this.generateButton.setText(SimpleAfirmaMessages.getString("CreateHashDialog.4")); //$NON-NLS-1$
		this.generateButton.setMnemonic('G');
		this.generateButton.addKeyListener(this);
		this.generateButton.addActionListener(ae -> {

			doHashProcess(
				parent,
				getFileTextField().getText(),
				getSelectedHashAlgorithm(),
				CreateHashFiles.this.isRecursiveSelected
			);

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
			                  final String dir,
			                  final String hashAlgorithm,
			                  final boolean recursive) {

		// Se crea la ventana de espera.
		final CommonWaitDialog dialog = new CommonWaitDialog(
			parent,
			SimpleAfirmaMessages.getString("CreateHashFiles.18"), //$NON-NLS-1$
			SimpleAfirmaMessages.getString("CreateHashFiles.20") //$NON-NLS-1$
		);

		// Arrancamos el proceso en un hilo aparte
		final SwingWorker<Map<String, byte[]>, Void> worker = new SwingWorker<Map<String, byte[]>, Void>() {

			@Override
			protected java.util.Map<String, byte[]> doInBackground() throws Exception {
				final Map<String, byte[]> hashs = generateDirectoryHash(
					Paths.get(dir),
					recursive,
					hashAlgorithm
				);
				return hashs;
			}

			@Override
			protected void done() {
				super.done();
				dialog.dispose();
			}
		};
		worker.execute();

		if (getSize(new File(dir)) > SIZE_WAIT) {
			// Se muestra la ventana de espera
			dialog.setVisible(true);
		}

		try {

			final Map<String, byte[]> hashs = worker.get();
			final String xml = generateHashXML(
				hashs,
				hashAlgorithm,
				recursive
			);

			// El XML se almacenaria en la carpeta que eligiese el usuario.
			final String ext = SimpleAfirmaMessages.getString("CreateHashFiles.17"); //$NON-NLS-1$
			AOUIFactory.getSaveDataToFile(
				xml.getBytes(),
				SimpleAfirmaMessages.getString("CreateHashFiles.19"), //$NON-NLS-1$ ,,,
				null,
				AutoFirmaUtil.getCanonicalFile(new File(dir)).getName() + ext,
				new String[] { ext },
				SimpleAfirmaMessages.getString("CreateHashDialog.9") + " (*" + ext + ")", //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
				parent
			);
		}
		catch (final AOCancelledOperationException e) {
			// Operacion cancelada
		}
		catch (final Exception e) {
			if (e.getCause() instanceof java.lang.OutOfMemoryError) {
				AOUIFactory.showErrorMessage(
					parent,
					SimpleAfirmaMessages.getString("CreateHashFiles.2"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("CreateHashDialog.14"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE
				);
				Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
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
				Logger.getLogger("es.gob.afirma").log( //$NON-NLS-1$
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
			AOUIFactory.showErrorMessage(null, SimpleAfirmaMessages.getString("MenuValidation.6"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("MenuValidation.5"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE);
			return;
		}
		this.selectedFile.setText(file.getAbsolutePath());
		this.generateButton.setEnabled(true);
	}

	/** Genera el XML con las huellas digitales. El
	 * XML tendr&aacute; el siguiente esquema:
	 * <pre>
	 * &lt;xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema"&gt;
	 *	&lt;xs:element name="entries"&gt;
	 *		&lt;xs:complexType&gt;
	 * 			&lt;xs:sequence&gt;
	 *   			&lt;xs:element name="entry" maxOccurs="unbounded" minOccurs="0"&gt;
	 *     				&lt;xs:complexType&gt;
	 *       				&lt;xs:simpleContent&gt;
	 *         					&lt;xs:extension base="xs:string"&gt;
	 *           					&lt;xs:attribute type="xs:string" name="hash" use="required"/&gt;
	 *           					&lt;xs:attribute type="xs:string" name="name" use="required"/&gt;
	 *           					&lt;xs:attribute type="xs:string" name="hexhash"/&gt;
	 *         					&lt;\xs:extension&gt;
	 *       				&lt;\xs:simpleContent&gt;
	 *				     &lt;\xs:complexType&gt;
	 *   			&lt;\xs:element&gt;
	 *		 	&lt;\xs:sequence&gt;
	 *		 &lt;xs:attribute name="hashAlgorithm"&gt;
	 * 			&lt;xs:simpleType&gt;
	 *				&lt;xs:restriction base="xs:string"&gt;
	 * 					&lt;xs:enumeration value="SHA-1"/&gt;
	 *					&lt;xs:enumeration value="SHA-256"/&gt;
	 *					&lt;xs:enumeration value="SHA-384"/&gt;
	 *					&lt;xs:enumeration value="SHA-512"/&gt;
	 *				&lt;\xs:restriction&gt;
	 *			&lt;\xs:simpleType&gt;
	 * 		  &lt;\xs:attribute&gt;
	 * 	     &lt;xs:attribute type="xs:boolean" name="recursive" use="required"/&gt;
	 * 	   &lt;\xs:complexType&gt;
	 *	 &lt;\xs:element&gt;
	 * &lt;\xs:schema&gt;
	 * </pre>
	 * @param hashs Mapa con los nombres de fichero y sus huellas.
	 * @param algorithm Algoritmo con el que fueron generadas las huellas digitales.
	 * @param isRecursive Si se ha elegido que el recorrido por el directorio sea
	 *                    recursivo o no.
	 * @return String que contiene el XML.
	 * @throws ParserConfigurationException Si hay error en el analizador XML.
	 * @throws TransformerException Si hay error en la escritura del XML. */
	static String generateHashXML(final Map<String, byte[]> hashs,
			                      final String algorithm,
			                      final boolean isRecursive) throws ParserConfigurationException, TransformerException {

		final DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
		final DocumentBuilder docBuilder = docFactory.newDocumentBuilder();

		// Elemento raiz
		final Document doc = docBuilder.newDocument();
		final Element rootElement = doc.createElement("entries"); //$NON-NLS-1$
		doc.appendChild(rootElement);
		final Attr hashAlg = doc.createAttribute("hashAlgorithm"); //$NON-NLS-1$
		hashAlg.setValue(algorithm);
		rootElement.setAttributeNode(hashAlg);
		final Attr recursive = doc.createAttribute("recursive"); //$NON-NLS-1$
		recursive.setValue(String.valueOf(isRecursive));
		rootElement.setAttributeNode(recursive);

		final Set<String> paths = hashs.keySet();
		for (final String path : paths) {
			final byte[] hash = hashs.get(path);

			// Elemento entry
			final Element entry = doc.createElement("entry"); //$NON-NLS-1$
			rootElement.appendChild(entry);

			// Se inicializa el atributo name
			final Attr name = doc.createAttribute("name"); //$NON-NLS-1$
			name.setValue(path);
			entry.setAttributeNode(name);

			// Se inicializa el atributo hash
			final Attr hashAttribute = doc.createAttribute("hash"); //$NON-NLS-1$
			hashAttribute.setValue(Base64.encode(hash, true));
			entry.setAttributeNode(hashAttribute);

			// Se inicializa el atributo hexhash
			final Attr hexHashAttribute = doc.createAttribute("hexhash"); //$NON-NLS-1$
			hexHashAttribute.setValue(AOUtil.hexify(hash, false) + "h"); //$NON-NLS-1$
			entry.setAttributeNode(hexHashAttribute);
		}
		final StringWriter sw = new StringWriter();
		final TransformerFactory tf = TransformerFactory.newInstance();
		final Transformer transformer = tf.newTransformer();
		transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no"); //$NON-NLS-1$
		transformer.setOutputProperty(OutputKeys.METHOD, "xml"); //$NON-NLS-1$
		transformer.setOutputProperty(OutputKeys.INDENT, "yes"); //$NON-NLS-1$
		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			transformer.setOutputProperty(OutputKeys.ENCODING, "ISO-8859-1"); //$NON-NLS-1$
		}
		else {
			transformer.setOutputProperty(OutputKeys.ENCODING, StandardCharsets.UTF_8.name());
		}

		transformer.transform(
			new DOMSource(doc), new StreamResult(sw)
		);

		return sw.toString();

	}

	/** Obtiene un informe de huellas en XML del directorio indicado.
	 * @param dir Directorio sobre cuyos ficheros se desea calcular las huellas.
	 * @param isRecursive Si se desea se recorra el directorio recursivamente.
	 * @param algorithm Algoritmo para las huellas digitales.
	 * @return Informe XML de huellas.
	 * @throws NoSuchAlgorithmException Si no se soporta el algoritmo de huellas.
	 * @throws IOException Si hay problemas en el tratamiento de los ficheros.
	 * @throws ParserConfigurationException Si hay problemas con el analizador XML.
	 * @throws TransformerException Si hay problemas escribiendo el XML. */
	public static String getHashReport(final String dir, final boolean isRecursive, final String algorithm) throws NoSuchAlgorithmException,
	                                                                                                      IOException,
	                                                                                                      ParserConfigurationException,
	                                                                                                      TransformerException {
		final Map<String, byte[]> retMap = generateDirectoryHash(
			Paths.get(new File(dir).toURI()),
			isRecursive,
			algorithm
		);
		return generateHashXML(retMap, algorithm, isRecursive);
	}

	/** Genera la huella digital de los ficheros de un directorio.
	 * @param dir Directorio que ha elegido el usuario para generar la firma
	 *            digital.
	 * @param isRecursive Si se ha elegido que el recorrido por el directorio sea
	 *                    recursivo o no.
	 * @param algorithm Algoritmo con el que fue generada la huella digital.
	 * @return Valores hash para cada elemento del directorio.
	 * @throws IOException Error al abrir o cerrar el fichero seleccionado.
	 * @throws NoSuchAlgorithmException Si no se soporta el algoritmo de huella. */
	static Map<String, byte[]> generateDirectoryHash(final Path dir,
			                                         final boolean isRecursive,
			                                         final String algorithm) throws IOException,
			                                                                        NoSuchAlgorithmException {
		final Map<String, byte[]> directoryHash = new HashMap<>();
		if (Files.exists(dir)) {
			if (isRecursive) {
				directoryHash.putAll(
					recursiveDirectoryHash(null, dir, directoryHash, algorithm)
				);
			}
			else {
				try (Stream<Path> paths = Files.list(dir)) {
					final Iterator<Path> it = paths.iterator();
					while (it.hasNext()) {
						final Path path = it.next();
						if (!Files.isDirectory(path) &&
							!path.getFileName().toString().contains("~$") && //$NON-NLS-1$
							!FILES_TO_AVOID.contains(path.getFileName().toString())) {
								directoryHash.put(
									path.getFileName().toString(),
									HashUtil.getFileHash(algorithm, path)
								);
						}
					}
				}
			}
		}
		return directoryHash;
	}

	/** Recorre el directorio recursivamente.
	 * @param relativePath Nombre relativo del directorio.
	 * @param path Directorio a recorrer recursivamente.
	 * @param directoryHash Contiene el nombre del fichero y su respecitiva firma digital.
	 * @param algorithm Algoritmo para las huellas digitales.
	 * @return Mapa con los valores de huella para cada elemento del directorio.
	 * @throws IOException Indica si ha habido un error al abrir o cerrar un fichero.
	 * @throws NoSuchAlgorithmException Si no se soporta el algoritmo de huella. */
	private static Map<String, byte[]> recursiveDirectoryHash(final String relativePath,
			                                                  final Path path,
			                                                  final Map<String, byte[]> directoryHash,
			                                                  final String algorithm) throws IOException, NoSuchAlgorithmException {
		if (Files.exists(path)) {
			if (Files.isDirectory(path)) {
				try (Stream<Path> paths = Files.list(path)) {
					final Iterator<Path> it = paths.iterator();
					while (it.hasNext()) {
						final Path subPath = it.next();
						if (relativePath != null) {
							directoryHash.putAll(
								recursiveDirectoryHash(
									relativePath + File.separator + subPath.getFileName().toString(),
									subPath,
									directoryHash,
									algorithm
								)
							);
						}
						else {
							directoryHash.putAll(
								recursiveDirectoryHash(
									subPath.getFileName().toString(), subPath, directoryHash, algorithm
								)
							);
						}
					}
				} // Si no es un directorio
			}
			else {
				if (!relativePath.contains("~$") && !FILES_TO_AVOID.contains(path.getFileName().toString())) { //$NON-NLS-1$
					directoryHash.put(
						relativePath,
						HashUtil.getFileHash(algorithm, path)
					);
				}
			}
		}
		return directoryHash;
	}

	static long getSize(final File file) {
	    long size;
	    if (file.isDirectory()) {
	        size = 0;
	        for (final File child : file.listFiles()) {
	            size += getSize(child);
	        }
	    } else {
	        size = file.length();
	    }
	    return size;
	}


	// ------- Fin.

	// ---------------- Metodos get.
	/**
	 * Obtiene el tipo de algoritmo seleccionado por el usuario.
	 * @return El algoritmo seleccionado por el usuario.
	 */
	String getSelectedHashAlgorithm() {
		return this.hashAlgorithms.getSelectedItem().toString();
	}

	/**
	 * Obtiene el nombre del directorio seleccionado por el usuario.
	 * @return El nombre del directorio seleccionado por el usuario
	 */
	JTextField getFileTextField() {
		return this.selectedFile;
	}
	// ------- Fin metodos get.

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
