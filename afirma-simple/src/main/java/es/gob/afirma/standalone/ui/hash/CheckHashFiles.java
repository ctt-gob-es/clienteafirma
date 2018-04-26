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
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;
import java.util.stream.Stream;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingWorker;
import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.ui.GenericFileFilter;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.CommonWaitDialog;

/** Di&aacute;logo para la comprobaci&oacute;n de huellas digitales en
 * directorios.
 * @author Juliana Marulanda. */
public final class CheckHashFiles extends JDialog implements KeyListener {

	private static final long serialVersionUID = 5969239673119761747L;
	private final JTextField xml = new JTextField();
	private final JTextField directory = new JTextField();
	private static Map<String, List<String>> reportXML;
	private static String algorithm = null;
	private static Boolean isRecursive = null;
	private static final int SIZE_WAIT = 50000000; //Tamano en bytes


	/** Fichero a evitar. */
	private final static Set<String> FILES_TO_AVOID	= new HashSet<>(
		Arrays.asList(
			".fseventsd",		//$NON-NLS-1$
			".Spotlight-V100",	//$NON-NLS-1$
			".Trashes",			//$NON-NLS-1$
			"._.Trashes",		//$NON-NLS-1$
			".DS_Store",		//$NON-NLS-1$
			".desktop",			//$NON-NLS-1$
			"thumbs.db",		//$NON-NLS-1$
			"$Recycle.Bin"		//$NON-NLS-1$
		)
	);

	/** Inicia el proceso de comprobaci&oacute;n de huella digital de
	 * directorios.
	 * @param parent Componente padre para la modalidad. */
	public static void startHashCheck(final Frame parent) {
		final CheckHashFiles chfd = new CheckHashFiles(parent);
		chfd.setResizable(false);
		chfd.setSize(600, 250);
		chfd.setLocationRelativeTo(parent);
		chfd.setVisible(true);
	}

	/** Crea un di&aacute;logo para la comprobaci&oacute;n de huellas digitales
	 * de directorios.
	 * @param parent Componente padre para la modalidad. */
	private CheckHashFiles(final Frame parent) {
		super(parent);
		setTitle(SimpleAfirmaMessages.getString("CheckHashFiles.0")); //$NON-NLS-1$
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
		gbc.insets = new Insets(15, 10, 5, 10);
		setLocationRelativeTo(parent);
		setIconImage(
			AutoFirmaUtil.getDefaultDialogsIcon()
		);
		setTitle(SimpleAfirmaMessages.getString("CheckHashFiles.0")); //$NON-NLS-1$

		final JButton checkButton = new JButton(SimpleAfirmaMessages.getString("CheckHashDialog.1")); //$NON-NLS-1$
		checkButton.setMnemonic('R');
		checkButton.setEnabled(false);
		checkButton.addKeyListener(this);
		checkButton.addActionListener(
			e -> {
				try {
					// Se crea la ventana de espera.
					final CommonWaitDialog dialog = new CommonWaitDialog(
						parent,
						SimpleAfirmaMessages.getString("CreateHashFiles.21"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("CreateHashFiles.22") //$NON-NLS-1$
					);

					// Arrancamos el proceso en un hilo aparte
					final SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {

						@Override
						protected Void doInBackground() throws Exception {
							setReportXML(new HashMap<String, List<String>>());
							checkHashXML(Paths.get(getDirectorioText()), getXMLText());
							return null;
						}
						@Override
						protected void done() {
							super.done();
							dialog.dispose();
						}
					};
					worker.execute();

					if (CreateHashFiles.getSize(new File(getDirectorioText().toString())) > SIZE_WAIT) {
						// Se muestra la ventana de espera
						dialog.setVisible(true);
					}

					worker.get();
					if (!(getReportXML().containsKey("CheckHashDialog.5") || //$NON-NLS-1$
						  getReportXML().containsKey("CheckHashFiles.1") || //$NON-NLS-1$
						  getReportXML().containsKey("CheckHashFiles.10"))) { //$NON-NLS-1$
								AOUIFactory.showMessageDialog(
									CheckHashFiles.this,
									SimpleAfirmaMessages.getString("CheckHashDialog.2"), //$NON-NLS-1$
									SimpleAfirmaMessages.getString("CheckHashDialog.3"), //$NON-NLS-1$
									JOptionPane.INFORMATION_MESSAGE
								);
					}
					else {
						AOUIFactory.showMessageDialog(
							CheckHashFiles.this, SimpleAfirmaMessages.getString("CheckHashFiles.18"), //$NON-NLS-1$
							SimpleAfirmaMessages.getString("CheckHashFiles.17"), //$NON-NLS-1$
							JOptionPane.WARNING_MESSAGE
						);
					}
					final String ext = SimpleAfirmaMessages.getString("CheckHashFiles.20"); //$NON-NLS-1$
					AOUIFactory.getSaveDataToFile(
						generateXMLReport(
							getReportXML(),
							getAlgorithm(),
							getIsRecursive()
						).getBytes(),
						SimpleAfirmaMessages.getString("CheckHashFiles.15"), //$NON-NLS-1$ ,,,
						null,
						new java.io.File(SimpleAfirmaMessages.getString("CheckHashFiles.16")).getName() + ext, //$NON-NLS-1$
						Collections.singletonList(
							new GenericFileFilter(
								new String[] { ext },
								SimpleAfirmaMessages.getString("CheckHashFiles.11") + " (*" + ext + ")" //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
							)
						),
						parent
					);
					checkButton.setEnabled(false);
					CheckHashFiles.this.setVisible(false);
					CheckHashFiles.this.dispose();
				}
				catch (final OutOfMemoryError ooe) {
					AOUIFactory.showErrorMessage(
						parent,
						SimpleAfirmaMessages.getString("CreateHashFiles.2"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("CreateHashDialog.14"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE
					);
					Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
						"Fichero demasiado grande: " + ooe //$NON-NLS-1$
					);
				}
				catch (final AOCancelledOperationException ex1) {
					// Operacion cancelada por el usuario
				}
				catch (final Exception ex2) {
					Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
						"No ha sido posible comprobar las huellas digitales: " + ex2 //$NON-NLS-1$
					);
					AOUIFactory.showErrorMessage(
						CheckHashFiles.this,
						SimpleAfirmaMessages.getString("CheckHashDialog.6"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("CheckHashDialog.7"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE
					);
				}
			}
		);
		checkButton.setEnabled(false);

		this.xml.setEditable(false);
		this.xml.setFocusable(false);
		this.xml.setColumns(10);
		this.xml.addKeyListener(this);

		final JButton xmlExamineButton = new JButton(
			SimpleAfirmaMessages.getString("CreateHashDialog.5") //$NON-NLS-1$
		);
		xmlExamineButton.setMnemonic('E');
		xmlExamineButton.setEnabled(true);
		xmlExamineButton.addKeyListener(this);
		xmlExamineButton.addActionListener(e -> {
			try {
				setXMLText(
					AOUIFactory.getLoadFiles(
						SimpleAfirmaMessages.getString("CreateHashDialog.5"), //$NON-NLS-1$
						null,
						null,
						new String[] { "hashfiles" }, //$NON-NLS-1$
						SimpleAfirmaMessages.getString("CheckHashFiles.19"), //$NON-NLS-1$
						false,
						false,
						AutoFirmaUtil.getDefaultDialogsIcon(),
						CheckHashFiles.this
					)[0].getAbsolutePath()
				);
				final String hashFile = getXMLText();
				if (!(hashFile == null) && !hashFile.isEmpty()) {
					checkButton.setEnabled(true);
				}
			}
			catch (final AOCancelledOperationException ex) {
				// Operacion cancelada por el usuario
			}
		}
		);

		this.directory.setEditable(false);
		this.directory.setFocusable(false);
		this.directory.setColumns(10);
		this.directory.addKeyListener(this);

		final JButton directoryExamineButton = new JButton(
			SimpleAfirmaMessages.getString("CreateHashDialog.5") //$NON-NLS-1$
		);
		directoryExamineButton.setMnemonic('X');
		directoryExamineButton.setEnabled(true);
		directoryExamineButton.addKeyListener(this);
		directoryExamineButton.addActionListener(e -> {
			try {
				setDirectorioText(
					AOUIFactory.getLoadFiles(
						SimpleAfirmaMessages.getString("CreateHashDialog.5"), //$NON-NLS-1$
						null,
						null,
						null,
						SimpleAfirmaMessages.getString("CheckHashDialog.14"), //$NON-NLS-1$
						true,
						false,
						AutoFirmaUtil.getDefaultDialogsIcon(),
						CheckHashFiles.this
					)[0].getAbsolutePath()
				);
				final String hashFile = getXMLText();
				if (!(hashFile == null) && !hashFile.isEmpty()) {
					checkButton.setEnabled(true);
				}
			}
			catch (final AOCancelledOperationException ex) {
				// Operacion cancelada por el usuario
			}
		}
		);

		final JButton exitButton = new JButton(SimpleAfirmaMessages.getString("CheckHashFiles.12") //$NON-NLS-1$
		);

		if (!getXMLText().isEmpty() && !getDirectorioText().isEmpty()) {
			checkButton.setEnabled(true);
		}
		exitButton.setMnemonic('C');
		exitButton.addKeyListener(this);
		exitButton.addActionListener(
			e -> {
				CheckHashFiles.this.setVisible(false);
				CheckHashFiles.this.dispose();
			}
		);
		exitButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("CreateHashDialog.17") //$NON-NLS-1$
		);

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.CENTER));

		// En Mac OS X el orden de los botones es distinto
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			panel.add(exitButton);
			panel.add(checkButton);
		}
		else {
			panel.add(checkButton);
			panel.add(exitButton);
		}

		final JLabel directoryLabel = new JLabel(
			SimpleAfirmaMessages.getString("CheckHashFiles.13") //$NON-NLS-1$
		);
		directoryLabel.setLabelFor(this.directory);
		final JLabel xmlLabel = new JLabel(SimpleAfirmaMessages.getString("CheckHashFiles.14")); //$NON-NLS-1$
		xmlLabel.setLabelFor(this.xml);

		c.add(directoryLabel, gbc);
		gbc.insets = new Insets(0, 10, 0, 10);
		gbc.gridy++;
		c.add(this.directory, gbc);
		gbc.weightx = 0.0;
		c.add(directoryExamineButton, gbc);
		gbc.insets = new Insets(15, 10, 5, 10);
		gbc.gridy++;
		gbc.weightx = 1.0;
		c.add(xmlLabel, gbc);
		gbc.insets = new Insets(0, 10, 0, 10);
		gbc.gridy++;
		c.add(this.xml, gbc);
		gbc.weightx = 0.0;
		c.add(xmlExamineButton, gbc);
		gbc.insets = new Insets(15, 10, 5, 10);
		gbc.gridy++;
		gbc.gridwidth = GridBagConstraints.REMAINDER;
		c.add(panel, gbc);
	}

	/** Comprueba que las huellas digitales generadas para el directorio sean
	 * iguales que las almacenadas en el XML generado.
	 * @param file Directorio a comparar.
	 * @param expectedFileHash Fichero con el XML que almacena todas las huellas
	 *        digitales
	 *        del directorio.
	 * @param alg Algoritmo de huella con el que fueron generadas las huellas del XML.
	 * @return Si se ha podido realizar la comprobaci&oacute;n entre el fichero
	 *         seleccionado y la huella digital.
	 * @throws IOException Error al abrir o cerrar el fichero seleccionado.
	 * @throws NoSuchAlgorithmException Si no se soporta el algoritmo de huella.
	 * @throws FileNotFoundException Si el fichero seleccionado no
	 *                               existe. */
	static boolean checkFileHash(final Path file,
			                     final byte[] expectedFileHash,
			                     final String alg) throws FileNotFoundException,
			                                              NoSuchAlgorithmException,
			                                              IOException {

		final byte[] newHash = HashUtil.getFileHash(alg, file);
		return Arrays.equals(newHash, expectedFileHash);
	}

	/** Comprueba las huellas digitales de los ficheros de un directorio.
	 * @param dir Directorio a comparar con el XML.
	 * @param isRec <code>true</code> si se recorri&oacute; el directorio de forma recursiva,
	 *              <code>false</code> en caso contrario.
	 * @param directoryHash Mapa con los nombres de fichero analizados y sus huellas digitales.
	 * @param alg Algoritmo con el que fue generada la huella digital.
	 * @throws NoSuchAlgorithmException Si no se soporta el algoritmo de huella.
	 * @throws IOException Error en el tratamiento de los ficheros. */
	private static void checkDirectoryHash(final Path dir,
			                               final boolean isRec,
			                               final Map<String, byte[]> directoryHash,
			                               final String alg) throws IOException,
			                                                        NoSuchAlgorithmException {
		if (Files.exists(dir)) {
			if (isRec) {
				checkDirectoryRecursively(null, dir, directoryHash, alg);
			}
			else {
				checkDirectoryNonRecursively(dir, directoryHash, alg);
			}
		}
	}

	private static void checkDirectoryNonRecursively(final Path dir,
                                                     final Map<String, byte[]> directoryHash,
                                                     final String alg) throws IOException, NoSuchAlgorithmException {
		String messageCode = null;
		final String auxMessageCode = "CheckHashDialog.3"; //$NON-NLS-1$
		try (Stream<Path> paths = Files.list(dir)) {
			final Iterator<Path> it = paths.iterator();
			while (it.hasNext()) {
				final Path path = it.next();
				if (!Files.isDirectory(path)) {
					if (!FILES_TO_AVOID.contains(path.getFileName().toString()) && !path.getFileName().toString().contains("~$")) { //$NON-NLS-1$

						if (directoryHash.containsKey(path.getFileName().toString())) {
							if (!checkFileHash(path, directoryHash.get(path.getFileName().toString()), alg)) {
								messageCode = "CheckHashDialog.5"; //$NON-NLS-1$
								List<String> filenameList = getReportXML().get(messageCode);
								if (filenameList == null) {
									filenameList = new ArrayList<>();
									getReportXML().put(messageCode, filenameList);
								}
								getReportXML().get(messageCode).add(path.getFileName().toString());
							}
							else {
								List<String> filenameList = getReportXML().get(auxMessageCode);
								if (filenameList == null) {
									filenameList = new ArrayList<>();
									getReportXML().put(auxMessageCode, filenameList);
								}
								getReportXML().get(auxMessageCode).add(path.getFileName().toString());
							}
							directoryHash.remove(path.getFileName().toString());
						}
						else {
							messageCode = "CheckHashFiles.10"; //$NON-NLS-1$
							List<String> filenameList = getReportXML().get(messageCode);
							if (filenameList == null) {
								filenameList = new ArrayList<>();
								getReportXML().put(messageCode, filenameList);
							}
							getReportXML().get(messageCode).add(path.getFileName().toString());
						}
					}

				} // if (!Files.isDirectory(path))
			} // while
		}
	}

	private static void checkDirectoryRecursively(final String relativePath,
			                                      final Path dir,
			                                      final Map<String, byte[]> directoryHash,
			                                      final String alg) throws IOException, NoSuchAlgorithmException {

		final String auxMessageCode = "CheckHashDialog.3"; //$NON-NLS-1$
		String messageCode = null;
		if (Files.exists(dir)) {
			if (Files.isDirectory(dir)) {
				try (Stream<Path> paths = Files.list(dir)) {
					final Iterator<Path> it = paths.iterator();
					while (it.hasNext()) {
						final Path path = it.next();
						if (relativePath != null) {
							checkDirectoryRecursively(
								relativePath + File.separator + path.getFileName().toString(),
								path,
								directoryHash,
								alg
							);
						}
						else {
							checkDirectoryRecursively(path.getFileName().toString(), path, directoryHash, alg);
						}
					}
				} // No es un directorio
			}
			else {

				if (!FILES_TO_AVOID.contains(dir.getFileName().toString()) && !relativePath.contains("~$")) { //$NON-NLS-1$
					if (directoryHash.containsKey(relativePath)) {
						if (!checkFileHash(dir, directoryHash.get(relativePath), getAlgorithm())) {
							messageCode = "CheckHashDialog.5"; //$NON-NLS-1$
							List<String> filenameList = getReportXML().get(messageCode);
							if (filenameList == null) {
								filenameList = new ArrayList<>();
								getReportXML().put(messageCode, filenameList);
							}
							getReportXML().get(messageCode).add(relativePath);
						}
						else {
							List<String> filenameList = getReportXML().get(auxMessageCode);
							if (filenameList == null) {
								filenameList = new ArrayList<>();
								getReportXML().put(auxMessageCode, filenameList);
							}
							getReportXML().get(auxMessageCode).add(relativePath);
						}
						directoryHash.remove(relativePath);
					}
					else {
						messageCode = "CheckHashFiles.10"; //$NON-NLS-1$
						List<String> filenameList = getReportXML().get(messageCode);
						if (filenameList == null) {
							filenameList = new ArrayList<>();
							getReportXML().put(messageCode, filenameList);
						}
						getReportXML().get(messageCode).add(relativePath);
					}
				}
			}
		}
	}

	/** Recorre el fichero XML seleccionado por el usuario.
	 * @param dir Directorio seleccionado.
	 * @param xmlPath Fichero que cotiene el XML.
	 * @throws IOException Error al abrir o cerrar el fichero seleccionado.
	 * @throws SAXException Error al analizar el XML.
	 * @throws ParserConfigurationException Error al procesar un documento de tipo XML.
	 * @throws NoSuchAlgorithmException Error al construir la implementacion de un algoritmo. */
	static void checkHashXML(final Path dir, final String xmlPath) throws SAXException,
	                                                               IOException,
	                                                               ParserConfigurationException,
	                                                               NoSuchAlgorithmException {

		final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		final DocumentBuilder builder = factory.newDocumentBuilder();

		try (InputStream is = new FileInputStream(xmlPath)) {
			validateAgainstXSD(is, CheckHashFiles.class.getResourceAsStream("/schemas/folderhashes.xsd")); //$NON-NLS-1$
		}
		final Document doc = builder.parse(xmlPath);
		doc.getDocumentElement().normalize();

		final Map<String, byte[]> dirEntries = new HashMap<>();

		final NodeList nodeListEntries = doc.getElementsByTagName("entries"); //$NON-NLS-1$
		final Node nodeEntries = nodeListEntries.item(0);
		setAlgorithm(nodeEntries.getAttributes().getNamedItem("hashAlgorithm").getNodeValue()); //$NON-NLS-1$
		setIsRecursive(
			Boolean.valueOf(nodeEntries.getAttributes().getNamedItem("recursive").getNodeValue()) //$NON-NLS-1$
		);

		final NodeList nodeList = doc.getElementsByTagName("entry"); //$NON-NLS-1$
		for (int i = 0; i < nodeList.getLength(); i++) {
			final Node node = nodeList.item(i);

			final byte[] hash = Base64.decode(
				node.getAttributes().getNamedItem("hash").getNodeValue(), true //$NON-NLS-1$
			);
			final String name = node.getAttributes().getNamedItem("name").getNodeValue(); //$NON-NLS-1$
			dirEntries.put(name, hash);
		}
		checkDirectoryHash(dir, getIsRecursive(), dirEntries, getAlgorithm());

		if (dirEntries.keySet().size() > 0) {
			final String messageCode = "CheckHashFiles.1"; //$NON-NLS-1$
			List<String> filenameList = getReportXML().get(messageCode);
			if (filenameList == null) {
				filenameList = new ArrayList<>();
				getReportXML().put(messageCode, filenameList);
			}

			for (final String pathName : dirEntries.keySet()) {
				getReportXML().get(messageCode).add(pathName);
			}
		}
	}

	private static void validateAgainstXSD(final InputStream isxml,
			                               final InputStream xsd) throws SAXException, IOException {

		final SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
		final Schema schema = factory.newSchema(new StreamSource(xsd));
		final Validator validator = schema.newValidator();
		validator.validate(new StreamSource(isxml));
	}

	/** Genera el XML donde est&aacute;n almacenadas las comparaciones de las
	 * huellas digitales de los ficheros elegidos.
	 * El XML tendr&aacute; el siguiente esquema:
	 * <pre>
	 * &lt;xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema"&gt;
	 *	&lt;xs:element name="entries"&gt;
	 *		&lt;xs:complexType&gt;
	 * 			&lt;xs:sequence&gt;
	 *   			&lt;xs:element name="file_without_hash"&gt;
	 * 				    &lt;xs:complexType&gt;
	 *				       &lt;xs:sequence&gt;
	 *				         &lt;xs:element name="entry"&gt;
	 *				           &lt;xs:complexType&gt;
	 *				             &lt;xs:simpleContent&gt;
	 *				               &lt;xs:extension base="xs:string"&gt;
	 *				                 &lt;xs:attribute type="xs:string" name="name"/&gt;
	 *				               &lt;/xs:extension&gt;
	 *				             &lt;/xs:simpleContent&gt;
	 *				           &lt;/xs:complexType&gt;
	 *				         &lt;/xs:element&gt;
	 *				       &lt;/xs:sequence&gt;
	 *				     &lt;/xs:complexType&gt;
	 *				   &lt;/xs:element&gt;
	 *				   &lt;xs:element name="matching_hash"&gt;
	 *				     &lt;xs:complexType&gt;
	 *				       &lt;xs:sequence&gt;
	 *				         &lt;xs:element name="entry" maxOccurs="unbounded" minOccurs="0"&gt;
	 *				           &lt;xs:complexType&gt;
	 *				             &lt;xs:simpleContent&gt;
	 *				               &lt;xs:extension base="xs:string"&gt;
	 *				                 &lt;xs:attribute type="xs:string" name="name" use="optional"/&gt;
	 *				               &lt;/xs:extension&gt;
	 *				             &lt;/xs:simpleContent&gt;
	 *				           &lt;/xs:complexType&gt;
	 *				         &lt;/xs:element&gt;
	 *				       &lt;/xs:sequence&gt;
	 *  			    &lt;/xs:complexType&gt;
	 *   	   		  &lt;/xs:element&gt;
	 *			    &lt;xs:element name="hash_without_file"&gt;
	 *		 	     &lt;xs:complexType&gt;
	 *			       &lt;xs:sequence&gt;
	 *			         &lt;xs:element name="entry" maxOccurs="unbounded" minOccurs="0"&gt;
	 *			           &lt;xs:complexType&gt;
	 *			             &lt;xs:simpleContent&gt;
	 *			               &lt;xs:extension base="xs:string"&gt;
	 *			                 &lt;xs:attribute type="xs:string" name="name" use="optional"/&gt;
	 *			               &lt;/xs:extension&gt;
	 *			             &lt;/xs:simpleContent&gt;
	 *			           &lt;/xs:complexType&gt;
	 *			         &lt;/xs:element&gt;
	 *			       &lt;/xs:sequence&gt;
	 *			     &lt;/xs:complexType&gt;
	 *			   &lt;/xs:element&gt;
	 *			   &lt;xs:element name="not_matching_hash"&gt;
	 *			     &lt;xs:complexType&gt;
	 *			       &lt;xs:sequence&gt;
	 *			         &lt;xs:element name="entry"&gt;
	 *			           &lt;xs:complexType&gt;
	 *			             &lt;xs:simpleContent&gt;
	 *			               &lt;xs:extension base="xs:string"&gt;
	 *			                 &lt;xs:attribute type="xs:string" name="name"/&gt;
	 *			               &lt;/xs:extension&gt;
	 *			             &lt;/xs:simpleContent&gt;
	 *			           &lt;/xs:complexType&gt;
	 *			         &lt;/xs:element&gt;
	 *			       &lt;/xs:sequence&gt;
	 *			     &lt;/xs:complexType&gt;
	 *			   &lt;/xs:element&gt;
	 *			 &lt;/xs:sequence&gt;
	 *		   &lt;xs:attribute type="xs:string" name="hashAlgorithm"/&gt;
	 *		 &lt;xs:attribute type="xs:string" name="recursive"/&gt;
	 *		&lt;/xs:complexType&gt;
	 *	  &lt;/xs:element&gt;
	 *	&lt;/xs:schema&gt;
	 * </pre>
	 * @param mapReport Mapa con la informacion para generar el report el XML.
	 * @param alg Algoritmo con el que fue generado el XML.
	 * @param isRec Si se ha elegido que el recorrido por el directorio sea
	 *        recursivo o no.
	 * @return Informe de coincidencias de huellas en XML.
	 * @throws ParserConfigurationException Si no se puede obtener en analizador XML.
	 * @throws TransformerException Si hay errores escribiendo el XML. */
	static String generateXMLReport(final Map<String, List<String>> mapReport, final String alg, final boolean isRec) throws ParserConfigurationException,
	                                                                                                                         TransformerException {

		final DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
		final DocumentBuilder docBuilder = docFactory.newDocumentBuilder();

		// Elemento raiz
		final Document doc = docBuilder.newDocument();
		final Element rootElement = doc.createElement("entries"); //$NON-NLS-1$
		final Attr hashAlg = doc.createAttribute("hashAlgorithm"); //$NON-NLS-1$
		hashAlg.setValue(alg);
		rootElement.setAttributeNode(hashAlg);
		final Attr recursive = doc.createAttribute("recursive"); //$NON-NLS-1$
		recursive.setValue(String.valueOf(isRec));
		rootElement.setAttributeNode(recursive);
		doc.appendChild(rootElement);

		for (final String code : mapReport.keySet()) {
			Element entriesHeader = doc.createElement("matching_hash"); //$NON-NLS-1$
			// Elemento header
			if (code.equals("CheckHashDialog.5")) {//$NON-NLS-1$
				entriesHeader = doc.createElement("not_matching_hash"); //$NON-NLS-1$
			}
			else if (code.equals("CheckHashFiles.1")) {//$NON-NLS-1$
				entriesHeader = doc.createElement("hash_without_file"); //$NON-NLS-1$
			}
			else if (code.equals("CheckHashFiles.10")) {//$NON-NLS-1$
				entriesHeader = doc.createElement("file_without_hash"); //$NON-NLS-1$
			}
			rootElement.appendChild(entriesHeader);
			for (final String path : mapReport.get(code)) {
				// Elemento entry
				final Element entry = doc.createElement("entry"); //$NON-NLS-1$
				entriesHeader.appendChild(entry);
				// Se inicializa el atributo name
				final Attr name = doc.createAttribute("name"); //$NON-NLS-1$
				name.setValue(path);
				entry.setAttributeNode(name);
			}
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

		transformer.transform(new DOMSource(doc), new StreamResult(sw));
		return sw.toString();

	}

	/** Obtiene el nombre del fichero seleccionado por el usuario.
	 * @return Nombre del fichero seleccionado por el usuario. */
	String getXMLText() {
		return this.xml.getText();
	}

	/** Establece el nombre del fichero seleccionado por el usuario.
	 * @param text Nombre del fichero seleccionado por el usuario. */
	void setXMLText(final String text) {
		this.xml.setText(text);
	}

	/** Obtiene el nombre del fichero con la huella digital seleccionada por el
	 * usuario.
	 * @return Nombre del fichero con la huella digital seleccionada por el
	 *         usuario. */
	String getDirectorioText() {
		return this.directory.getText();
	}

	/** Establece el nombre del fichero con la huella digital seleccionada por
	 * el usuario.
	 * @param text Nombre de la huella digital seleccionada por el usuario. */
	void setDirectorioText(final String text) {
		this.directory.setText(text);
	}

	/** Indica si se ha establecido el proceso recursivo.
	 * @return <code>true</code> si se ha establecido el proceso recursivo,
	 *         <code>false</code> en caso contrario. */
	static boolean getIsRecursive() {
		return isRecursive.booleanValue();
	}

	/** Establece el modo recursivo.
	 * @param isRecursive <code>true</code> si se desea un proceso recursivo de los directorios,
	 *         <code>false</code> en caso contrario. */
	static void setIsRecursive(final Boolean isRecursive) {
		CheckHashFiles.isRecursive = isRecursive;
	}

	/** Obtiene los datos del informe de huellas.
	 * @return Datos del informe de huellas. */
	static Map<String, List<String>> getReportXML() {
		return reportXML;
	}

	/** Establece los datos del informe de huellas.
	 * @param reportXML Datos del informe de huellas. */
	static void setReportXML(final Map<String, List<String>> reportXML) {
		CheckHashFiles.reportXML = reportXML;
	}

	/** Obtiene el algoritmo elegido para generar la huella digital.
	 * @return Valor del algoritmo elegido para generar la huella digital. */
	static String getAlgorithm() {
		return algorithm;
	}

	/** Establece el algoritmo elegido para generar la huella digital.
	 * @param algorithm Algoritmo elegido para generar la huella digital. */
	static void setAlgorithm(final String algorithm) {
		CheckHashFiles.algorithm = algorithm;
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
