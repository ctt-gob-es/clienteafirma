/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.plugin.hash;

import java.awt.Component;
import java.awt.Container;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.StringWriter;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.RecursiveAction;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JButton;
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
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.plugins.UIFactory;

/** Di&aacute;logo para la comprobaci&oacute;n de huellas digitales en
 * directorios.
 * @author Juliana Marulanda. */
public final class CheckHashDirDialog extends JDialog implements KeyListener {

	private static final long serialVersionUID = 5969239673119761747L;

	private static final Logger LOGGER = Logger.getLogger(CheckHashDirDialog.class.getName());

	private final JTextField reportTextField = new JTextField();
	private final JTextField directoryTextField = new JTextField();


	/** Inicia el proceso de comprobaci&oacute;n de huella digital de
	 * directorios.
	 * @param parent Componente padre para la modalidad. */
	public static void startHashCheck(final Window parent) {
		final CheckHashDirDialog chfd = new CheckHashDirDialog(parent);
		chfd.setResizable(false);
		chfd.setSize(600, 250);
		chfd.setLocationRelativeTo(parent);
		chfd.setVisible(true);
	}

	/** Crea un di&aacute;logo para la comprobaci&oacute;n de huellas digitales
	 * de directorios.
	 * @param parent Componente padre para la modalidad. */
	private CheckHashDirDialog(final Window parent) {
		super(parent);
		setTitle(Messages.getString("CheckHashFiles.0")); //$NON-NLS-1$
		setModalityType(ModalityType.APPLICATION_MODAL);
		createUI(parent);
	}

	/** Crea todos los elementos necesarios para generar una huella digital de
	 * directorios.
	 * @param parent Componente padre para la modalidad. */
	void createUI(final Component parent) {

		final Container c = getContentPane();
		final GridBagLayout gbl = new GridBagLayout();
		c.setLayout(gbl);
		final GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.weightx = 1.0;
		gbc.gridy = 0;
		gbc.insets = new Insets(15, 10, 5, 10);
		setLocationRelativeTo(parent);

		final Image icon = getDialogIcon();
		setIconImage(icon);
		setTitle(Messages.getString("CheckHashFiles.0")); //$NON-NLS-1$

		final JButton checkButton = new JButton(Messages.getString("CheckHashDialog.1")); //$NON-NLS-1$
		checkButton.setMnemonic('R');
		checkButton.setEnabled(false);
		checkButton.addKeyListener(this);
		checkButton.addActionListener(
			e -> {
				try {
					// Se crea la ventana de espera
					final JDialog dialog = UIFactory.getWaitingDialog(
							null,
							Messages.getString("CreateHashFiles.21"), //$NON-NLS-1$
							Messages.getString("CreateHashFiles.22") //$NON-NLS-1$
							);

					// Arrancamos el proceso en un hilo aparte
					final SwingWorker<CheckResult, Void> worker = new SwingWorker<CheckResult, Void>() {

						@Override
						protected CheckResult doInBackground() throws Exception {

							final HashReport report = new HashReport();
							try {
								checkHash(Paths.get(getDirectorioText()), new File(getReportPath()), report);
							}
							catch (final Exception ex) {
								return new CheckResult(ex);
							}
							return new CheckResult(report);
						}
						@Override
						protected void done() {
							super.done();
							if (dialog != null) {
								dialog.dispose();
							}
						}
					};
					worker.execute();

					// Se muestra la ventana de espera
					if (dialog != null) {
						dialog.setVisible(true);
					}

					final CheckResult result = worker.get();

					// Si el proceso genero un error, lo relanzamos
					if (result.getException() != null) {
						throw result.getException();
					}

					final HashReport report = result.getReport();

					if (!report.hasErrors()) {
						AOUIFactory.showMessageDialog(
								CheckHashDirDialog.this,
								Messages.getString("CheckHashDialog.2"), //$NON-NLS-1$
								Messages.getString("CheckHashDialog.3"), //$NON-NLS-1$
								JOptionPane.INFORMATION_MESSAGE
								);
					}
					else {
						AOUIFactory.showMessageDialog(
								CheckHashDirDialog.this,
								Messages.getString("CheckHashFiles.18"), //$NON-NLS-1$
								Messages.getString("CheckHashFiles.17"), //$NON-NLS-1$
								JOptionPane.WARNING_MESSAGE
								);
					}

					// Guardamos el informe
					final byte[] reportContent = generateXMLReport(report).getBytes(report.getCharset());
					HashUIHelper.showSaveReportDialog(reportContent, new File(getDirectorioText()).getParent(), parent);

					checkButton.setEnabled(false);
					CheckHashDirDialog.this.setVisible(false);
					CheckHashDirDialog.this.dispose();
				}
				catch (final OutOfMemoryError ooe) {
					AOUIFactory.showErrorMessage(
						Messages.getString("CreateHashFiles.2"), //$NON-NLS-1$
						Messages.getString("CreateHashDialog.14"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE,
						ooe
					);
					LOGGER.log(Level.SEVERE, "Fichero demasiado grande", ooe); //$NON-NLS-1$
				}
				catch (final AOCancelledOperationException ex) {
					// Operacion cancelada por el usuario
				}
				catch (final DocumentException ex) {
					LOGGER.log(Level.WARNING, "El documento seleccionado no es un documento de hashes soportado"); //$NON-NLS-1$
					AOUIFactory.showErrorMessage(
						Messages.getString("CheckHashDialog.17"), //$NON-NLS-1$
						Messages.getString("CheckHashDialog.7"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE,
						ex
					);
				}
				catch (final CorruptedDocumentException ex) {
					LOGGER.log(Level.WARNING, "El documento seleccionado es un documento de hashes corrupto o manipulado", ex); //$NON-NLS-1$
					AOUIFactory.showErrorMessage(
						Messages.getString("CheckHashDialog.18"), //$NON-NLS-1$
						Messages.getString("CheckHashDialog.7"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE,
						ex
					);
				}
				catch (final Exception ex) {
					LOGGER.log(Level.SEVERE, "No ha sido posible comprobar las huellas digitales", ex); //$NON-NLS-1$
					AOUIFactory.showErrorMessage(
						Messages.getString("CheckHashDialog.6"), //$NON-NLS-1$
						Messages.getString("CheckHashDialog.7"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE,
						ex
					);
				}
			}
		);
		checkButton.setEnabled(false);

		this.reportTextField.setEditable(false);
		this.reportTextField.setFocusable(false);
		this.reportTextField.setColumns(10);
		this.reportTextField.addKeyListener(this);

		final JButton xmlExamineButton = new JButton(
			Messages.getString("CreateHashDialog.5") //$NON-NLS-1$
		);
		xmlExamineButton.setMnemonic('E');
		xmlExamineButton.setEnabled(true);
		xmlExamineButton.addKeyListener(this);
		xmlExamineButton.addActionListener(e -> {
			try {
				setReportPath(
					AOUIFactory.getLoadFiles(
						Messages.getString("CreateHashDialog.5"), //$NON-NLS-1$
						null,
						null,
						new String[] { "hashfiles", "txthashfiles" }, //$NON-NLS-1$ //$NON-NLS-2$
						Messages.getString("CheckHashFiles.19"), //$NON-NLS-1$
						false,
						false,
						icon,
						CheckHashDirDialog.this
					)[0].getAbsolutePath()
				);
				final String hashFile = getReportPath();
				if (!(hashFile == null) && !hashFile.isEmpty()) {
					checkButton.setEnabled(true);
				}
			}
			catch (final AOCancelledOperationException ex) {
				// Operacion cancelada por el usuario
			}
		}
		);

		this.directoryTextField.setEditable(false);
		this.directoryTextField.setFocusable(false);
		this.directoryTextField.setColumns(10);
		this.directoryTextField.addKeyListener(this);

		final JButton directoryExamineButton = new JButton(
			Messages.getString("CreateHashDialog.5") //$NON-NLS-1$
		);
		directoryExamineButton.setMnemonic('X');
		directoryExamineButton.setEnabled(true);
		directoryExamineButton.addKeyListener(this);
		directoryExamineButton.addActionListener(e -> {
			try {
				setDirectoryPath(
					AOUIFactory.getLoadFiles(
						Messages.getString("CreateHashDialog.5"), //$NON-NLS-1$
						null,
						null,
						null,
						Messages.getString("CheckHashDialog.14"), //$NON-NLS-1$
						true,
						false,
						icon,
						CheckHashDirDialog.this
					)[0].getAbsolutePath()
				);
				final String hashFile = getReportPath();
				if (!(hashFile == null) && !hashFile.isEmpty()) {
					checkButton.setEnabled(true);
				}
			}
			catch (final AOCancelledOperationException ex) {
				// Operacion cancelada por el usuario
			}
		}
		);

		final JButton exitButton = new JButton(Messages.getString("CheckHashFiles.12") //$NON-NLS-1$
		);

		if (!getReportPath().isEmpty() && !getDirectorioText().isEmpty()) {
			checkButton.setEnabled(true);
		}
		exitButton.setMnemonic('C');
		exitButton.addKeyListener(this);
		exitButton.addActionListener(
			e -> {
				CheckHashDirDialog.this.setVisible(false);
				CheckHashDirDialog.this.dispose();
			}
		);
		exitButton.getAccessibleContext().setAccessibleDescription(
			Messages.getString("CreateHashDialog.17") //$NON-NLS-1$
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
			Messages.getString("CheckHashFiles.13") //$NON-NLS-1$
		);
		directoryLabel.setLabelFor(this.directoryTextField);
		final JLabel xmlLabel = new JLabel(Messages.getString("CheckHashFiles.14")); //$NON-NLS-1$
		xmlLabel.setLabelFor(this.reportTextField);

		c.add(directoryLabel, gbc);
		gbc.insets = new Insets(0, 10, 0, 10);
		gbc.gridy++;
		c.add(this.directoryTextField, gbc);
		gbc.weightx = 0.0;
		c.add(directoryExamineButton, gbc);
		gbc.insets = new Insets(15, 10, 5, 10);
		gbc.gridy++;
		gbc.weightx = 1.0;
		c.add(xmlLabel, gbc);
		gbc.insets = new Insets(0, 10, 0, 10);
		gbc.gridy++;
		c.add(this.reportTextField, gbc);
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

	/**
	 * Analiza el fichero para identificar si se trata de alguno de los formatos soportados
	 * de documentos de hashes y comprueba que los hashes de los ficheros del directorio
	 * indicado se correspondan con los del documento.
	 * @param dir Directorio seleccionado.
	 * @param hashDocumentPath Ruta del documento de hashes.
	 * @param report Informe en el que registrar el resultado de la comprobaci&oacute;n.
	 * @throws IOException Error al abrir o cerrar el fichero seleccionado.
	 * @throws DocumentException Cuando se proporcione un documento de hashes no soportado.
	 * @throws CorruptedDocumentException Cuando se ha identificado que el documento est&aacute; corrupto.
	 */
	public static void checkHash(final Path dir, final File hashDocumentPath, final HashReport report)
			throws IOException, DocumentException, CorruptedDocumentException {

		// Cargamos el documento de hashes
		final byte[] hashDocumentContent = loadData(hashDocumentPath.getAbsolutePath());

		final HashDocument hashDocument = HashDocumentFactory.loadDocument(hashDocumentContent, getExtension(hashDocumentPath));

		// El informe se genera en base a la misma configuracion que tuviese el documento de hashes
		report.setAlgorithm(hashDocument.getAlgorithm());
		report.setRecursive(hashDocument.isRecursive());

		final long startTime = new Date().getTime();

		final ForkJoinPool pool = new ForkJoinPool(10);
		final RecursiveAction recursiveAction = new CheckHashAction(
				dir, dir.toFile(), hashDocument, report, pool);
		pool.invoke(recursiveAction);
		recursiveAction.join();

		final long processTime = new Date().getTime() - startTime;
		LOGGER.info("Tiempo total de comprobacion de hashes: " + processTime / 1000.0 + " seg"); //$NON-NLS-1$ //$NON-NLS-2$
		LOGGER.info("Numero de ficheros procesados: " + report.getProcessedFilesCount()); //$NON-NLS-1$

		// Agregamos al informe las entradas que no se procesaron
		for (final String pathname : hashDocument.getHashes().keySet()) {
			report.reportHashWithoutFile(pathname);
		}
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
	 * @return Informe de coincidencias de huellas en XML.
	 * @throws ParserConfigurationException Si no se puede obtener en analizador XML.
	 * @throws TransformerException Si hay errores escribiendo el XML. */
	public static String generateXMLReport(final HashReport mapReport)
			throws ParserConfigurationException, TransformerException {

		final DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
		final DocumentBuilder docBuilder = docFactory.newDocumentBuilder();

		// Elemento raiz
		final Document doc = docBuilder.newDocument();
		final Element rootElement = doc.createElement("entries"); //$NON-NLS-1$
		final Attr hashAlgAttr = doc.createAttribute("hashAlgorithm"); //$NON-NLS-1$
		hashAlgAttr.setValue(mapReport.getAlgorithm());
		rootElement.setAttributeNode(hashAlgAttr);
		final Attr recursiveAttr = doc.createAttribute("recursive"); //$NON-NLS-1$
		recursiveAttr.setValue(Boolean.toString(mapReport.isRecursive()));
		rootElement.setAttributeNode(recursiveAttr);
		doc.appendChild(rootElement);

		// El hash del fichero coincide con el hash almacenado
		final Iterator<String> machingHashIt = mapReport.getMatchingHashIterator();
		if (machingHashIt.hasNext()) {
			final Element entriesHeader = doc.createElement("matching_hash"); //$NON-NLS-1$
			while (machingHashIt.hasNext()) {
				entriesHeader.appendChild(createReportEntry(doc, machingHashIt.next()));
			}
			rootElement.appendChild(entriesHeader);
		}

		// El hash del fichero no coincide con el hash almacenado
		final Iterator<String> noMachingHashIt = mapReport.getNoMatchingHashIterator();
		if (noMachingHashIt.hasNext()) {
			final Element entriesHeader = doc.createElement("not_matching_hash"); //$NON-NLS-1$
			while (noMachingHashIt.hasNext()) {
				entriesHeader.appendChild(createReportEntry(doc, noMachingHashIt.next()));
			}
			rootElement.appendChild(entriesHeader);
		}

		// No se ha encontrado el fichero correspondiente a un hash almacenado
		final Iterator<String> hashWithoutFileIt = mapReport.getHashWithoutFileIterator();
		if (hashWithoutFileIt.hasNext()) {
			final Element entriesHeader = doc.createElement("hash_without_file"); //$NON-NLS-1$
			while (hashWithoutFileIt.hasNext()) {
				entriesHeader.appendChild(createReportEntry(doc, hashWithoutFileIt.next()));
			}
			rootElement.appendChild(entriesHeader);
		}

		// No se ha encontrado un hash almacenado que corresponda al fichero
		final Iterator<String> fileWithoutHashIt = mapReport.getFileWithoutHashIterator();
		if (fileWithoutHashIt.hasNext()) {
			final Element entriesHeader = doc.createElement("file_without_hash"); //$NON-NLS-1$
			while (fileWithoutHashIt.hasNext()) {
				entriesHeader.appendChild(createReportEntry(doc, fileWithoutHashIt.next()));
			}
			rootElement.appendChild(entriesHeader);
		}

		// Componemos el XML
		final StringWriter sw = new StringWriter();
		final TransformerFactory tf = TransformerFactory.newInstance();
		final Transformer transformer = tf.newTransformer();
		transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no"); //$NON-NLS-1$
		transformer.setOutputProperty(OutputKeys.METHOD, "xml"); //$NON-NLS-1$
		transformer.setOutputProperty(OutputKeys.INDENT, "yes"); //$NON-NLS-1$
		transformer.setOutputProperty(OutputKeys.ENCODING, StandardCharsets.UTF_8.name());

		transformer.transform(new DOMSource(doc), new StreamResult(sw));
		return sw.toString();
	}

	private static Element createReportEntry(final Document doc, final String path) {
		final Element entry = doc.createElement("entry"); //$NON-NLS-1$
		final Attr name = doc.createAttribute("name"); //$NON-NLS-1$
		name.setValue(path);
		entry.setAttributeNode(name);

		return entry;
	}

	/** Obtiene el nombre del fichero seleccionado por el usuario.
	 * @return Nombre del fichero seleccionado por el usuario. */
	String getReportPath() {
		return this.reportTextField.getText();
	}

	/** Establece el nombre del fichero seleccionado por el usuario.
	 * @param path Nombre del fichero seleccionado por el usuario. */
	void setReportPath(final String path) {
		this.reportTextField.setText(path);
	}

	/** Obtiene el nombre del fichero con la huella digital seleccionada por el
	 * usuario.
	 * @return Nombre del fichero con la huella digital seleccionada por el
	 *         usuario. */
	String getDirectorioText() {
		return this.directoryTextField.getText();
	}

	/** Establece el nombre del fichero con la huella digital seleccionada por
	 * el usuario.
	 * @param path Nombre de la huella digital seleccionada por el usuario. */
	void setDirectoryPath(final String path) {
		this.directoryTextField.setText(path);
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

	/**
	 * Carga los datos de un fichero.
	 * @param path Ruta del fichero del que cargar los datos.
	 * @return Contenido del fichero.
	 * @throws IOException Cuando no se puede completar la carga de los datos.
	 */
	private static byte[] loadData(final String path) throws IOException {
		URI pathUri;
		try {
			pathUri = AOUtil.createURI(path);
		}
		catch (final Exception e) {
			throw new IOException("No se pudo componer la ruta del fichero", e); //$NON-NLS-1$
		}
		return AOUtil.getDataFromInputStream(AOUtil.loadFile(pathUri));
	}

	/**
	 * Recupera la extensi&oacute;n del fichero.
	 * @param file Fichero.
	 * @return Extensi&oacute;n del fichero sin punto o {@code null} si no ten&iacute;a.
	 */
	private static String getExtension(final File file) {
		final String filename = file.getName();
		final int pos = filename.lastIndexOf('.');
		return pos == -1 || pos == filename.length() - 1 ? null : filename.substring(pos + 1);
	}

	/**
	 * Recupera el icono que deben mostrar los di&aacute;logos.
	 * @return Icono de los di&aacute;logos o {@code null} si no se puede cargar.
	 */
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

	class CheckResult {

		private final Exception exception;
		private final HashReport report;

		public CheckResult(final HashReport report) {
			this.report = report;
			this.exception = null;
		}

		public CheckResult(final Exception exception) {
			this.report = null;
			this.exception = exception;
		}

		public Exception getException() {
			return this.exception;
		}

		public HashReport getReport() {
			return this.report;
		}
	}
}
