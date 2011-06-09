package es.gob.afirma.standalone.ui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.FileDialog;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.net.URI;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.filechooser.FileFilter;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.batik.swing.JSVGCanvas;

import es.gob.afirma.exceptions.AOCancelledOperationException;
import es.gob.afirma.exceptions.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.misc.AOCryptoUtil;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.Platform;
import es.gob.afirma.signers.AOCAdESSigner;
import es.gob.afirma.signers.AOPDFSigner;
import es.gob.afirma.signers.AOSigner;
import es.gob.afirma.signers.AOXAdESSigner;
import es.gob.afirma.signers.AOXMLDSigSigner;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.ui.AOUIManager;

/**
 * Panel de selecci&oacute;n y firma del fichero objetivo.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class SignPanel extends JPanel {

	private static final long serialVersionUID = -4828575650695534417L;
	
	private final JSVGCanvas fileTypeVectorIcon = new JSVGCanvas();
	
	private static final String FILE_ICON_PDF = "/resources/icon_pdf.svg";
	private static final String FILE_ICON_XML = "/resources/icon_xml.svg";
	private static final String FILE_ICON_BINARY = "/resources/icon_binary.svg";
	
	private AOSigner signer;
	private byte[] dataToSign;
	
	private JPanel filePanel;
	private JPanel lowerPanel;
	
	private DropTarget dropTarget;
	
	private final JFrame window;
	
	private final JButton signButton = new JButton();
	private final JButton selectButton = new JButton();
	
	private final SimpleAfirma saf;
	
	private File currentDir = null;
	private File currentFile = null;
	
	private boolean isXML(final byte[] data) {
		try {
			DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new ByteArrayInputStream(data));
		}
		catch(final Exception e) {
			return false;
		}
		return true;
	}
	
	private void loadFile(final String filename) throws IOException {
		
		this.setCursor(new Cursor(Cursor.WAIT_CURSOR));
		
		final File file = new File(filename);
		
		String errorMessage = null;
		if (!file.exists()) {
			errorMessage = "El fichero seleccionado no existe, y por lo tanto no puede firmarse.";
		}
		else if (!file.canRead()) {
			errorMessage = "No se tienen permisos de lectura para el fichero seleccionado.\nSeleccione otro fichero o habilite los permisos de lectura para este.";
		}
		else if (file.length() < 1) {
			errorMessage = "El fichero seleccionado est‡ vac’o.\nNo es posible firmar ficheros vac’os.";
		}
		if (errorMessage != null) {
			JOptionPane.showOptionDialog(
				SignPanel.this, 
				errorMessage,
				"Error",
				JOptionPane.OK_OPTION,
				JOptionPane.ERROR_MESSAGE,
				null,
				new Object[] { "Cerrar "},
				null
			);
			return;
		}
		
		final FileInputStream fis = new FileInputStream(file);
		
		final byte[] data = AOUtil.getDataFromInputStream(fis);
		if (data == null || data.length < 1) {
			throw new IOException("No se ha podido leer el fichero");
		}
		this.dataToSign = data;
		
		try {
			fis.close();
		} catch(final Exception e) {}
		
		String fileDescription;
		String iconPath;
		if (new AOPDFSigner().isValidDataFile(data)) {
			iconPath = FILE_ICON_PDF;
			fileDescription = "Documento Adobe PDF";
			this.signer = new AOPDFSigner();
		}
		else if (isXML(data)) {
			iconPath = FILE_ICON_XML;
			fileDescription = "Documento XML";
			this.signer = new AOXAdESSigner();
		}
		else {
			iconPath = FILE_ICON_BINARY;
			fileDescription = "Documento binario gen\u00E9rico";
			this.signer = new AOCAdESSigner();
		}
		
		final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);

		try {
			this.fileTypeVectorIcon.setDocument(dbf.newDocumentBuilder().parse(
				this.getClass().getResourceAsStream(iconPath)
			));
		}
		catch(final Exception e) {
			Logger.getLogger("es.gob.afirma").warning(
				"No se ha podido cargar el icono del tipo de fichero/firma, este no se mostrara: " + e
			);
		}
		
		final long fileSize = file.length();
		final long fileLastModified = file.lastModified();

		this.lowerPanel.remove(this.filePanel);
		this.filePanel = new FilePanel(
			this.fileTypeVectorIcon, 
			NumberFormat.getInstance().format(fileSize), 
			filename, 
			fileDescription, 
			new Date(fileLastModified)
		);
		this.lowerPanel.add(this.filePanel);
		this.lowerPanel.revalidate();
		
		if (this.window != null) {
			this.window.getRootPane().putClientProperty("Window.documentFile", new File(filename));
			this.window.setTitle(this.window.getTitle() + " - " + new File(filename).getName());
		}
		
		this.currentFile = file;
		
		this.signButton.setEnabled(true);
		
		this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		this.signButton.requestFocusInWindow();
	}
	
	private void createUI(final ActionListener al) {
		this.setBackground(SimpleAfirma.WINDOW_COLOR);
		this.setLayout(new GridLayout(2, 1));
		this.add(new UpperPanel(al));
		this.lowerPanel = new LowerPanel(al);
		this.add(this.lowerPanel);
		this.dropTarget = new DropTarget(
			this.filePanel, 
			DnDConstants.ACTION_COPY, 
			new DropTargetListener() {
			
				@Override public void dropActionChanged(final DropTargetDragEvent dtde) {}
				@Override public void dragOver(final DropTargetDragEvent dtde) {}
				@Override public void dragExit(final DropTargetEvent dte) {}
				
				@Override
				public void drop(final DropTargetDropEvent dtde) {

					final Transferable tr = dtde.getTransferable();
					if (tr.isDataFlavorSupported(DataFlavor.javaFileListFlavor)) {
						dtde.acceptDrop(DnDConstants.ACTION_COPY);
						final Object transData;
						try {
							transData = tr.getTransferData(DataFlavor.javaFileListFlavor);
						}
						catch(final Exception e) {
							Logger.getLogger("es.gob.afirma").warning(
								"Ha fallado la operacion de arrastrar y soltar: " + e
							);
							dtde.dropComplete(false);
							return;
						}
						if (transData instanceof List) {
							dtde.getDropTargetContext().dropComplete(true);
							final List<?> fileList = (List<?>) transData;
							if (fileList.isEmpty()) {
								dtde.dropComplete(false);
								return;
							}
							if (fileList.size() > 1) {
								JOptionPane.showOptionDialog(
									SignPanel.this, 
									"Ha arrastrado varios ficheros a la vez, pero solo se soporta la firma individual de archivos.\nSólo se firmará el primero",
									"Advertencia",
									JOptionPane.OK_OPTION,
									JOptionPane.WARNING_MESSAGE,
									null,
									new Object[] { "Cerrar "},
									null
								);
							}
							String fileName = fileList.get(0).toString();
							if (fileName.startsWith("http://") ||
								fileName.startsWith("https://") ||
								fileName.startsWith("ftp://")
							) {
								JOptionPane.showOptionDialog(
									SignPanel.this, 
									"No se soporta la firma de ficheros remotos con esta aplicación,\nseleccione un fichero local para firmarlo", 
									"Error",
									JOptionPane.OK_OPTION,
									JOptionPane.ERROR_MESSAGE,
									null,
									new Object[] { "Cerrar "},
									null
								);
								dtde.dropComplete(false);
								return;
							}
							else if (fileName.startsWith("file://")) {
								try {
									fileName = new File(new URI(fileName)).getPath();
								}
								catch(final Exception e) {
									Logger.getLogger("es.gob.afirma").warning(
										"Ha fallado la operacion de arrastrar y soltar al obtener la ruta del fichero arrastrado: " + e
									);
									dtde.dropComplete(false);
									return;
								}
							}
							try {
								loadFile(fileName);
							}
							catch(final IOException e) {
								Logger.getLogger("es.gob.afirma").warning(
									"Ha fallado la operacion de arrastrar y soltar al cargar el fichero arrastrado: " + e
								);
								dtde.dropComplete(false);
							}
						}
					}
					else {
						dtde.rejectDrop();
						dtde.dropComplete(false);
					}
			
				}
				
				@Override
				public void dragEnter(final DropTargetDragEvent dtde) {
					if (!dtde.isDataFlavorSupported(DataFlavor.javaFileListFlavor)) {
						dtde.rejectDrag();
					}
				}
			},
			true
		);
		setVisible(true);
	}
	
	/**
	 * Construye el panel de firma, en el que se selecciona y se firma un fichero.
	 * @param win Ventana de primer nivel, para el cambio de t&iacute;tulo en la carga de fichero
	 * @param sa Clase principal, para proporcionar el <code>AOKeyStoreManager</code> necesario para 
	 *           realizar las firmas y cambiar de panel al finalizar una firma
	 */
	public SignPanel(final JFrame win, final SimpleAfirma sa) {
		super(true);
		this.window = win;
		this.saf = sa;
		createUI(null);
	}
	
	private final class UpperPanel extends JPanel {

		private static final long serialVersionUID = 533243192995645135L;
		
		private void createUI(final ActionListener al) {
			this.setLayout(new BorderLayout(5,5));
			this.setBorder(BorderFactory.createEmptyBorder(15, 15, 15, 15));
			this.setBackground(SimpleAfirma.WINDOW_COLOR);
						
			SignPanel.this.selectButton.setText("Seleccionar fichero a firmar");
			if (al != null) SignPanel.this.selectButton.addActionListener(al);
			SignPanel.this.selectButton.setMnemonic('S');
			SignPanel.this.selectButton.getAccessibleContext().setAccessibleDescription(
				"Pulse este botón para iniciar el proceso de selecci—n de fichero a firmar"
			);
			SignPanel.this.selectButton.getAccessibleContext().setAccessibleName(
				"Bot—n de selecci—n de fichero a firmar"
			);
			SignPanel.this.selectButton.requestFocusInWindow();
			SignPanel.this.selectButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent arg0) {
					String fileToLoad;
					
					if (Platform.OS.MACOSX.equals(Platform.getOS())) {
						if (SignPanel.this.currentDir == null) SignPanel.this.currentDir = new File(Platform.getUserHome());
						final FileDialog fd = new FileDialog((Frame)null, "Seleccione el fichero a firmar");
						fd.setDirectory(SignPanel.this.currentDir.getAbsolutePath());
						fd.setVisible(true);
						if (fd.getFile() == null) return;
						SignPanel.this.currentDir = new File(fd.getDirectory());
						fileToLoad = fd.getDirectory() + fd.getFile();
					}
					else {
						final JFileChooser fc = new JFileChooser();
						if (SignPanel.this.currentDir != null) fc.setCurrentDirectory(SignPanel.this.currentDir);
						if (JFileChooser.APPROVE_OPTION == fc.showOpenDialog(UpperPanel.this)) {
							SignPanel.this.currentDir = fc.getCurrentDirectory();
							fileToLoad = fc.getSelectedFile().getAbsolutePath();
						}
						else return;
					}
					
					
					try {
						loadFile(fileToLoad);
					}
					catch(final Exception e) {
						JOptionPane.showOptionDialog(
							UpperPanel.this, 
							"No se ha podido leer el contenido del fichero a firmar,\n" +
								"compruebe que no esta abierto por otra aplicación y\n" +
								"si está en almacenamiento extraible pruebe a expulsarlo\n" +
								"e insertarlo de nuevo",
							"Error",
							JOptionPane.OK_OPTION,
							JOptionPane.ERROR_MESSAGE,
							null,
							new Object[] { "Cerrar "},
							null
						);
					}
					
				}
			});
			
			final JLabel welcomeLabel = new JLabel("Bienvenido a \"Firma electr—nica f‡cil con @Firma\"");
			welcomeLabel.setFocusable(false);
			welcomeLabel.setFont(welcomeLabel.getFont().deriveFont(Font.PLAIN, 26));
			welcomeLabel.setLabelFor(SignPanel.this.selectButton);
			welcomeLabel.setForeground(new Color(3399));
			this.add(welcomeLabel, BorderLayout.PAGE_START);
			
			final String intro = 
				"<html><p>En esta pantalla puede firmar elect&oacute;nicamente un fichero que se encuentre en su disco duro.<br>" +
				"Cuando firma electr&oacute;nicamente un fichero pueden incorporarse a este ciertos datos personales, entre los que " +
				"pueden encontrarse su número de DNI, su nombre y apellidos o incluso informaci&oacute;n " +
				"sobre su situaci&oacute;n laboral si utiliza un certificado profesional. " +
				"Consulte las pol&iacute;ticas de seguridad y protecci&oacute;n de datos de los receptores de los " +
				"ficheros firmados antes en eviarlos o distribuirlos.<br>"
			;
			final JLabel introText = new JLabel(intro);
			introText.setLabelFor(SignPanel.this.selectButton);
			introText.setFocusable(false);
			
			final JPanel introPanel = new JPanel(new BorderLayout());
			introPanel.add(introText, BorderLayout.PAGE_START);
			introPanel.setBackground(SimpleAfirma.WINDOW_COLOR);
			
			this.add(introPanel, BorderLayout.CENTER);
			
			final JPanel selectPanel = new JPanel(new FlowLayout(FlowLayout.LEFT), true);
			selectPanel.setBackground(SimpleAfirma.WINDOW_COLOR);
			selectPanel.add(SignPanel.this.selectButton);
			this.add(selectPanel, BorderLayout.PAGE_END);
			
		}
		
		private UpperPanel(final ActionListener al) {
			super(true);
			createUI(al);
		}
		
	}
	
	private final class LowerPanel extends JPanel {

		private static final long serialVersionUID = 533243192995645135L;
		
		private LowerPanel(final ActionListener al) {
			super(true);
			SwingUtilities.invokeLater(new Runnable() {
				@Override
				public void run() {
					createUI(al);
				}
			});
		}
		
		private void createUI(final ActionListener al) {
			this.setBackground(SimpleAfirma.WINDOW_COLOR);
			this.setLayout(new BorderLayout(5,5));
			this.setBorder(BorderFactory.createEmptyBorder(15, 15, 15, 15));
			
			SignPanel.this.filePanel = new ResizingTextPanel(
				"Pulse el bot\u00F3n o arrastre un fichero en este \u00E1rea"
			);
			SignPanel.this.filePanel.setBackground(Color.DARK_GRAY);
			SignPanel.this.filePanel.setForeground(Color.LIGHT_GRAY);
			SignPanel.this.filePanel.getAccessibleContext().setAccessibleDescription(
				"Arrastre un fichero a este \u00E1rea para seleccionarlo para poder firmarlo electr\u00F3nicamente"
			);
			SignPanel.this.filePanel.getAccessibleContext().setAccessibleName(
				"Panel arratre fichero"
			);
			SignPanel.this.filePanel.setToolTipText(
				"Arrastre un fichero a este ‡rea para seleccionarlo y poder firmarlo electr—nicamente"
			);
			SignPanel.this.filePanel.setFocusable(false);
			SignPanel.this.filePanel.setDropTarget(SignPanel.this.dropTarget);
			
			this.add(SignPanel.this.filePanel,BorderLayout.CENTER);
			
			final JPanel signPanel = new JPanel(true);
			signPanel.setBackground(SimpleAfirma.WINDOW_COLOR);
			SignPanel.this.signButton.setText("Firmar fichero");
			SignPanel.this.signButton.setMnemonic('F');
			if (al != null) SignPanel.this.signButton.addActionListener(al);
			SignPanel.this.signButton.setEnabled(false);
			signPanel.add(SignPanel.this.signButton);
			SignPanel.this.signButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent arg0) {
					sign();
				}
			});

			this.add(signPanel, BorderLayout.PAGE_END);
		}
		
	}
	
	private final class FilePanel extends JPanel {

		private static final long serialVersionUID = -8648491975442788750L;
		
		private FilePanel(final Component icon, 
				          final String fileSize, 
				          final String filePath,
				          final String fileDescription,
				          final Date fileLastModified) {
			super(true);
			SwingUtilities.invokeLater(new Runnable() {
				@Override
				public void run() {
					createUI(icon, fileSize, filePath, fileDescription, fileLastModified);
				}
			});
		}
		
		private void createUI(final Component icon, 
		                      final String fileSize, 
		                      final String filePath,
		                      final String fileDescription,
		                      final Date fileLastModified) {
		    this.setBackground(SimpleAfirma.WINDOW_COLOR);
		    this.setBorder(BorderFactory.createLineBorder(Color.black));
		    this.setLayout(new GridBagLayout());
		    
		    final JLabel pathLabel = new JLabel(filePath);
		    pathLabel.setFont(pathLabel.getFont().deriveFont(Font.BOLD, pathLabel.getFont().getSize() + 4f));
		    final JLabel descLabel = new JLabel("Tipo de fichero: " + fileDescription);
		    final JLabel dateLabel = new JLabel("Fecha \u00FAltima modificaci\u00F3n: " + 
		            new SimpleDateFormat("dd/MMMMM/yyyy").format(fileLastModified));
		    final JLabel sizeLabel = new JLabel("Tama\u00F1o: " + fileSize + " KB");
		    
		    final JPanel detailPanel = new JPanel();
		    detailPanel.setBackground(SimpleAfirma.WINDOW_COLOR);
		    detailPanel.setLayout(new BoxLayout(detailPanel, BoxLayout.Y_AXIS));
		    detailPanel.add(pathLabel);
		    detailPanel.add(Box.createRigidArea(new Dimension(0,8)));
		    detailPanel.add(descLabel);
		    detailPanel.add(dateLabel);
		    detailPanel.add(Box.createRigidArea(new Dimension(0,8)));
		    detailPanel.add(sizeLabel);
		    
		    final JButton openFileButton = new JButton("Ver Fichero");
		    openFileButton.setMnemonic('v');
		    openFileButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent ae) {
					try {
						Desktop.getDesktop().open(new File(filePath));
					}
					catch(final IOException e) {
						JOptionPane.showOptionDialog(
							FilePanel.this, 
							"Error",
							"No se ha podido abrir el fichero,\ncompruebe que no ha sido manipulado mientras se ejecutaba esta aplicaci—n",
							JOptionPane.OK_OPTION,
							JOptionPane.ERROR_MESSAGE,
							null,
							new Object[] { "Cerrar "},
							null
						);
						return;
					}
				}
			});
		    
		    final GridBagConstraints c = new GridBagConstraints();
		    c.fill = GridBagConstraints.BOTH;
		    c.weightx = 0.0;
		    c.weighty = 1.0;
		    c.ipadx = 60;
		    c.ipady = 60;
		    c.insets = new Insets(11, 0, 11, 0);
		    c.anchor = GridBagConstraints.NORTHWEST;
		    this.add(icon, c);
		    c.weightx = 1.0;
		    c.gridx = 1;
		    c.ipadx = 0;
            c.ipady = 0;
		    c.insets = new Insets(11, 0, 11, 5);
		    c.anchor = GridBagConstraints.NORTH;
		    this.add(detailPanel, c);
		    c.fill = GridBagConstraints.HORIZONTAL;
            c.weightx = 0.0;
            c.weighty = 0.0;
            c.gridx = 2;
            c.insets = new Insets(11, 6, 11, 11);
            c.anchor = GridBagConstraints.NORTHEAST;
            this.add(openFileButton, c);
		}
	}
	
	private void sign() {
		this.setCursor(new Cursor(Cursor.WAIT_CURSOR));
		if (this.signer == null || this.dataToSign == null || this.saf == null) {
			return;
		}
		final AOKeyStoreManager ksm = this.saf.getAOKeyStoreManager();
		final String alias;
		
        //TODO: Filtrar por KeyUsage de firma
		try {
			alias = AOUIManager.showCertSelectionDialog(
				ksm.getAliases(),
				ksm.getKeyStores(), 
				//isDNIeProvider(ksm.getKeyStores().get(0).getProvider().getName()) ? SIGN_CERT_USAGE : null, 
				this, 
				true, 
				true, 
				false 
			);
		}
		catch(final AOCertificatesNotFoundException e) {
			JOptionPane.showOptionDialog(
				this, 
				"No se ha encontrado ningœn certificado para firmar,\nCompruebe su almacén de claves e importe uno si es necesario",
				"Advertencia",
				JOptionPane.OK_OPTION,
				JOptionPane.WARNING_MESSAGE,
				null,
				new Object[] { "Cerrar "},
				null
			);
			return;
		}
		catch(final AOCancelledOperationException e) {
            return;
        }
		finally {
		    this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		}
		
		final Properties p = new Properties();
		p.put("signingCertificateV2", "true");
		p.put("ignoreStyleSheets", "false");
		
		this.setCursor(new Cursor(Cursor.WAIT_CURSOR));
		
		final byte[] signResult;
		try {
			signResult = this.signer.sign(
				this.dataToSign, 
				"SHA1withRSA", 
				ksm.getKeyEntry(
					alias, 
					AOCryptoUtil.getPreferredPCB(ksm.getType(), this)
				), 
				p
			);
		}
		catch(final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de firma: " + e);
			JOptionPane.showOptionDialog(
				SignPanel.this, 
				"Ocurrió un problema durante el proceso de firma, pruebe a reintentar\nla firma reiniciando la aplicación",
				"Error",
				JOptionPane.OK_OPTION,
				JOptionPane.ERROR_MESSAGE,
				null,
				new Object[] { "Cerrar "},
				null
			);
			return; 
		} 
		finally {
		    this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		}
		this.signButton.setEnabled(false);
		
		String newFileName = this.currentFile.getName();
		String[] filterExtensions;
		String filterDescription;
		if (this.signer instanceof AOPDFSigner) {
			if (!newFileName.toLowerCase().endsWith(".pdf")) {
				newFileName = newFileName + ".pdf";
			}
			newFileName = newFileName.substring(0, newFileName.length()-4) + ".signed.pdf";
			filterExtensions = new String[] { ".pdf" };
			filterDescription = "Documentos Adobe PDF (*.pdf)";
		}
		else if (this.signer instanceof AOXMLDSigSigner || this.signer instanceof AOXAdESSigner) {
			newFileName = newFileName + ".xsig";
			filterExtensions = new String[] { ".xsig", ".xml" };
			filterDescription = "Firmas XML (*.xml, *.xsig)";
		}
		else {
			newFileName = newFileName + ".csig";
			filterExtensions = new String[] { ".csig", ".p7s" };
			filterDescription = "Firmas binarias (*.csig, *.p7s)";
		}
		
		final String fDescription = filterDescription;
		final String[] fExtensions = filterExtensions;
		        
		// Para las comprobaciones de si el usuario ha teclado la etension o espera que
		// la aplicacion la anada sola
		boolean nameMissingExtension = true;
		
        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            if (this.currentDir == null) this.currentDir = new File(Platform.getUserHome());
            final FileDialog fd = new FileDialog(this.window, "Guardar como", FileDialog.SAVE);
            fd.setDirectory(this.currentDir.getAbsolutePath());
            fd.setFile(newFileName);
            fd.setFilenameFilter(new FilenameFilter() {
                @Override
                public boolean accept(final File dir, final String name) {
                    for (final String ext : fExtensions) {
                        if (name.endsWith(ext)) {
                            return true;
                        }
                    }
                    return false;
                }
            });
            fd.setVisible(true);
            if (fd.getFile() == null) return;
            this.currentDir = new File(fd.getDirectory());
            newFileName = fd.getDirectory() + fd.getFile();
        }
        else {
            final JFileChooser fc = new JFileChooser();
            if (this.currentDir != null) fc.setCurrentDirectory(this.currentDir);
            fc.setSelectedFile(new File(newFileName));
            fc.setFileFilter(new FileFilter() {
                @Override
                public boolean accept(final File file) {
                    for (final String ext : fExtensions) {
                        if (file.getName().endsWith(ext)) {
                            return true;
                        }
                    }
                    return false;
                }
                @Override
                public String getDescription() {
                    return fDescription;
                }
            });
            if (JFileChooser.APPROVE_OPTION == fc.showOpenDialog(this.window)) {
                this.currentDir = fc.getCurrentDirectory();
                newFileName = fc.getSelectedFile().getAbsolutePath();
            }
            else {
            	return;
            }
        }
        
        // Anadimos la extension si es necesario
        for (final String ext : fExtensions) {
            if (newFileName.toLowerCase().endsWith(ext)) {
                nameMissingExtension = false;
            }
        }
        newFileName = newFileName + (nameMissingExtension ? fExtensions[0] : "");         
        
        final File outputFile = new File(newFileName);
        if (outputFile.exists()) {
        	if (JOptionPane.NO_OPTION == JOptionPane.showConfirmDialog(
    			SignPanel.this,
    			"Ya existe un fichero con ese nombre,\nÀDesea sobreescribirlo?",
    			"Advertencia",
    			JOptionPane.YES_NO_OPTION,
    			JOptionPane.WARNING_MESSAGE
			)) {
        		this.signButton.setEnabled(true);
        		return;
        	}
        }
        
        FileOutputStream fos = null;
        BufferedOutputStream bos = null;
        try {
            fos = new FileOutputStream(new File(newFileName));
            bos = new BufferedOutputStream(fos);
            bos.write(signResult);
        }
        catch(final Exception e) {
            Logger.getLogger("es.gob.afirma").severe(
                    "No se ha podido guardar el resultado de la firma: " + e
            );
            JOptionPane.showOptionDialog(
                    this, 
                    "No se ha podido guardar el resultado de la firma, aseguresé de\nque el fichero seleccionado no está abierto por otra aplicación y\nde que cuenta con los permisos adecuados y reintente la operación",
                    "Error",
                    JOptionPane.OK_OPTION,
                    JOptionPane.ERROR_MESSAGE,
                    null,
                    new Object[] { "Cerrar "},
                    null
            );
            this.signButton.setEnabled(true);
        }
        finally {
            try {
                if (bos != null) bos.flush();
            }
            catch(final Exception e) {}
            try {
                if (fos != null) fos.flush();
            }
            catch(final Exception e) {}
            try {
                if (bos != null) bos.close();
            }
            catch(final Exception e) {}
            try {
                if (fos != null) fos.close();
            }
            catch(final Exception e) {}
        }
		this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		this.saf.loadResultsPanel(signResult, newFileName, ksm.getCertificate(alias));
	}
	

}
