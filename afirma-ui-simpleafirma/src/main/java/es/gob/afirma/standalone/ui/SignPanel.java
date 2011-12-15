/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

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
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URI;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.util.Date;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.ProgressMonitor;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;
import javax.swing.WindowConstants;
import javax.swing.filechooser.FileFilter;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.batik.swing.JSVGCanvas;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.keystores.main.common.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.common.KeyStoreUtilities;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.signers.xmldsig.AOXMLDSigSigner;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.Messages;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.VisorFirma;

/** Panel de selecci&oacute;n y firma del fichero objetivo.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
@SuppressWarnings("restriction")
public final class SignPanel extends JPanel {

    private static final long serialVersionUID = -4828575650695534417L;

    private final JSVGCanvas fileTypeVectorIcon = new JSVGCanvas();

    private static final String DNIE_SIGNATURE_ALIAS = "CertFirmaDigital"; //$NON-NLS-1$
    private static final String FILE_ICON_PDF = "/resources/icon_pdf.svg"; //$NON-NLS-1$
    private static final String FILE_ICON_XML = "/resources/icon_xml.svg"; //$NON-NLS-1$
    private static final String FILE_ICON_BINARY = "/resources/icon_binary.svg"; //$NON-NLS-1$
    private static final String FILE_ICON_SIGN = "/resources/icon_sign.svg"; //$NON-NLS-1$

    AOSigner signer;
    byte[] dataToSign = null;

    JPanel filePanel;
    private JPanel lowerPanel;

    DropTarget dropTarget;

    final JFrame window;

    final JButton signButton = new JButton();
    final JButton selectButton = new JButton();

    final SimpleAfirma saf;
    
    ProgressMonitor pm = new ProgressMonitor(SignPanel.this, Messages.getString("SignPanel.15"), "", 0, 1000);  //$NON-NLS-1$//$NON-NLS-2$

    /** Indica si la operaci&oacute;n a realizar es una cofirma. */
    boolean cosign = false;

    File currentFile = null;

    private boolean isXML(final byte[] data) {
        try {
            DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new ByteArrayInputStream(data));
        }
        catch (final Exception e) {
            return false;
        }
        return true;
    }

    /** Carga el fichero a firmar.
     * @param filename Nombre (ruta completa incuida) del fichero a firmar
     * @throws IOException Si ocurre alg&uacute;n problema durante la apertura o lectura del fichero */
    public void loadFile(final String filename) throws IOException {

        this.setCursor(new Cursor(Cursor.WAIT_CURSOR));

        final File file = new File(filename);

        String errorMessage = null;
        if (!file.exists()) {
            errorMessage = Messages.getString("SignPanel.3"); //$NON-NLS-1$
        }
        else if (!file.canRead()) {
            errorMessage = Messages.getString("SignPanel.7"); //$NON-NLS-1$
        }
        else if (file.length() < 1) {
            errorMessage = Messages.getString("SignPanel.5"); //$NON-NLS-1$
        }
        if (errorMessage != null) {
            UIUtils.showErrorMessage(
                    SignPanel.this,
                    errorMessage,
                    Messages.getString("SignPanel.25"), //$NON-NLS-1$
                    JOptionPane.ERROR_MESSAGE
            );
            return;
        }

        final InputStream fis = new FileInputStream(file);

        final byte[] data = AOUtil.getDataFromInputStream(fis);
        if (data == null || data.length < 1) {
            throw new IOException("No se ha podido leer el fichero"); //$NON-NLS-1$
        }
        this.dataToSign = data;

        try {
            fis.close();
        }
        catch (final Exception e) { /* Ignoramos los errores */ }

        String fileDescription;
        String iconPath;
        String iconTooltip;
        this.cosign = false;
        // Comprobamos si es un fichero PDF
        if (new AOPDFSigner().isValidDataFile(data)) {
            iconPath = FILE_ICON_PDF;
            iconTooltip = Messages.getString("SignPanel.0"); //$NON-NLS-1$
            fileDescription = Messages.getString("SignPanel.9"); //$NON-NLS-1$
            this.signer = new AOPDFSigner();
        }
        // Comprobamos si es un fichero de firma (los PDF pasaran por la condicion anterior)
        else if ((this.signer = AOSignerFactory.getSigner(data)) != null) {
            AOSignInfo info = null;
            try {
                info = this.signer.getSignInfo(data);
            }
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").warning("no se pudo extraer la informacion de firma: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            }
            iconPath = FILE_ICON_SIGN;
            iconTooltip = Messages.getString("SignPanel.6") + (info != null ? info.getFormat() : ""); //$NON-NLS-1$ //$NON-NLS-2$
            fileDescription = "Documento de firma"; //$NON-NLS-1$
            this.cosign = true;
        }
        // Comprobamos si es un fichero XML
        else if (isXML(data)) {
            iconPath = FILE_ICON_XML;
            iconTooltip = Messages.getString("SignPanel.8"); //$NON-NLS-1$
            fileDescription = Messages.getString("SignPanel.10"); //$NON-NLS-1$
            this.signer = new AOXAdESSigner();
        }
        // Cualquier otro tipo de fichero
        else {
            iconPath = FILE_ICON_BINARY;
            iconTooltip = Messages.getString("SignPanel.12"); //$NON-NLS-1$
            fileDescription = Messages.getString("SignPanel.11"); //$NON-NLS-1$
            this.signer = new AOCAdESSigner();
        }

        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        try {
            this.fileTypeVectorIcon.setDocument(dbf.newDocumentBuilder().parse(this.getClass().getResourceAsStream(iconPath)));
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
            "No se ha podido cargar el icono del tipo de fichero/firma, este no se mostrara: " + e //$NON-NLS-1$
            );
        }
        this.fileTypeVectorIcon.setFocusable(false);
        this.fileTypeVectorIcon.setToolTipText(iconTooltip);

        final double fileSize = file.length() / 1024;
        final long fileLastModified = file.lastModified();

        this.lowerPanel.remove(this.filePanel);
        this.filePanel =
                new FilePanel(this.fileTypeVectorIcon,
                              NumberFormat.getInstance().format(fileSize),
                              filename,
                              fileDescription,
                              new Date(fileLastModified));
        this.lowerPanel.add(this.filePanel);
        this.lowerPanel.revalidate();

        if (this.window != null) {
            this.window.getRootPane().putClientProperty("Window.documentFile", new File(filename)); //$NON-NLS-1$
            this.window.setTitle(Messages.getString("SimpleAfirma.10") + " - " + new File(filename).getName()); //$NON-NLS-1$ //$NON-NLS-2$
        }

        this.currentFile = file;

        if (this.saf.isKeyStoreReady()) {
            setSignCommandEnabled(true);
        }
        else {
            this.signButton.setText(""); //$NON-NLS-1$
            this.signButton.setIcon(new ImageIcon(this.getClass().getResource("/resources/progress.gif"))); //$NON-NLS-1$
            this.signButton.setToolTipText(Messages.getString("SignPanel.13")); //$NON-NLS-1$
        }

        this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        this.signButton.requestFocusInWindow();
    }

    private void createUI(final ActionListener al, final boolean firstTime) {
        if (!LookAndFeelManager.HIGH_CONTRAST) {
            this.setBackground(LookAndFeelManager.WINDOW_COLOR);
        }
        this.setLayout(new GridLayout(2, 1));
        this.add(new UpperPanel(al, firstTime));
        this.lowerPanel = new LowerPanel(al);
        this.add(this.lowerPanel);
        this.dropTarget = new DropTarget(this.filePanel, DnDConstants.ACTION_COPY, new DropTargetListener() {

            @Override
            public void dropActionChanged(final DropTargetDragEvent dtde) { /* No implementado */}

            @Override
            public void dragOver(final DropTargetDragEvent dtde) { /* No implementado */ }

            @Override
            public void dragExit(final DropTargetEvent dte) { /* No implementado */ }

            @Override
            public void drop(final DropTargetDropEvent dtde) {

                final Transferable tr = dtde.getTransferable();
                if (tr.isDataFlavorSupported(DataFlavor.javaFileListFlavor)) {
                    dtde.acceptDrop(DnDConstants.ACTION_COPY);
                    final Object transData;
                    try {
                        transData = tr.getTransferData(DataFlavor.javaFileListFlavor);
                    }
                    catch (final Exception e) {
                        Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
                        "Ha fallado la operacion de arrastrar y soltar: " + e //$NON-NLS-1$
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
                            UIUtils.showErrorMessage(
                                    SignPanel.this,
                                    Messages.getString("SignPanel.18"), //$NON-NLS-1$
                                    Messages.getString("SignPanel.19"), //$NON-NLS-1$
                                    JOptionPane.WARNING_MESSAGE
                            );
                        }
                        String fileName = fileList.get(0).toString();
                        if (fileName.startsWith("http://") || //$NON-NLS-1$
                            fileName.startsWith("https://") || //$NON-NLS-1$
                            fileName.startsWith("ftp://") //$NON-NLS-1$
                        ) {
                            UIUtils.showErrorMessage(
                                    SignPanel.this,
                                    Messages.getString("SignPanel.24"), //$NON-NLS-1$
                                    Messages.getString("SignPanel.25"), //$NON-NLS-1$
                                    JOptionPane.ERROR_MESSAGE
                            );
                            dtde.dropComplete(false);
                            return;
                        }
                        else if (fileName.startsWith("file://")) { //$NON-NLS-1$
                            try {
                                fileName = new File(new URI(fileName)).getPath();
                            }
                            catch (final Exception e) {
                                Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
                                "Ha fallado la operacion de arrastrar y soltar al obtener la ruta del fichero arrastrado: " + e //$NON-NLS-1$
                                );
                                dtde.dropComplete(false);
                                return;
                            }
                        }
                        try {
                            loadFile(fileName);
                        }
                        catch (final IOException e) {
                            Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
                            "Ha fallado la operacion de arrastrar y soltar al cargar el fichero arrastrado: " + e //$NON-NLS-1$
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
        }, true);
        setVisible(true);
    }

    /** Construye el panel de firma, en el que se selecciona y se firma un fichero.
     * @param win Ventana de primer nivel, para el cambio de t&iacute;tulo en la carga de fichero
     * @param sa Clase principal, para proporcionar el <code>AOKeyStoreManager</code> necesario para
     *        realizar las firmas y cambiar de panel al finalizar una firma
     * @param firstTime <code>true</code> si se la primera vez que se muestra este panel en la
     * aplicaci&oacute;n, <code>false</code> en caso contrario
     */
    public SignPanel(final JFrame win, final SimpleAfirma sa, final boolean firstTime) {
        super(true);
        this.window = win;
        this.saf = sa;
        createUI(null, firstTime);
    }

    private final class UpperPanel extends JPanel {

        private static final long serialVersionUID = 533243192995645135L;

        UpperPanel(final ActionListener al, final boolean firstTime) {
            super(true);
            createUI(al, firstTime);
        }
        
        private void createUI(final ActionListener al, final boolean firstTime) {
            this.setLayout(new BorderLayout(5, 5));
            this.setBorder(BorderFactory.createEmptyBorder(15, 15, 15, 15));
            
            SignPanel.this.selectButton.setText(Messages.getString("SignPanel.32")); //$NON-NLS-1$
            if (al != null) {
                SignPanel.this.selectButton.addActionListener(al);
            }
            SignPanel.this.selectButton.setMnemonic('S');
            SignPanel.this.selectButton.getAccessibleContext().setAccessibleDescription(Messages.getString("SignPanel.33") //$NON-NLS-1$
                                       );
            SignPanel.this.selectButton.getAccessibleContext().setAccessibleName(Messages.getString("SignPanel.34") //$NON-NLS-1$
                                       );
            SignPanel.this.selectButton.requestFocusInWindow();
            SignPanel.this.selectButton.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent arg0) {

                    String fileToLoad;

                    if (Platform.OS.MACOSX.equals(Platform.getOS()) || Platform.OS.WINDOWS.equals(Platform.getOS())) {
                        if (SignPanel.this.saf.getCurrentDir() == null) {
                            SignPanel.this.saf.setCurrentDir(new File(Platform.getUserHome()));
                        }
                        final FileDialog fd = new FileDialog((Frame) null, Messages.getString("SignPanel.35")); //$NON-NLS-1$
                        fd.setDirectory(SignPanel.this.saf.getCurrentDir().getAbsolutePath());
                        fd.setVisible(true);
                        if (fd.getFile() == null) {
                            return;
                        }
                        SignPanel.this.saf.setCurrentDir(new File(fd.getDirectory()));
                        fileToLoad = fd.getDirectory() + fd.getFile();
                    }
                    else {
                        final JFileChooser fc = new JFileChooser();
                        if (SignPanel.this.saf.getCurrentDir() != null) {
                            fc.setCurrentDirectory(SignPanel.this.saf.getCurrentDir());
                        }
                        if (JFileChooser.APPROVE_OPTION == fc.showOpenDialog(UpperPanel.this)) {
                            SignPanel.this.saf.setCurrentDir(fc.getCurrentDirectory());
                            fileToLoad = fc.getSelectedFile().getAbsolutePath();
                        }
                        else {
                            return;
                        }
                    }

                    try {
                        loadFile(fileToLoad);
                    }
                    catch (final Exception e) {
                        UIUtils.showErrorMessage(
                                UpperPanel.this,
                                Messages.getString("SignPanel.36"), //$NON-NLS-1$
                                Messages.getString("SignPanel.25"), //$NON-NLS-1$
                                JOptionPane.ERROR_MESSAGE
                        );
                    }

                }
            });

            final JLabel welcomeLabel = new JLabel(((firstTime) ? (Messages.getString("SignPanel.14") + " ") : ("")) + Messages.getString("SignPanel.39")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
            welcomeLabel.setFocusable(false);
            welcomeLabel.setFont(welcomeLabel.getFont().deriveFont(Font.PLAIN, 26));
            welcomeLabel.setLabelFor(SignPanel.this.selectButton);
            this.add(welcomeLabel, BorderLayout.PAGE_START);

            String intro = Messages.getString("SignPanel.40"); //$NON-NLS-1$
            
            try {
                int nReaders = javax.smartcardio.TerminalFactory.getDefault().terminals().list().size();
                if (nReaders == 1) {
                    intro = intro + Messages.getString("SignPanel.2"); //$NON-NLS-1$
                }
                else if (nReaders > 1) {
                    intro = intro + Messages.getString("SignPanel.4"); //$NON-NLS-1$
                }
            }
            catch(Exception e) { /* Ignoramos los errores */ }
            
            final JLabel introText = new JLabel(intro);
            introText.setLabelFor(SignPanel.this.selectButton);
            introText.setFocusable(false);

            final JPanel introPanel = new JPanel(new BorderLayout());
            introPanel.add(introText, BorderLayout.PAGE_START);
            this.add(introPanel, BorderLayout.CENTER);

            final JPanel selectPanel = new JPanel(new FlowLayout(FlowLayout.LEFT), true);
            selectPanel.add(SignPanel.this.selectButton);
            this.add(selectPanel, BorderLayout.PAGE_END);

            // Configuramos el color
            if (!LookAndFeelManager.HIGH_CONTRAST) {
                this.setBackground(LookAndFeelManager.WINDOW_COLOR);
                introPanel.setBackground(LookAndFeelManager.WINDOW_COLOR);
                selectPanel.setBackground(LookAndFeelManager.WINDOW_COLOR);
                welcomeLabel.setForeground(new Color(3399));
            }
           
        }
    }

    private final class LowerPanel extends JPanel {

        private static final long serialVersionUID = 533243192995645135L;

        LowerPanel(final ActionListener al) {
            super(true);
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    createUI(al);
                }
            });
        }

        void createUI(final ActionListener al) {
            this.setLayout(new BorderLayout(5, 5));
            this.setBorder(BorderFactory.createEmptyBorder(15, 15, 15, 15));

            SignPanel.this.filePanel = new ResizingTextPanel(Messages.getString("SignPanel.41")); //$NON-NLS-1$
            SignPanel.this.filePanel.getAccessibleContext().setAccessibleDescription(Messages.getString("SignPanel.42")); //$NON-NLS-1$
            SignPanel.this.filePanel.getAccessibleContext().setAccessibleName(Messages.getString("SignPanel.43")); //$NON-NLS-1$
            SignPanel.this.filePanel.setToolTipText(Messages.getString("SignPanel.42")); //$NON-NLS-1$
            SignPanel.this.filePanel.setFocusable(false);
            SignPanel.this.filePanel.setDropTarget(SignPanel.this.dropTarget);

            this.add(SignPanel.this.filePanel, BorderLayout.CENTER);

            final JPanel buttonPanel = new JPanel(true);
            SignPanel.this.signButton.setPreferredSize(new Dimension(160, 27));
            if (!SignPanel.this.cosign) {
                SignPanel.this.signButton.setText(Messages.getString("SignPanel.45")); //$NON-NLS-1$
            }
            else {
                SignPanel.this.signButton.setText(Messages.getString("SignPanel.16")); //$NON-NLS-1$
            }
            SignPanel.this.signButton.setMnemonic('F');
            if (al != null) {
                SignPanel.this.signButton.addActionListener(al);
            }
            SignPanel.this.signButton.setEnabled(false);
            buttonPanel.add(SignPanel.this.signButton);
            SignPanel.this.signButton.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent ae) {
                    sign();
                }
            });

            // Establecemos la configuracion de color
            if (!LookAndFeelManager.HIGH_CONTRAST) {
                this.setBackground(LookAndFeelManager.WINDOW_COLOR);
                SignPanel.this.filePanel.setBackground(Color.DARK_GRAY);
                SignPanel.this.filePanel.setForeground(Color.LIGHT_GRAY);
                buttonPanel.setBackground(LookAndFeelManager.WINDOW_COLOR);
            }
            
            this.add(buttonPanel, BorderLayout.PAGE_END);
        }
    }

    private final class FilePanel extends JPanel {

        private static final long serialVersionUID = -8648491975442788750L;

        FilePanel(final Component icon,
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

        void createUI(final Component icon,
                              final String fileSize,
                              final String filePath,
                              final String fileDescription,
                              final Date fileLastModified) {
            
            this.setBorder(BorderFactory.createLineBorder(Color.black));
            this.setLayout(new GridBagLayout());

            final JLabel pathLabel = new JLabel(filePath);
            pathLabel.setFont(pathLabel.getFont().deriveFont(Font.BOLD, pathLabel.getFont().getSize() + 3f));

            final JLabel descLabel = new JLabel(Messages.getString("SignPanel.46") + fileDescription); //$NON-NLS-1$
            final JLabel dateLabel = new JLabel(Messages.getString("SignPanel.47") + //$NON-NLS-1$
                                                DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.SHORT).format(fileLastModified));

            final JLabel sizeLabel = new JLabel(Messages.getString("SignPanel.49") + fileSize + " KB"); //$NON-NLS-1$ //$NON-NLS-2$

            final JPanel detailPanel = new JPanel();
            detailPanel.setLayout(new BoxLayout(detailPanel, BoxLayout.Y_AXIS));
            detailPanel.add(pathLabel);
            detailPanel.add(Box.createRigidArea(new Dimension(0, 8)));
            detailPanel.add(descLabel);
            detailPanel.add(dateLabel);
            detailPanel.add(Box.createRigidArea(new Dimension(0, 8)));
            detailPanel.add(sizeLabel);

            // Puede arrastrarse un fichero a cualquiera de estos componentes para cargarlo
            this.setDropTarget(SignPanel.this.dropTarget);

            // Establecemos la configuracion de color
            if (!LookAndFeelManager.HIGH_CONTRAST) {
                this.setBackground(LookAndFeelManager.WINDOW_COLOR);
                detailPanel.setBackground(LookAndFeelManager.WINDOW_COLOR);
            }
            
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
            c.insets = new Insets(14, 0, 11, 5);
            c.anchor = GridBagConstraints.NORTH;
            this.add(detailPanel, c);
            c.fill = GridBagConstraints.HORIZONTAL;
            c.weightx = 0.0;
            c.weighty = 0.0;
            c.gridx = 2;
            c.insets = new Insets(11, 6, 11, 11);
            c.anchor = GridBagConstraints.NORTHEAST;
            
            // Si es una firma la abriremos desde el mismo aplicativo
            final boolean isSign = filePath.endsWith(".csig") || filePath.endsWith(".xsig"); //$NON-NLS-1$ //$NON-NLS-2$
            
            if (isSign || UIUtils.hasAssociatedApplication(filePath.substring(filePath.lastIndexOf(".")))) { //$NON-NLS-1$
                final JButton openFileButton = new JButton(Messages.getString("SignPanel.51")); //$NON-NLS-1$
                openFileButton.setMnemonic('v');
                openFileButton.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(final ActionEvent ae) {
                        if (isSign) {
                            new VisorFirma(new File(filePath), false).initialize(false, null);
                        }
                        else {
                            try {
                                Desktop.getDesktop().open(new File(filePath));
                            }
                            catch (final IOException e) {
                                UIUtils.showErrorMessage(
                                        FilePanel.this,
                                        Messages.getString("SignPanel.53"), //$NON-NLS-1$
                                        Messages.getString("SignPanel.25"), //$NON-NLS-1$
                                        JOptionPane.ERROR_MESSAGE
                                );
                                return;
                            }
                        }
                    }
                });
                this.add(openFileButton, c);
                pathLabel.setLabelFor(openFileButton);
                descLabel.setLabelFor(openFileButton);
                dateLabel.setLabelFor(openFileButton);
                sizeLabel.setLabelFor(openFileButton);
            }
        }
    }

    /** Firma el fichero actualmente cargado. */
    public void sign() {
        new SignTask().execute();
    }

    /** M&eacute;todo para indicar a la clase que el <code>AOKeyStoreManager</code> est&aacute; listo para usarse. */
    public void notifyStoreReady() {
        if (this.dataToSign != null) {
            setSignCommandEnabled(true);
        }
    }

    void setSignCommandEnabled(final boolean e) {
        if (e) {
            this.signButton.setIcon(null);
            if (!this.cosign) {
                this.signButton.setText(Messages.getString("SignPanel.45")); //$NON-NLS-1$
            }
            else {
                SignPanel.this.signButton.setText(Messages.getString("SignPanel.16")); //$NON-NLS-1$
            }
            this.signButton.setToolTipText(null);
        }
        this.signButton.setEnabled(e);
        this.saf.setSignMenuCommandEnabled(e);
    }
    
    private final class SignTask extends SwingWorker<Void, Void> {
        
        public SignTask() {
            super();
        }

        @Override
        public Void doInBackground() {
            
            SignPanel.this.setCursor(new Cursor(Cursor.WAIT_CURSOR));
            
            if (SignPanel.this.signer == null || SignPanel.this.dataToSign == null || SignPanel.this.saf == null) {
                return null;
            }
            final AOKeyStoreManager ksm = SignPanel.this.saf.getAOKeyStoreManager();
            String alias = null;

            try {
                if (ksm.getKeyStores().get(0).containsAlias(DNIE_SIGNATURE_ALIAS) || ksm.getCertificate(DNIE_SIGNATURE_ALIAS).getIssuerX500Principal().toString().contains("DNIE")) { //$NON-NLS-1$
                    alias = DNIE_SIGNATURE_ALIAS;
                }
            }
            catch(final Exception e) { /* Ignoramos los errores */ }

            if (alias == null) {
                try {
                    alias = KeyStoreUtilities.showCertSelectionDialog(ksm.getAliases(), ksm,
                                                                SignPanel.this,
                                                                true,
                                                                true,
                                                                false);
                }
                catch (final AOCertificatesNotFoundException e) {
                    UIUtils.showErrorMessage(
                            SignPanel.this,
                            Messages.getString("SignPanel.55"), //$NON-NLS-1$
                            Messages.getString("SignPanel.19"), //$NON-NLS-1$
                            JOptionPane.WARNING_MESSAGE
                    );
                    return null;
                }
                catch (final AOCancelledOperationException e) {
                    return null;
                }
                finally {
                    SignPanel.this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                }
            }

            final Properties p = new Properties();
            p.put("mode", "implicit"); //$NON-NLS-1$ //$NON-NLS-2$
            p.put("signingCertificateV2", "true"); //$NON-NLS-1$ //$NON-NLS-2$
            p.put("ignoreStyleSheets", "false"); //$NON-NLS-1$ //$NON-NLS-2$

            setSignCommandEnabled(false);
            
            SignPanel.this.setCursor(new Cursor(Cursor.WAIT_CURSOR));
            
            SignPanel.this.pm.setProgress(999);
            
            // Deshabilitamos la posibilidad de cancelar el dialogo de espera
            try {
                final JDialog dialog = (JDialog) SignPanel.this.pm.getAccessibleContext().getAccessibleParent();
                dialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
                ((JOptionPane)dialog.getContentPane().getComponent(0)).setOptions(new Object[]{});
            }
            catch(final Exception e) { /* Ignoramos los errores */ }
            
            try {
                final Field jProgressBarField = ProgressMonitor.class.getDeclaredField("myBar"); //$NON-NLS-1$
                jProgressBarField.setAccessible(true);
                JProgressBar progressBar = (JProgressBar) jProgressBarField.get(SignPanel.this.pm);
                
                final Method setIndeterminateMethod = JProgressBar.class.getMethod("setIndeterminate", Boolean.TYPE); //$NON-NLS-1$
                setIndeterminateMethod.invoke(progressBar, Boolean.TRUE);
                
                if (Platform.OS.MACOSX.equals(Platform.getOS())) {
                    final Method putCLientPropertyMethod = JProgressBar.class.getMethod("putClientProperty", Object.class, Object.class); //$NON-NLS-1$
                    putCLientPropertyMethod.invoke(progressBar, "JProgressBar.style", "circular"); //$NON-NLS-1$ //$NON-NLS-2$
                }

            } 
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
                        "No se ha podido mostrar la barra de progreso indeterminado: " + e); //$NON-NLS-1$
            }
            
            try { Thread.sleep(5000); } catch(Exception e) { /* Ignoramos los errores */ }

            final byte[] signResult;
            try {
                if (SignPanel.this.cosign) {
                    signResult = SignPanel.this.signer.cosign(SignPanel.this.dataToSign, "SHA1withRSA", //$NON-NLS-1$
                                              ksm.getKeyEntry(alias, KeyStoreUtilities.getPreferredPCB(ksm.getType(), SignPanel.this)),
                                              p);
                }
                else {
                    signResult = SignPanel.this.signer.sign(SignPanel.this.dataToSign, "SHA1withRSA", //$NON-NLS-1$
                            ksm.getKeyEntry(alias, KeyStoreUtilities.getPreferredPCB(ksm.getType(), SignPanel.this)),
                            p);
                }
            }
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de firma: " + e); //$NON-NLS-1$ //$NON-NLS-2$
                UIUtils.showErrorMessage(
                        SignPanel.this,
                        Messages.getString("SignPanel.65"), //$NON-NLS-1$
                        Messages.getString("SignPanel.25"), //$NON-NLS-1$
                        JOptionPane.ERROR_MESSAGE
                );
                setSignCommandEnabled(true);
                return null;
            }
            catch(final OutOfMemoryError ooe) {
                Logger.getLogger("es.gob.afirma").severe("Falta de memoria en el proceso de firma: " + ooe); //$NON-NLS-1$ //$NON-NLS-2$
                UIUtils.showErrorMessage(
                    SignPanel.this,
                    Messages.getString("SignPanel.1"), //$NON-NLS-1$
                    Messages.getString("SignPanel.25"), //$NON-NLS-1$
                    JOptionPane.ERROR_MESSAGE
                );
                setSignCommandEnabled(true);
                return null;
            }
            finally {
                SignPanel.this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                SignPanel.this.pm.setProgress(1000);
            }

            String newFileName = SignPanel.this.currentFile.getName();
            String[] filterExtensions;
            String filterDescription;
            if (SignPanel.this.signer instanceof AOPDFSigner) {
                if (!newFileName.toLowerCase().endsWith(".pdf")) { //$NON-NLS-1$
                    newFileName = newFileName + ".pdf"; //$NON-NLS-1$
                }
                newFileName = newFileName.substring(0, newFileName.length() - 4).replace(".", "_") + "_signed.pdf"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                filterExtensions = new String[] {
                    ".pdf"}; //$NON-NLS-1$
                filterDescription = Messages.getString("SignPanel.72"); //$NON-NLS-1$
            }
            else if (SignPanel.this.signer instanceof AOXMLDSigSigner || SignPanel.this.signer instanceof AOXAdESSigner) {
                newFileName = newFileName.replace(".", "_") + ".xsig"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                filterExtensions = new String[] {
                        ".xsig", ".xml"}; //$NON-NLS-1$ //$NON-NLS-2$
                filterDescription = Messages.getString("SignPanel.76"); //$NON-NLS-1$
            }
            else {
                newFileName = newFileName.replace(".", "_") + ".csig"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                filterExtensions = new String[] {
                        ".csig", ".p7s"}; //$NON-NLS-1$ //$NON-NLS-2$
                filterDescription = Messages.getString("SignPanel.80"); //$NON-NLS-1$
            }

            final String fDescription = filterDescription;
            final String[] fExtensions = filterExtensions;

            // Para las comprobaciones de si el usuario ha teclado la extension o espera que
            // la aplicacion la anada sola
            boolean nameMissingExtension = true;

            if (Platform.OS.MACOSX.equals(Platform.getOS()) || Platform.OS.WINDOWS.equals(Platform.getOS())) {
                if (SignPanel.this.saf.getCurrentDir() == null) {
                    SignPanel.this.saf.setCurrentDir(new File(Platform.getUserHome()));
                }
                final FileDialog fd = new FileDialog(SignPanel.this.window, Messages.getString("SignPanel.81"), FileDialog.SAVE); //$NON-NLS-1$
                fd.setDirectory(SignPanel.this.saf.getCurrentDir().getAbsolutePath());
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
                if (fd.getFile() == null) {
                    setSignCommandEnabled(true);
                    return null;
                }
                SignPanel.this.saf.setCurrentDir(new File(fd.getDirectory()));
                newFileName = fd.getDirectory() + fd.getFile();
            }
            else {
                final JFileChooser fc = new JFileChooser();
                if (SignPanel.this.saf.getCurrentDir() != null) {
                    fc.setCurrentDirectory(SignPanel.this.saf.getCurrentDir());
                }
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
                if (JFileChooser.APPROVE_OPTION == fc.showSaveDialog(SignPanel.this.window)) {
                    SignPanel.this.saf.setCurrentDir(fc.getCurrentDirectory());
                    newFileName = fc.getSelectedFile().getAbsolutePath();
                }
                else {
                    setSignCommandEnabled(true);
                    return null;
                }
            }

            // Anadimos la extension si es necesario
            for (final String ext : fExtensions) {
                if (newFileName.toLowerCase().endsWith(ext)) {
                    nameMissingExtension = false;
                }
            }
            newFileName = newFileName + (nameMissingExtension ? fExtensions[0] : ""); //$NON-NLS-1$

            // Cuando se usa un FileDialog la confirmacion de sobreescritura la gestiona
            // el sistema operativo, pero en Mac hay comportamiento extrano con la extension

            final File outputFile = new File(newFileName);

            OutputStream fos = null;
            OutputStream bos = null;
            try {
                fos = new FileOutputStream(outputFile);
                bos = new BufferedOutputStream(fos);
                bos.write(signResult);
            }
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
                  "No se ha podido guardar el resultado de la firma: " + e //$NON-NLS-1$
                );
                UIUtils.showErrorMessage(
                        SignPanel.this,
                        Messages.getString("SignPanel.88"), //$NON-NLS-1$
                        Messages.getString("SignPanel.25"), //$NON-NLS-1$
                        JOptionPane.ERROR_MESSAGE
                );
                setSignCommandEnabled(true);
            }
            finally {
                try {
                    if (bos != null) {
                        bos.flush();
                    }
                }
                catch (final Exception e) { /* Ignoramos los errores */ }
                try {
                    if (fos != null) {
                        fos.flush();
                    }
                }
                catch (final Exception e) { /* Ignoramos los errores */ }
                try {
                    if (bos != null) {
                        bos.close();
                    }
                }
                catch (final Exception e) { /* Ignoramos los errores */ }
                try {
                    if (fos != null) {
                        fos.close();
                    }
                }
                catch (final Exception e) { /* Ignoramos los errores */ }
            }
            SignPanel.this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            SignPanel.this.saf.loadResultsPanel(signResult, newFileName, ksm.getCertificate(alias));
        
            return null;
        }
        
    }
    
}
