/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
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
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URI;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.util.Date;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;
import java.util.prefs.Preferences;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.ProgressMonitor;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;
import javax.swing.WindowConstants;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.batik.swing.JSVGCanvas;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.pades.BadPdfPasswordException;
import es.gob.afirma.signers.pades.PdfIsCertifiedException;
import es.gob.afirma.signers.xades.AOFacturaESigner;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.standalone.DataAnalizerUtil;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.PreferencesNames;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.VisorFirma;

/** Panel de selecci&oacute;n y firma del fichero objetivo.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class SignPanel extends JPanel {

    static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final long serialVersionUID = -4828575650695534417L;

    private final JSVGCanvas fileTypeVectorIcon = new JSVGCanvas();

    private static final String FILE_ICON_PDF = "/resources/icon_pdf.svg"; //$NON-NLS-1$
    private static final String FILE_ICON_XML = "/resources/icon_xml.svg"; //$NON-NLS-1$
    private static final String FILE_ICON_BINARY = "/resources/icon_binary.svg"; //$NON-NLS-1$
    private static final String FILE_ICON_SIGN = "/resources/icon_sign.svg"; //$NON-NLS-1$
    private static final String FILE_ICON_FACTURAE = "/resources/icon_facturae.svg"; //$NON-NLS-1$

    private AOSigner signer;

    AOSigner getSigner() {
    	return this.signer;
    }

    private byte[] dataToSign = null;

    byte[] getDataToSign() {
    	return this.dataToSign;
    }

    private JPanel filePanel;

    JPanel getFilePanel() {
    	return this.filePanel;
    }

    void setFilePanel(final JPanel panel) {
    	this.filePanel = panel;
    }

    private JPanel lowerPanel;

    private DropTarget dropTarget;

	DropTarget getDropTgt() {
    	return this.dropTarget;
    }

    private final JFrame window;

    JFrame getWindow() {
    	return this.window;
    }

    private final JButton signButton = new JButton();

    JButton getSignButton() {
    	return this.signButton;
    }

    private final JButton selectButton = new JButton();

    JButton getSelectButton() {
    	return this.selectButton;
    }

    private final SimpleAfirma saf;

    SimpleAfirma getSimpleAfirma() {
    	return this.saf;
    }

    private final ProgressMonitor pm = new ProgressMonitor(SignPanel.this, SimpleAfirmaMessages.getString("SignPanel.15"), "", 0, 1000);  //$NON-NLS-1$//$NON-NLS-2$

    ProgressMonitor getProgressMonitor() {
    	return this.pm;
    }

    /** Indica si la operaci&oacute;n a realizar es una cofirma. */
    private boolean cosign = false;

    boolean isCosign() {
    	return this.cosign;
    }

    private File currentFile = null;

    File getCurrentFile() {
    	return this.currentFile;
    }

    /** Carga el fichero a firmar.
     * @param file Fichero a firmar
     * @throws IOException Si ocurre alg&uacute;n problema durante la apertura o lectura del fichero */
    public void loadFile(final File file) throws IOException {

        this.setCursor(new Cursor(Cursor.WAIT_CURSOR));

        String errorMessage = null;
        if (!file.exists()) {
            errorMessage = SimpleAfirmaMessages.getString("SignPanel.3"); //$NON-NLS-1$
        }
        else if (file.isDirectory()) {
        	errorMessage = SimpleAfirmaMessages.getString("SignPanel.21"); //$NON-NLS-1$
        }
        else if (!file.canRead()) {
            errorMessage = SimpleAfirmaMessages.getString("SignPanel.7"); //$NON-NLS-1$
        }
        else if (file.length() < 1) {
            errorMessage = SimpleAfirmaMessages.getString("SignPanel.5"); //$NON-NLS-1$
        }
        if (errorMessage != null) {
            UIUtils.showErrorMessage(
                    SignPanel.this,
                    errorMessage,
                    SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                    JOptionPane.ERROR_MESSAGE
            );
            this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            return;
        }

        final InputStream fis = new FileInputStream(file);

        final byte[] data;
        try {
            data = AOUtil.getDataFromInputStream(fis);
        }
        catch(final OutOfMemoryError e) {
            UIUtils.showErrorMessage(
                 SignPanel.this,
                 SimpleAfirmaMessages.getString("SignPanel.26"), //$NON-NLS-1$
                 SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                 JOptionPane.ERROR_MESSAGE
            );
            this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            return;
        }
        finally {
            fis.close();
        }
        if (data == null || data.length < 1) {
            throw new IOException("No se ha podido leer el fichero"); //$NON-NLS-1$
        }
        this.dataToSign = data;


        String fileDescription;
        String iconPath;
        String iconTooltip;
        this.cosign = false;
        // Comprobamos si es un fichero PDF
        if (DataAnalizerUtil.isPDF(data)) {
            iconPath = FILE_ICON_PDF;
            iconTooltip = SimpleAfirmaMessages.getString("SignPanel.0"); //$NON-NLS-1$
            fileDescription = SimpleAfirmaMessages.getString("SignPanel.9"); //$NON-NLS-1$
            this.signer = new AOPDFSigner();
        }
        // Comprobamos si es una factura electronica
        else if (DataAnalizerUtil.isFacturae(data)) {
        	if (new AOFacturaESigner().isSign(data)) {
        		UIUtils.showErrorMessage(this,
        				SimpleAfirmaMessages.getString("SignPanel.22"), //$NON-NLS-1$
        				SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
        				JOptionPane.WARNING_MESSAGE);
        		this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        		return;
        	}
        	iconPath = FILE_ICON_FACTURAE;
        	iconTooltip = SimpleAfirmaMessages.getString("SignPanel.17"); //$NON-NLS-1$
        	fileDescription = SimpleAfirmaMessages.getString("SignPanel.20"); //$NON-NLS-1$
        	this.signer = new AOFacturaESigner();
        }
        // Comprobamos si es un fichero de firma (los PDF y las facturas pasaran por la condicion anterior)
        else if ((this.signer = AOSignerFactory.getSigner(data)) != null) {
            AOSignInfo info = null;
            try {
                info = this.signer.getSignInfo(data);
            }
            catch (final Exception e) {
                LOGGER.warning("no se pudo extraer la informacion de firma: " + e); //$NON-NLS-1$
            }
            iconPath = FILE_ICON_SIGN;
            iconTooltip = SimpleAfirmaMessages.getString("SignPanel.6") + (info != null ? info.getFormat() : ""); //$NON-NLS-1$ //$NON-NLS-2$
            fileDescription = "Documento de firma"; //$NON-NLS-1$
            this.cosign = true;
        }
        // Comprobamos si es un fichero XML
        else if (DataAnalizerUtil.isXML(data)) {
            iconPath = FILE_ICON_XML;
            iconTooltip = SimpleAfirmaMessages.getString("SignPanel.8"); //$NON-NLS-1$
            fileDescription = SimpleAfirmaMessages.getString("SignPanel.10"); //$NON-NLS-1$
            this.signer = new AOXAdESSigner();
        }
        // Cualquier otro tipo de fichero
        else {
            iconPath = FILE_ICON_BINARY;
            iconTooltip = SimpleAfirmaMessages.getString("SignPanel.12"); //$NON-NLS-1$
            fileDescription = SimpleAfirmaMessages.getString("SignPanel.11"); //$NON-NLS-1$
            this.signer = new AOCAdESSigner();
        }

        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        try {
            this.fileTypeVectorIcon.setDocument(dbf.newDocumentBuilder().parse(this.getClass().getResourceAsStream(iconPath)));
        }
        catch (final Exception e) {
            LOGGER.warning(
                "No se ha podido cargar el icono del tipo de fichero/firma, este no se mostrara: " + e //$NON-NLS-1$
            );
        }
        this.fileTypeVectorIcon.setFocusable(false);
        this.fileTypeVectorIcon.setToolTipText(iconTooltip);
        this.fileTypeVectorIcon.setBackground(new Color(255, 255, 255, 0));

        final double fileSize = file.length() / 1024;
        final long fileLastModified = file.lastModified();

        this.lowerPanel.remove(this.filePanel);
        this.filePanel = new FilePanel(this.fileTypeVectorIcon,
              NumberFormat.getInstance().format(fileSize),
              file,
              fileDescription,
              new Date(fileLastModified)
        );
        this.lowerPanel.add(this.filePanel);
        this.lowerPanel.revalidate();

        if (this.window != null) {
            this.window.getRootPane().putClientProperty("Window.documentFile", file); //$NON-NLS-1$
            this.window.setTitle(SimpleAfirmaMessages.getString("SimpleAfirma.10") + " - " + file.getName()); //$NON-NLS-1$ //$NON-NLS-2$
        }

        this.currentFile = file;

        if (this.saf.isKeyStoreReady()) {
            setSignCommandEnabled(true);
        }
        else {
            this.signButton.setText(""); //$NON-NLS-1$
            this.signButton.setIcon(new ImageIcon(this.getClass().getResource("/resources/progress.gif"))); //$NON-NLS-1$
            this.signButton.setToolTipText(SimpleAfirmaMessages.getString("SignPanel.13")); //$NON-NLS-1$
        }

        this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        this.signButton.requestFocusInWindow();
    }

    private void createUI(final boolean firstTime) {

        if (!LookAndFeelManager.HIGH_CONTRAST) {
            this.setBackground(LookAndFeelManager.WINDOW_COLOR);
        }

        this.setLayout(new GridLayout(2, 1));
        this.add(new UpperPanel(firstTime));
        this.lowerPanel = new LowerPanel();
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
                        LOGGER.warning(
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
                                    SimpleAfirmaMessages.getString("SignPanel.18"), //$NON-NLS-1$
                                    SimpleAfirmaMessages.getString("SimpleAfirma.48"), //$NON-NLS-1$
                                    JOptionPane.WARNING_MESSAGE
                            );
                        }
                        File file = null;
                        final String filename = fileList.get(0).toString();
                        if (filename.startsWith("http://") || //$NON-NLS-1$
                        	filename.startsWith("https://") || //$NON-NLS-1$
                        	filename.startsWith("ftp://") //$NON-NLS-1$
                        ) {
                            UIUtils.showErrorMessage(
                                    SignPanel.this,
                                    SimpleAfirmaMessages.getString("SignPanel.24"), //$NON-NLS-1$
                                    SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                                    JOptionPane.ERROR_MESSAGE
                            );
                            dtde.dropComplete(false);
                            return;
                        }
                        else if (filename.startsWith("file://")) { //$NON-NLS-1$
                            try {
                            	file = new File(new URI(filename));
                            }
                            catch (final Exception e) {
                                LOGGER.warning(
                                "Ha fallado la operacion de arrastrar y soltar al obtener la ruta del fichero arrastrado: " + e //$NON-NLS-1$
                                );
                                dtde.dropComplete(false);
                                return;
                            }
                        }
                        try {
                            loadFile(file != null ? file : new File(filename));
                        }
                        catch (final IOException e) {
                            LOGGER.warning(
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
        createUI(firstTime);
    }

    private final class UpperPanel extends JPanel {

        private static final long serialVersionUID = 533243192995645135L;

        UpperPanel(final boolean firstTime) {
            super(true);
            createUI(firstTime);
        }

        private void createUI(final boolean firstTime) {
            this.setLayout(new BorderLayout(5, 5));
            this.setBorder(BorderFactory.createEmptyBorder(15, 15, 15, 15));

            SignPanel.this.getSelectButton().setText(SimpleAfirmaMessages.getString("SignPanel.32")); //$NON-NLS-1$
            SignPanel.this.getSelectButton().setMnemonic('S');
            SignPanel.this.getSelectButton().getAccessibleContext().setAccessibleDescription(
        		SimpleAfirmaMessages.getString("SignPanel.33") //$NON-NLS-1$
            );
            SignPanel.this.getSelectButton().getAccessibleContext().setAccessibleName(
        		SimpleAfirmaMessages.getString("SignPanel.34") //$NON-NLS-1$
            );
            SignPanel.this.getSelectButton().requestFocusInWindow();
            SignPanel.this.getSelectButton().addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent arg0) {
                	final File file;
                	try {
	                    file = AOUIFactory.getLoadFiles(
	                		SimpleAfirmaMessages.getString("SignPanel.35"), //$NON-NLS-1$
	                		SignPanel.this.getSimpleAfirma().getCurrentDir() != null ? SignPanel.this.getSimpleAfirma().getCurrentDir().getAbsolutePath() : null,
	                		null,
	                		null,
	                		null,
	                		false,
	                		false,
	                		UpperPanel.this
	            		)[0];
                	}
                	catch(final AOCancelledOperationException e) {
                		return;
                	}

                    try {
                        loadFile(file);
                    }
                    catch (final Exception e) {
                    	LOGGER.severe("Error en la carga de fichero " + file.getAbsolutePath() + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
                        UIUtils.showErrorMessage(
                                UpperPanel.this,
                                SimpleAfirmaMessages.getString("SignPanel.36"), //$NON-NLS-1$
                                SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                                JOptionPane.ERROR_MESSAGE
                        );
                        // La carga habra dejado el cursor en reloj de arena
                        SignPanel.this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                    }

                }
            });

            final JLabel welcomeLabel = new JLabel((firstTime ? SimpleAfirmaMessages.getString("SignPanel.14") + " " : "") + SimpleAfirmaMessages.getString("SignPanel.39")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
            welcomeLabel.setFocusable(false);
            welcomeLabel.setFont(welcomeLabel.getFont().deriveFont(Font.PLAIN, 26));
            welcomeLabel.setLabelFor(SignPanel.this.getSelectButton());
            this.add(welcomeLabel, BorderLayout.PAGE_START);

            String intro = SimpleAfirmaMessages.getString("SignPanel.40"); //$NON-NLS-1$

            try {
				final int nReaders = javax.smartcardio.TerminalFactory.getDefault().terminals().list().size();
                if (nReaders == 1) {
                    intro = intro + SimpleAfirmaMessages.getString("SignPanel.2"); //$NON-NLS-1$
                }
                else if (nReaders > 1) {
                    intro = intro + SimpleAfirmaMessages.getString("SignPanel.4"); //$NON-NLS-1$
                }
            }
            catch(final Exception e) { /* Ignoramos los errores */ }

            final JLabel introText = new JLabel(intro);
            introText.setLabelFor(SignPanel.this.getSelectButton());
            introText.setFocusable(false);

            final JPanel introPanel = new JPanel(new BorderLayout());
            introPanel.add(introText, BorderLayout.PAGE_START);
            this.add(introPanel, BorderLayout.CENTER);

            final JPanel selectPanel = new JPanel(new FlowLayout(FlowLayout.LEFT), true);
            selectPanel.add(SignPanel.this.getSelectButton());
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

        LowerPanel() {
            super(true);
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    createUI();
                }
            });
        }

        void createUI() {
            this.setLayout(new BorderLayout(5, 5));
            this.setBorder(BorderFactory.createEmptyBorder(15, 15, 15, 15));

            SignPanel.this.setFilePanel(new ResizingTextPanel(SimpleAfirmaMessages.getString("SignPanel.41"))); //$NON-NLS-1$
            SignPanel.this.getFilePanel().getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("SignPanel.42")); //$NON-NLS-1$
            SignPanel.this.getFilePanel().getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("SignPanel.43")); //$NON-NLS-1$
            SignPanel.this.getFilePanel().setToolTipText(SimpleAfirmaMessages.getString("SignPanel.42")); //$NON-NLS-1$
            SignPanel.this.getFilePanel().setFocusable(false);
            SignPanel.this.getFilePanel().setDropTarget(SignPanel.this.getDropTgt());

            this.add(SignPanel.this.getFilePanel(), BorderLayout.CENTER);

            final JPanel buttonPanel = new JPanel(true);
            SignPanel.this.getSignButton().setPreferredSize(new Dimension(160, 27));
            if (!SignPanel.this.isCosign()) {
                SignPanel.this.getSignButton().setText(SimpleAfirmaMessages.getString("SignPanel.45")); //$NON-NLS-1$
            }
            else {
                SignPanel.this.getSignButton().setText(SimpleAfirmaMessages.getString("SignPanel.16")); //$NON-NLS-1$
            }
            SignPanel.this.getSignButton().setMnemonic('F');
            SignPanel.this.getSignButton().setEnabled(false);
            buttonPanel.add(SignPanel.this.getSignButton());
            SignPanel.this.getSignButton().addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent ae) {
                    sign();
                }
            });

            // Establecemos la configuracion de color
            if (!LookAndFeelManager.HIGH_CONTRAST) {
                this.setBackground(LookAndFeelManager.WINDOW_COLOR);
                SignPanel.this.getFilePanel().setBackground(Color.DARK_GRAY);
                SignPanel.this.getFilePanel().setForeground(Color.LIGHT_GRAY);
                buttonPanel.setBackground(LookAndFeelManager.WINDOW_COLOR);
            }

            this.add(buttonPanel, BorderLayout.PAGE_END);
        }
    }

    private final class FilePanel extends JPanel {

        private static final long serialVersionUID = -8648491975442788750L;

        FilePanel(final Component icon,
                          final String fileSize,
                          final File file,
                          final String fileDescription,
                          final Date fileLastModified) {
            super(true);
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    createUI(icon, fileSize, file, fileDescription, fileLastModified);
                }
            });
        }

        void createUI(final Component icon,
                              final String fileSize,
                              final File file,
                              final String fileDescription,
                              final Date fileLastModified) {

            this.setBorder(BorderFactory.createLineBorder(Color.black));
            this.setLayout(new GridBagLayout());

            final JLabel pathLabel = new JLabel(file.getAbsolutePath());
            pathLabel.setFont(pathLabel.getFont().deriveFont(Font.BOLD, pathLabel.getFont().getSize() + 3f));

            final JLabel descLabel = new JLabel(SimpleAfirmaMessages.getString("SignPanel.46") + fileDescription); //$NON-NLS-1$
            final JLabel dateLabel = new JLabel(SimpleAfirmaMessages.getString("SignPanel.47") + //$NON-NLS-1$
                                                DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.SHORT).format(fileLastModified));

            final JLabel sizeLabel = new JLabel(SimpleAfirmaMessages.getString("SignPanel.49") + fileSize + " KB"); //$NON-NLS-1$ //$NON-NLS-2$

            final JPanel detailPanel = new JPanel();
            detailPanel.setLayout(new BoxLayout(detailPanel, BoxLayout.Y_AXIS));
            detailPanel.add(pathLabel);
            detailPanel.add(Box.createRigidArea(new Dimension(0, 8)));
            detailPanel.add(descLabel);
            detailPanel.add(dateLabel);
            detailPanel.add(Box.createRigidArea(new Dimension(0, 8)));
            detailPanel.add(sizeLabel);

            // Puede arrastrarse un fichero a cualquiera de estos componentes para cargarlo
            this.setDropTarget(SignPanel.this.getDropTgt());

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


            final JButton openFileButton = new JButton(SimpleAfirmaMessages.getString("SignPanel.51")); //$NON-NLS-1$
            openFileButton.setMnemonic('v');
            openFileButton.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent ae) {
                    if (file.getName().endsWith(".csig") || file.getName().endsWith(".xsig")) { //$NON-NLS-1$ //$NON-NLS-2$
                        new VisorFirma(false).initialize(false, file);
                    }
                    else {
                        try {
                            Desktop.getDesktop().open(file);
                        }
                        catch (final IOException e) {
                            UIUtils.showErrorMessage(
                                FilePanel.this,
                                SimpleAfirmaMessages.getString("SignPanel.53"), //$NON-NLS-1$
                                SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
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
                this.signButton.setText(SimpleAfirmaMessages.getString("SignPanel.45")); //$NON-NLS-1$
            }
            else {
                SignPanel.this.signButton.setText(SimpleAfirmaMessages.getString("SignPanel.16")); //$NON-NLS-1$
            }
            this.signButton.setToolTipText(null);
        }
        this.signButton.setEnabled(e);
        this.saf.setSignMenuCommandEnabled(e);
    }

    private final class SignTask extends SwingWorker<Void, Void> {

        SignTask() {
            super();
        }

        @Override
        public Void doInBackground() {

            SignPanel.this.setCursor(new Cursor(Cursor.WAIT_CURSOR));

            if (SignPanel.this.getSigner() == null || SignPanel.this.getDataToSign() == null || SignPanel.this.getSimpleAfirma() == null) {
                return null;
            }
            final AOKeyStoreManager ksm = SignPanel.this.getSimpleAfirma().getAOKeyStoreManager();

            PrivateKeyEntry pke;
                try {
                	final AOKeyStoreDialog dialog = new AOKeyStoreDialog(ksm, SignPanel.this, true, false, true);
                	dialog.show();
                	pke = ksm.getKeyEntry(dialog.getSelectedAlias(), ksm.getType().getCertificatePasswordCallback(SignPanel.this));
                }
                catch (final AOCancelledOperationException e) {
                    return null;
                }
                catch (final Exception e) {
                	LOGGER.severe("Ocurrio un error al extraer la clave privada del certificiado seleccionado: " + e); //$NON-NLS-1$
                	UIUtils.showErrorMessage(
                            SignPanel.this,
                            SimpleAfirmaMessages.getString("SignPanel.56"), //$NON-NLS-1$
                            SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                            JOptionPane.ERROR_MESSAGE
                        );
                	return null;
            	}
                finally {
                    SignPanel.this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                }

            setSignCommandEnabled(false);

            SignPanel.this.setCursor(new Cursor(Cursor.WAIT_CURSOR));

            SignPanel.this.getProgressMonitor().setProgress(999);

            // Deshabilitamos la posibilidad de cancelar el dialogo de espera
            try {
                final JDialog dialog = (JDialog) SignPanel.this.getProgressMonitor().getAccessibleContext().getAccessibleParent();
                dialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
                ((JOptionPane)dialog.getContentPane().getComponent(0)).setOptions(new Object[]{});
            }
            catch(final Exception e) { /* Ignoramos los errores */ }

            try {
                final Field jProgressBarField = ProgressMonitor.class.getDeclaredField("myBar"); //$NON-NLS-1$
                jProgressBarField.setAccessible(true);
                final JProgressBar progressBar = (JProgressBar) jProgressBarField.get(SignPanel.this.getProgressMonitor());

                final Method setIndeterminateMethod = JProgressBar.class.getMethod("setIndeterminate", Boolean.TYPE); //$NON-NLS-1$
                setIndeterminateMethod.invoke(progressBar, Boolean.TRUE);

                if (Platform.OS.MACOSX.equals(Platform.getOS())) {
                    final Method putCLientPropertyMethod = JProgressBar.class.getMethod("putClientProperty", Object.class, Object.class); //$NON-NLS-1$
                    putCLientPropertyMethod.invoke(progressBar, "JProgressBar.style", "circular"); //$NON-NLS-1$ //$NON-NLS-2$
                }

            }
            catch (final Exception e) {
                LOGGER.warning(
                        "No se ha podido mostrar la barra de progreso indeterminado: " + e); //$NON-NLS-1$
            }

            try { Thread.sleep(1000); } catch(final Exception e) { /* Ignoramos los errores */ }

            final Preferences preferences = Preferences.userRoot();
            final Properties p = ExtraParamsHelper.preferencesToExtraParams(preferences, SignPanel.this.getSigner());

            final String signatureAlgorithm = preferences.get(PreferencesNames.PREFERENCE_SIGNATURE_ALGORITHM, "SHA512withRSA"); //$NON-NLS-1$
            final byte[] signResult;

            try {
                if (SignPanel.this.isCosign()) {
                    signResult = SignPanel.this.getSigner().cosign(
                		SignPanel.this.getDataToSign(),
                		signatureAlgorithm,
                        pke.getPrivateKey(),
                        pke.getCertificateChain(),
                        p
                    );
                }
                else {
                    signResult = SignPanel.this.getSigner().sign(
                		SignPanel.this.getDataToSign(),
                		signatureAlgorithm,
                		pke.getPrivateKey(),
                        pke.getCertificateChain(),
                        p
                    );
                }
            }
            catch(final AOCancelledOperationException e) {
                setSignCommandEnabled(true);
                return null;
            }
            catch(final PdfIsCertifiedException e) {
            	LOGGER.warning("PDF no firmado por estar certificado: " + e); //$NON-NLS-1$
                UIUtils.showErrorMessage(
                        SignPanel.this,
                        SimpleAfirmaMessages.getString("SignPanel.27"), //$NON-NLS-1$
                        SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                        JOptionPane.ERROR_MESSAGE
                );
                setSignCommandEnabled(true);
                return null;
            }
            catch(final BadPdfPasswordException e) {
            	LOGGER.warning("PDF protegido con contrasena mal proporcionada: " + e); //$NON-NLS-1$
                UIUtils.showErrorMessage(
                        SignPanel.this,
                        SimpleAfirmaMessages.getString("SignPanel.23"), //$NON-NLS-1$
                        SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                        JOptionPane.ERROR_MESSAGE
                );
                setSignCommandEnabled(true);
                return null;
            }
            catch(final Exception e) {
                LOGGER.severe("Error durante el proceso de firma: " + e); //$NON-NLS-1$
                UIUtils.showErrorMessage(
                        SignPanel.this,
                        SimpleAfirmaMessages.getString("SignPanel.65"), //$NON-NLS-1$
                        SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                        JOptionPane.ERROR_MESSAGE
                );
                setSignCommandEnabled(true);
                return null;
            }
            catch(final OutOfMemoryError ooe) {
                LOGGER.severe("Falta de memoria en el proceso de firma: " + ooe); //$NON-NLS-1$
                UIUtils.showErrorMessage(
                    SignPanel.this,
                    SimpleAfirmaMessages.getString("SignPanel.1"), //$NON-NLS-1$
                    SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                    JOptionPane.ERROR_MESSAGE
                );
                setSignCommandEnabled(true);
                return null;
            }
            finally {
                SignPanel.this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
                SignPanel.this.getProgressMonitor().setProgress(1000);
            }

            String newFileName = SignPanel.this.getCurrentFile().getName();
            String[] filterExtensions;
            String filterDescription;
            if (SignPanel.this.getSigner() instanceof AOPDFSigner) {
                if (!newFileName.toLowerCase().endsWith(".pdf")) { //$NON-NLS-1$
                    newFileName = newFileName + ".pdf"; //$NON-NLS-1$
                }
                newFileName = newFileName.substring(0, newFileName.length() - 4).replace(".", "_") + "_signed.pdf"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                filterExtensions = new String[] {
                    ".pdf"}; //$NON-NLS-1$
                filterDescription = SimpleAfirmaMessages.getString("SignPanel.72"); //$NON-NLS-1$
            }
            else if (SignPanel.this.getSigner() instanceof AOXAdESSigner || SignPanel.this.getSigner() instanceof AOFacturaESigner) {
                newFileName = newFileName.replace(".", "_") + ".xsig"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                filterExtensions = new String[] {
                        ".xsig", ".xml"}; //$NON-NLS-1$ //$NON-NLS-2$
                filterDescription = SimpleAfirmaMessages.getString("SignPanel.76"); //$NON-NLS-1$
            }
            else {
                newFileName = newFileName.replace(".", "_") + ".csig"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                filterExtensions = new String[] {
                        ".csig", ".p7s"}; //$NON-NLS-1$ //$NON-NLS-2$
                filterDescription = SimpleAfirmaMessages.getString("SignPanel.80"); //$NON-NLS-1$
            }

            final String fDescription = filterDescription;
            final String[] fExtensions = filterExtensions;

            if (SignPanel.this.getSimpleAfirma().getCurrentDir() == null) {
                SignPanel.this.getSimpleAfirma().setCurrentDir(new File(Platform.getUserHome()));
            }

            final File fd;
            try {
            	fd = FileUIManager.saveFile(
            			SignPanel.this.getWindow(),
            			signResult,
            			SignPanel.this.getSimpleAfirma().getCurrentDir(),
            			newFileName,
            			fExtensions,
            			fDescription,
            			SimpleAfirmaMessages.getString("SignPanel.81")); //$NON-NLS-1$
            }
            catch(final IOException e) {
                LOGGER.severe(
                    "No se ha podido guardar el resultado de la firma: " + e //$NON-NLS-1$
                  );
                  UIUtils.showErrorMessage(
                          SignPanel.this,
                          SimpleAfirmaMessages.getString("SignPanel.88"), //$NON-NLS-1$
                          SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                          JOptionPane.ERROR_MESSAGE
                  );
                  setSignCommandEnabled(true);
                  return null;
            }
            catch(final AOCancelledOperationException e) {
            	setSignCommandEnabled(true);
            	return null;
            }
            finally {
            	SignPanel.this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            }

            SignPanel.this.getSimpleAfirma().setCurrentDir(fd);
            newFileName = fd.getAbsolutePath();

            SignPanel.this.getSimpleAfirma().loadResultsPanel(
            		signResult,
            		newFileName,
            		(X509Certificate) pke.getCertificate());

            return null;
        }

    }

}
