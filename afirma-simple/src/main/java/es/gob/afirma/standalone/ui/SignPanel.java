/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_BIN;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_FACTURAE;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_ODF;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_OOXML;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_PDF;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_XML;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.filters.CertificateFilter;
import es.gob.afirma.keystores.filters.MultipleCertificateFilter;
import es.gob.afirma.keystores.filters.PseudonymFilter;
import es.gob.afirma.keystores.filters.rfc.KeyUsageFilter;
import es.gob.afirma.signers.xades.AOFacturaESigner;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.DataAnalizerUtil;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.preferences.PreferencesManager;

/** Panel de selecci&oacute;n y firma del fichero objetivo.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class SignPanel extends JPanel {

    static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final long serialVersionUID = -4828575650695534417L;


    private static String[][] signersTypeRelation = new String[][] {
    	{"es.gob.afirma.signers.pades.AOPDFSigner", SimpleAfirmaMessages.getString("SignPanel.104")}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"es.gob.afirma.signers.xades.AOFacturaESigner", SimpleAfirmaMessages.getString("SignPanel.105")}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"es.gob.afirma.signers.xades.AOXAdESSigner", SimpleAfirmaMessages.getString("SignPanel.106")}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"es.gob.afirma.signers.cades.AOCAdESSigner", SimpleAfirmaMessages.getString("SignPanel.107")}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"es.gob.afirma.signers.odf.AOODFSigner", SimpleAfirmaMessages.getString("SignPanel.108")}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"es.gob.afirma.signers.ooxml.AOOOXMLSigner", SimpleAfirmaMessages.getString("SignPanel.109")} //$NON-NLS-1$ //$NON-NLS-2$
    };

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

    /** Indica si la operaci&oacute;n a realizar es una cofirma. */
    private boolean cosign = false;

    boolean isCosign() {
    	return this.cosign;
    }

    private File currentFile = null;

    File getCurrentFile() {
    	return this.currentFile != null ?
			AutoFirmaUtil.getCanonicalFile(this.currentFile) :
				null;
    }

    /** Carga el fichero a firmar.
     * @param file Fichero a firmar
     * @throws IOException Si ocurre alg&uacute;n problema durante la apertura o lectura del fichero */
    public void loadFile(final File file) throws IOException {

        setCursor(new Cursor(Cursor.WAIT_CURSOR));

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
        	AOUIFactory.showErrorMessage(
                SignPanel.this,
                errorMessage,
                SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
            setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            return;
        }

        final byte[] data;

        try (
    		final InputStream fis = new FileInputStream(file);
		) {
	        try {
	            data = AOUtil.getDataFromInputStream(fis);
	        }
	        catch(final OutOfMemoryError e) {
	        	LOGGER.warning(
        			"No hay memoria suficiente para leer el fichero '" + file.getAbsolutePath() + "': " + e //$NON-NLS-1$ //$NON-NLS-2$
    			);
	        	AOUIFactory.showErrorMessage(
	                 SignPanel.this,
	                 SimpleAfirmaMessages.getString("SignPanel.26"), //$NON-NLS-1$
	                 SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
	                 JOptionPane.ERROR_MESSAGE
	            );
	            setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	            return;
	        }
        }

        if (data == null || data.length < 1) {
            throw new IOException("No se ha podido leer el fichero"); //$NON-NLS-1$
        }

        this.dataToSign = data;
        this.cosign = false;

        SignPanelFileType signPanelFileType;

        // Comprobamos si es un fichero PDF
        if (DataAnalizerUtil.isPDF(data)) {
        	signPanelFileType = SignPanelFileType.PDF;
        	this.signer = AOSignerFactory.getSigner(
        		PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_PDF)
    		);
        }
        // Comprobamos si es una factura electronica
        else if (DataAnalizerUtil.isFacturae(data)) {
        	if (new AOFacturaESigner().isSign(data)) {
        		AOUIFactory.showErrorMessage(this,
    				SimpleAfirmaMessages.getString("SignPanel.22"), //$NON-NLS-1$
    				SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
    				JOptionPane.WARNING_MESSAGE
				);
        		setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        		return;
        	}
        	signPanelFileType = SignPanelFileType.FACTURAE;
        	this.signer = AOSignerFactory.getSigner(
            		PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_FACTURAE)
        		);
        }
        // Comprobamos si es un OOXML
        else if (DataAnalizerUtil.isOOXML(data)) {
        	signPanelFileType = SignPanelFileType.OOXML;
        	this.signer = AOSignerFactory.getSigner(
	    		PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_OOXML)
    		);
        }
        // Comprobamos si es un ODF
        else if (DataAnalizerUtil.isODF(data)) {
        	signPanelFileType = SignPanelFileType.ODF;
        	this.signer = AOSignerFactory.getSigner(
	    		PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_ODF)
    		);
        }
        // Comprobamos si es un fichero de firma CAdES o XAdES (los PDF, facturas, OOXML y ODF pasaran por las condiciones anteriores)
        else if ((this.signer = AOSignerFactory.getSigner(data)) != null) {
        	if (this.signer instanceof AOXAdESSigner) {
        		signPanelFileType = SignPanelFileType.SIGN_XADES;
        	}
        	else {
        		signPanelFileType = SignPanelFileType.SIGN_CADES;
        	}
            this.cosign = true;
        }
        // Comprobamos si es un fichero XML
        else if (DataAnalizerUtil.isXML(data)) {
        	signPanelFileType = SignPanelFileType.XML;
        	this.signer = AOSignerFactory.getSigner(
        		PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_XML)
    		);
        }
        // Cualquier otro tipo de fichero
        else {
            signPanelFileType = SignPanelFileType.BINARY;
            this.signer = AOSignerFactory.getSigner(
	    		PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_BIN)
    		);
        }

        final String signatureName = getSignatureName(this.signer);

        final double fileSize = file.length() / 1024;
        final long fileLastModified = file.lastModified();

        this.lowerPanel.remove(this.filePanel);

        this.filePanel = new SignPanelFilePanel(
    		signPanelFileType,
    		this.signer,
    		signatureName,
            NumberFormat.getInstance().format(fileSize),
            file,
            new Date(fileLastModified),
            getDropTgt()
        );

        this.lowerPanel.add(this.filePanel);
        this.lowerPanel.revalidate();

        if (this.window != null) {
            this.window.getRootPane().putClientProperty("Window.documentFile", file); //$NON-NLS-1$
            this.window.setTitle(SimpleAfirmaMessages.getString("SimpleAfirma.10", SimpleAfirma.getVersion()) + " - " + file.getName()); //$NON-NLS-1$ //$NON-NLS-2$
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

        setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        this.signButton.requestFocusInWindow();
    }

    /** Identifica el tipo de firma asociado a un firmador. Si no identifica el firmador,
     * devuelte el tipo por defecto.
     * @param signer Firmador.
     * @return Tipo de firma que se debe ejecutar. */
    private static String getSignatureName(final AOSigner signer) {
    	for (final String[] signatureType : signersTypeRelation) {
    		final Class<?> c;
    		try {
    			c = Class.forName(signatureType[0]);
    		}
    		catch (final Exception e) {
    			continue;
    		}
    		if (c.isInstance(signer)) {
    			return signatureType[1];
    		}
    	}
		return SimpleAfirmaMessages.getString("SignPanel.110"); //$NON-NLS-1$
	}

	private void createUI() {

        if (!LookAndFeelManager.HIGH_CONTRAST) {
            setBackground(LookAndFeelManager.WINDOW_COLOR);
        }

        setLayout(new GridLayout(2, 1));
        this.add(new UpperPanel());
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
                        	AOUIFactory.showErrorMessage(
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
                        	AOUIFactory.showErrorMessage(
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

        if (getFilePanel() != null && getDropTgt() != null) {
        	getFilePanel().setDropTarget(getDropTgt());
        }
    }

    /** Construye el panel de firma, en el que se selecciona y se firma un fichero.
     * @param win Ventana de primer nivel, para el cambio de t&iacute;tulo en la carga de fichero
     * @param sa Clase principal, para proporcionar el <code>AOKeyStoreManager</code> necesario para
     *        realizar las firmas y cambiar de panel al finalizar una firma. */
    public SignPanel(final JFrame win, final SimpleAfirma sa) {
        super(true);
        this.window = win;
        this.saf = sa;
        createUI();
    }

    private final class UpperPanel extends JPanel {

        private static final long serialVersionUID = 533243192995645135L;

        UpperPanel() {
            super(true);
            createUI();
        }

        private void createUI() {
            setLayout(new BorderLayout(5, 5));
            setBorder(BorderFactory.createEmptyBorder(15, 15, 15, 15));

            getSelectButton().setText(SimpleAfirmaMessages.getString("SignPanel.32")); //$NON-NLS-1$
            getSelectButton().setMnemonic('S');
            getSelectButton().getAccessibleContext().setAccessibleDescription(
        		SimpleAfirmaMessages.getString("SignPanel.33") //$NON-NLS-1$
            );
            getSelectButton().getAccessibleContext().setAccessibleName(
        		SimpleAfirmaMessages.getString("SignPanel.34") //$NON-NLS-1$
            );
            getSelectButton().requestFocusInWindow();
            getSelectButton().addActionListener(arg0 -> {
				final File file;
				try {
			        file = AOUIFactory.getLoadFiles(
			    		SimpleAfirmaMessages.getString("SignPanel.35"), //$NON-NLS-1$
			    		null,
			    		null,
			    		null,
			    		null,
			    		false,
			    		false,
			    		AutoFirmaUtil.getDefaultDialogsIcon(),
			    		UpperPanel.this
					)[0];
				}
				catch(final AOCancelledOperationException e1) {
					return;
				}

			    try {
			        loadFile(file);
			    }
			    catch (final Exception e2) {
			    	LOGGER.severe("Error en la carga de fichero " + file.getAbsolutePath() + ": " + e2); //$NON-NLS-1$ //$NON-NLS-2$
			    	AOUIFactory.showErrorMessage(
			            UpperPanel.this,
			            SimpleAfirmaMessages.getString("SignPanel.36"), //$NON-NLS-1$
			            SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
			            JOptionPane.ERROR_MESSAGE
			        );
			        // La carga habra dejado el cursor en reloj de arena
			        SignPanel.this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			    }

			});

            final JLabel welcomeLabel = new JLabel(SimpleAfirmaMessages.getString("SignPanel.14")); //$NON-NLS-1$
            welcomeLabel.setFocusable(false);
            welcomeLabel.setFont(welcomeLabel.getFont().deriveFont(Font.PLAIN, 26));
            welcomeLabel.setLabelFor(getSelectButton());
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
            catch(final Exception | Error e) {
            	LOGGER.warning(
        			"No ha sido posible obtener la lista de lectores de tarjetas del sistema: " + e //$NON-NLS-1$
    			);
            }

            final JLabel introText = new JLabel(intro);
            introText.setLabelFor(getSelectButton());
            introText.setFocusable(false);

            final JPanel introPanel = new JPanel(new BorderLayout());
            introPanel.add(introText, BorderLayout.PAGE_START);
            this.add(introPanel, BorderLayout.CENTER);

            final JPanel selectPanel = new JPanel(new FlowLayout(FlowLayout.LEFT), true);
            selectPanel.add(getSelectButton());
            this.add(selectPanel, BorderLayout.PAGE_END);

            // Configuramos el color
            if (!LookAndFeelManager.HIGH_CONTRAST) {
                setBackground(LookAndFeelManager.WINDOW_COLOR);
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
            SwingUtilities.invokeLater(() -> createUI());
        }

        void createUI() {
            setLayout(new BorderLayout(5, 5));
            setBorder(BorderFactory.createEmptyBorder(15, 15, 15, 15));

            setFilePanel(new ResizingTextPanel(SimpleAfirmaMessages.getString("SignPanel.41"))); //$NON-NLS-1$
            getFilePanel().getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("SignPanel.42")); //$NON-NLS-1$
            getFilePanel().getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("SignPanel.43")); //$NON-NLS-1$
            getFilePanel().setToolTipText(SimpleAfirmaMessages.getString("SignPanel.42")); //$NON-NLS-1$
            getFilePanel().setFocusable(false);
            getFilePanel().setDropTarget(getDropTgt());

            this.add(getFilePanel(), BorderLayout.CENTER);

            final JPanel buttonPanel = new JPanel(true);
            getSignButton().setPreferredSize(new Dimension(160, 27));
            if (!isCosign()) {
                getSignButton().setText(SimpleAfirmaMessages.getString("SignPanel.45")); //$NON-NLS-1$
            }
            else {
                getSignButton().setText(SimpleAfirmaMessages.getString("SignPanel.16")); //$NON-NLS-1$
            }
            getSignButton().setMnemonic('F');
            getSignButton().setEnabled(false);
            buttonPanel.add(getSignButton());
            getSignButton().addActionListener(
        		ae -> sign()
    		);

            // Establecemos la configuracion de color
            if (!LookAndFeelManager.HIGH_CONTRAST) {
                setBackground(LookAndFeelManager.WINDOW_COLOR);
                getFilePanel().setBackground(Color.DARK_GRAY);
                getFilePanel().setForeground(Color.LIGHT_GRAY);
                buttonPanel.setBackground(LookAndFeelManager.WINDOW_COLOR);
            }

            this.add(buttonPanel, BorderLayout.PAGE_END);
        }
    }

    /** Firma el fichero actualmente cargado. */
    public void sign() {
    	final CommonWaitDialog signWaitDialog = new CommonWaitDialog(
			getSimpleAfirma().getMainFrame(),
			SimpleAfirmaMessages.getString("SignPanel.48"), //$NON-NLS-1$
			SimpleAfirmaMessages.getString("SignPanel.50") //$NON-NLS-1$
		);
    	new SignPanelSignTask(
    		this,
    		getCertFilters(),
    		signWaitDialog
		).execute();
    	signWaitDialog.setVisible(true);
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

    static List<? extends CertificateFilter> getCertFilters() {
    	final List<CertificateFilter> filters = new ArrayList<>();
    	if (PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_KEYSTORE_SIGN_ONLY_CERTS)) {
    		filters.add(new KeyUsageFilter(KeyUsageFilter.SIGN_CERT_USAGE));
    	}
    	if (PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_KEYSTORE_ALIAS_ONLY_CERTS)) {
    		filters.add(new PseudonymFilter());
    	}
    	if (filters.size() > 1) {
    		return Arrays.asList(
				new MultipleCertificateFilter(filters.toArray(new CertificateFilter[0]))
			);
    	}
    	else if (filters.size() == 1) {
    		return filters;
    	}
    	return null;
    }

}
