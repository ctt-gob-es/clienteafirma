/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.HyperlinkEvent;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeSelectionModel;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.DataAnalizerUtil;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.crypto.CertAnalyzer;
import es.gob.afirma.standalone.crypto.CertificateInfo;
import es.gob.afirma.standalone.crypto.CompleteSignInfo;
import es.gob.afirma.standalone.crypto.TimestampsAnalyzer;

final class SignDataPanel extends JPanel {

    private static final long serialVersionUID = 4956746943438652928L;

    private static final String FILE_ICON_PDF = "/resources/icon_pdf.png";  //$NON-NLS-1$
    private static final String FILE_ICON_SIGN = "/resources/icon_sign.png"; //$NON-NLS-1$
    private static final String FILE_ICON_FACTURAE = "/resources/icon_facturae.png"; //$NON-NLS-1$
    private static final String FILE_ICON_OOXML_WIN = "/resources/icon_office_win.png"; //$NON-NLS-1$
    private static final String FILE_ICON_ODF = "/resources/icon_openoffice.png"; //$NON-NLS-1$

    private final JLabel certDescText = new JLabel();
    private final JLabel filePathText = new JLabel();
    private final JLabel certIcon = new JLabel();
    private final JEditorPane certDescription = new JEditorPane();
    private final JButton validateCertButton = null;

    static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    SignDataPanel(final File signFile, final byte[] sign, final JComponent fileTypeIcon, final X509Certificate cert, final KeyListener extKeyListener) {
        SwingUtilities.invokeLater(() -> createUI(signFile, sign, fileTypeIcon, cert, extKeyListener));
    }

    void createUI(final File signFile, final byte[] sign, final JComponent fileTypeIcon, final X509Certificate cert, final KeyListener extKeyListener) {

        // Texto con la ruta del fichero
        final JTextField filePath = new JTextField();
        filePath.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("SignDataPanel.0")); //$NON-NLS-1$
        filePath.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("SignDataPanel.1")); //$NON-NLS-1$
        filePath.setBorder(BorderFactory.createEmptyBorder());

        filePath.setEditable(false);
        filePath.setFocusable(false);
        filePath.setText(signFile == null ? SimpleAfirmaMessages.getString("SignDataPanel.24") : signFile.getAbsolutePath());  //$NON-NLS-1$
        filePath.addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(final MouseEvent me) {
                // me.isPopupTrigger() depende del Look & Feel y no se puede usar
                if (me.getButton() == MouseEvent.BUTTON3 && me.getClickCount() == 1) {
                    new CopyMenuItem(filePath, SimpleAfirmaMessages.getString("SignDataPanel.30")).show(me.getComponent(), me.getX(), me.getY()); //$NON-NLS-1$
                }
            }
        });

        // Etiqueta encima del cuadro con la ruta de fichero
        this.filePathText.setText(SimpleAfirmaMessages.getString("SignDataPanel.2")); //$NON-NLS-1$
        this.filePathText.setLabelFor(filePath);

        final JPanel filePathPanel = new JPanel();
        filePathPanel.setBorder(BorderFactory.createLineBorder(Color.GRAY));
        filePathPanel.setLayout(new BoxLayout(filePathPanel, BoxLayout.X_AXIS));
        filePathPanel.add(Box.createRigidArea(new Dimension(0, 40)));
        filePathPanel.add(Box.createRigidArea(new Dimension(5, 0)));

        boolean isOpennable = false;

        if (fileTypeIcon != null) {
            filePathPanel.add(fileTypeIcon);
        }
        else {
            final String fileIcon;
            final String fileTooltip;
            if (DataAnalizerUtil.isPDF(sign)) {
            	isOpennable = true;
                fileIcon = FILE_ICON_PDF;
                fileTooltip = SimpleAfirmaMessages.getString("SignDataPanel.9"); //$NON-NLS-1$
            }
            else if (DataAnalizerUtil.isFacturae(sign)) {
                fileIcon = FILE_ICON_FACTURAE;
                fileTooltip = SimpleAfirmaMessages.getString("SignDataPanel.10"); //$NON-NLS-1$
            }
            else if (DataAnalizerUtil.isOOXML(sign)) {
            	isOpennable = true;
                fileIcon = FILE_ICON_OOXML_WIN;
                fileTooltip = SimpleAfirmaMessages.getString("SignDataPanel.40"); //$NON-NLS-1$
            }
            else if (DataAnalizerUtil.isODF(sign)) {
            	isOpennable = true;
                fileIcon = FILE_ICON_ODF;
                fileTooltip = SimpleAfirmaMessages.getString("SignPanel.41"); //$NON-NLS-1$
            }
            else {
                fileIcon = FILE_ICON_SIGN;
                fileTooltip = SimpleAfirmaMessages.getString("SignDataPanel.31"); //$NON-NLS-1$
            }
            final JLabel iconLabel = new JLabel(new ImageIcon(SignDataPanel.class.getResource(fileIcon)));
            iconLabel.setToolTipText(fileTooltip);
            iconLabel.setFocusable(false);
            filePathPanel.add(iconLabel);
        }

        // Boton de apertura del fichero firmado
        JButton openFileButton = null;
        if (isOpennable && signFile != null) {
            openFileButton = new JButton(SimpleAfirmaMessages.getString("SignDataPanel.3")); //$NON-NLS-1$
            openFileButton.setPreferredSize(new Dimension(150, 24));
            openFileButton.setMnemonic('v');
            openFileButton.setToolTipText(SimpleAfirmaMessages.getString("SignDataPanel.4")); //$NON-NLS-1$
            openFileButton.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("SignDataPanel.5")); //$NON-NLS-1$
            openFileButton.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("SignDataPanel.6")); //$NON-NLS-1$
            openFileButton.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent ae) {
                    try {
                        Desktop.getDesktop().open(signFile);
                    }
                    catch (final Exception e) {
                    	LOGGER.warning(
                			"Error abriendo el fichero con el visor por defecto: " + e //$NON-NLS-1$
            			);
                    	AOUIFactory.showErrorMessage(
                            SignDataPanel.this,
                            SimpleAfirmaMessages.getString("SignDataPanel.7"), //$NON-NLS-1$
                            SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                            JOptionPane.ERROR_MESSAGE
                        );
                    }
                }
            });
        }


        filePathPanel.add(Box.createRigidArea(new Dimension(11, 0)));
        filePathPanel.add(filePath);
        filePathPanel.add(Box.createRigidArea(new Dimension(11, 0)));
        if (openFileButton != null) {
            filePathPanel.add(openFileButton);
            filePathPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        }

        JPanel certDescPanel = null;

        // Panel con los datos del certificado
        if (cert != null) {
            final CertificateInfo certInfo = CertAnalyzer.getCertInformation(cert);

            if (certInfo != null) {

	            this.certIcon.setIcon(certInfo.getIcon());
	            this.certIcon.setToolTipText(certInfo.getIconTooltip());
	            this.certIcon.setFocusable(false);

	            // Para que se detecten apropiadamente los hipervinculos hay que establecer
	            // el tipo de contenido antes que el contenido
	            this.certDescription.setContentType("text/html"); //$NON-NLS-1$
	            setFocusable(false);
	            this.certDescription.setEditable(false);
	            this.certDescription.setOpaque(false);
	            this.certDescription.setText(certInfo.getDescriptionText());
	            this.certDescription.setToolTipText(SimpleAfirmaMessages.getString("SignDataPanel.12")); //$NON-NLS-1$
	            this.certDescription.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("SignDataPanel.13")); //$NON-NLS-1$
	            this.certDescription.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("SignDataPanel.14")); //$NON-NLS-1$

	            final EditorFocusManager editorFocusManager = new EditorFocusManager (this.certDescription, new EditorFocusManagerAction() {
                    @Override
                    public void openHyperLink(final HyperlinkEvent he, final int linkIndex) {
                        openCertificate(cert, SignDataPanel.this);
                    }
                });
                this.certDescription.addFocusListener(editorFocusManager);
                this.certDescription.addKeyListener(editorFocusManager);
	            this.certDescription.addHyperlinkListener(editorFocusManager);

//	            CertificateVerificable cfv = null;
//	            try {
//	            	cfv = CertificateVerifierFactory.getCertificateVerifier(cert);
//	            }
//	            catch(final Exception e) {
//	            	LOGGER.warning("No se ha podido cargar el verificador de certificados: " + e); //$NON-NLS-1$
//	            }
//
//	            if (cfv != null) {
//	            	final CertificateVerificable cf = cfv;
//	            	this.validateCertButton = new JButton();
//	                this.validateCertButton.setPreferredSize(new Dimension(150, 24));
//	                this.validateCertButton.setText(SimpleAfirmaMessages.getString("SignDataPanel.15")); //$NON-NLS-1$
//	                this.validateCertButton.setMnemonic('c');
//	                this.validateCertButton.setToolTipText(SimpleAfirmaMessages.getString("SignDataPanel.16")); //$NON-NLS-1$
//	                this.validateCertButton.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("SignDataPanel.17")); //$NON-NLS-1$
//	                this.validateCertButton.getAccessibleContext()
//	                .setAccessibleDescription(SimpleAfirmaMessages.getString("SignDataPanel.18")); //$NON-NLS-1$
//	                this.validateCertButton.addActionListener(new ActionListener() {
//						@Override
//						public void actionPerformed(final ActionEvent ae) {
//						    SignDataPanel.this.setCursor(new Cursor(Cursor.WAIT_CURSOR));
//
//							final ValidationResult vr = cf.validateCertificate(cert);
//							String validationMessage;
//							int validationMessageType = JOptionPane.ERROR_MESSAGE;
//							switch(vr) {
//								case VALID:
//									validationMessage = SimpleAfirmaMessages.getString("SignDataPanel.19"); //$NON-NLS-1$
//									validationMessageType = JOptionPane.INFORMATION_MESSAGE;
//									break;
//								case EXPIRED:
//									validationMessage = SimpleAfirmaMessages.getString("SignDataPanel.32"); //$NON-NLS-1$
//									break;
//								case REVOKED:
//									validationMessage = SimpleAfirmaMessages.getString("SignDataPanel.33"); //$NON-NLS-1$
//									break;
//								case NOT_YET_VALID:
//									validationMessage = SimpleAfirmaMessages.getString("SignDataPanel.34"); //$NON-NLS-1$
//									break;
//								case CA_NOT_SUPPORTED:
//									validationMessage = SimpleAfirmaMessages.getString("SignDataPanel.35"); //$NON-NLS-1$
//									break;
//								case CORRUPT:
//									validationMessage = SimpleAfirmaMessages.getString("SignDataPanel.36"); //$NON-NLS-1$
//									break;
//								case SERVER_ERROR:
//									validationMessage = SimpleAfirmaMessages.getString("SignDataPanel.37"); //$NON-NLS-1$
//									break;
//								case UNKNOWN:
//									validationMessage = SimpleAfirmaMessages.getString("SignDataPanel.38"); //$NON-NLS-1$
//									break;
//								default:
//									validationMessage = SimpleAfirmaMessages.getString("SignDataPanel.11");  //$NON-NLS-1$
//							}
//							AOUIFactory.showMessageDialog(
//								SignDataPanel.this,
//								validationMessage,
//								SimpleAfirmaMessages.getString("SignDataPanel.39"),  //$NON-NLS-1$
//								validationMessageType
//							);
//
//						    SignDataPanel.this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
//
//						}
//					});
//	            }

            }
            certDescPanel = new JPanel();
            certDescPanel.setBorder(BorderFactory.createLineBorder(Color.GRAY));
            certDescPanel.setLayout(new BoxLayout(certDescPanel, BoxLayout.X_AXIS));
            certDescPanel.add(Box.createRigidArea(new Dimension(0, 40)));
            certDescPanel.add(Box.createRigidArea(new Dimension(5, 0)));
            certDescPanel.add(this.certIcon);
            certDescPanel.add(Box.createRigidArea(new Dimension(11, 0)));
            certDescPanel.add(this.certDescription);
            certDescPanel.add(Box.createRigidArea(new Dimension(11, 0)));
            if (this.validateCertButton != null) {
                certDescPanel.add(this.validateCertButton);
                certDescPanel.add(Box.createRigidArea(new Dimension(5, 0)));
            }
            if (!LookAndFeelManager.HIGH_CONTRAST) {
                certDescPanel.setBackground(LookAndFeelManager.WINDOW_COLOR);
            }

            this.certDescText.setText(SimpleAfirmaMessages.getString("SignDataPanel.21")); //$NON-NLS-1$
            this.certDescText.setLabelFor(this.certDescription);
        }

        // Panel el detalle de la firma
        CompleteSignInfo signInfo;
        try {
            signInfo = SignDataPanel.getSignInfo(sign);
        }
        catch (final Exception e) {
        	LOGGER.severe("Error obteniendo los datos de la firma: " + e); //$NON-NLS-1$
            signInfo = null;
        }
        final JScrollPane detailPanel = new JScrollPane(
    		signInfo == null ? null : SignDataPanel.getSignDataTree(signInfo, extKeyListener, SignDataPanel.this)
		);

        // En Apple siempre hay barras, y es el SO el que las pinta o no depende de si hacen falta
        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            detailPanel.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
            detailPanel.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
        }

        final JLabel detailPanelText = new JLabel(SimpleAfirmaMessages.getString("SignDataPanel.22")); //$NON-NLS-1$
        detailPanelText.setLabelFor(detailPanel);

        // Establecemos la configuracion de color
        if (!LookAndFeelManager.HIGH_CONTRAST) {
            setBackground(LookAndFeelManager.WINDOW_COLOR);
            filePath.setBackground(LookAndFeelManager.WINDOW_COLOR);
            filePathPanel.setBackground(LookAndFeelManager.WINDOW_COLOR);
        }

        setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.weightx = 1.0;
        c.gridy = 0;
        c.insets = new Insets(11, 0, 0, 0);
        this.add(this.filePathText, c);
        c.gridy = 1;
        c.insets = new Insets(0, 0, 0, 0);
        this.add(filePathPanel, c);
        c.gridy = 2;
        c.insets = new Insets(11, 0, 0, 0);
        if (certDescPanel != null) {
            this.add(this.certDescText, c);
            c.gridy = 3;
            c.insets = new Insets(0, 0, 0, 0);
            this.add(certDescPanel, c);
            c.gridy = 4;
            c.insets = new Insets(11, 0, 0, 0);
        }
        this.add(detailPanelText, c);
        c.fill = GridBagConstraints.BOTH;
        c.weighty = 1.0;
        c.gridy = 5;
        c.insets = new Insets(0, 0, 0, 0);
        this.add(detailPanel, c);
    }

	static void openCertificate(final X509Certificate cert, final Component parent) {
        try {
            final File tmp = File.createTempFile("afirma", ".cer");  //$NON-NLS-1$//$NON-NLS-2$
            tmp.deleteOnExit();
            try (
        		final OutputStream bos = new BufferedOutputStream(new FileOutputStream(tmp));
    		) {
            	bos.write(cert.getEncoded());
            }
            Desktop.getDesktop().open(tmp);
        }
        catch(final Exception e) {
        	LOGGER.warning(
    			"Error abriendo el fichero con el visor por defecto: " + e //$NON-NLS-1$
			);
        	AOUIFactory.showErrorMessage(
                parent,
                SimpleAfirmaMessages.getString("SignDataPanel.23"), //$NON-NLS-1$
                SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE
            );
        }
    }

    /** Recupera la informaci&oacute;n de la firma indicada.
     * @param signData Firma.
     * @return Informaci&oacute;n de la firma.
     * @throws IOException Si ocurren problemas relacionados con la lectura de los datos */
    private static CompleteSignInfo getSignInfo(final byte[] signData) throws IOException {
        final CompleteSignInfo signInfo = new CompleteSignInfo();
        signInfo.setSignData(signData);
        final AOSigner signer = AOSignerFactory.getSigner(signData);
        if (signer == null) {
        	LOGGER.warning("Formato de firma no reconocido"); //$NON-NLS-1$
            throw new IllegalArgumentException("Formato de firma no reconocido"); //$NON-NLS-1$
        }
        try {
            signInfo.setSignInfo(signer.getSignInfo(signData));
        }
        catch (final Exception e) {
        	LOGGER.warning("Error al leer la informacion de la firma: " + e); //$NON-NLS-1$
        }
        try {
        	signInfo.setSignsTree(signer.getSignersStructure(signData, true));
        }
        catch (final Exception e) {
        	LOGGER.warning("Error al extraer el arbol de firmantes: " + e);  //$NON-NLS-1$
        	signInfo.setSignsTree(null);
        }
        try {
            signInfo.setData(signer.getData(signData));
        }
        catch (final Exception e) {
        	LOGGER.warning("Error al extraer los datos firmados: " + e);  //$NON-NLS-1$
        }
        try {
        	signInfo.setTimestampsInfo(
    			TimestampsAnalyzer.getTimestamps(signData)
			);
        }
        catch (final Exception e) {
        	LOGGER.warning("Error al extraer los sellos de tiempo: " + e);  //$NON-NLS-1$
        }
        return signInfo;
    }

    private static JTree getSignDataTree(final CompleteSignInfo signInfo,
    		                             final KeyListener extKeyListener,
    		                             final Component parent) {
        final DefaultMutableTreeNode root = new DefaultMutableTreeNode();

        // Formato de firma
        final DefaultMutableTreeNode signInfoBranch = new DefaultMutableTreeNode(
    		SimpleAfirmaMessages.getString("SignDataPanel.25") //$NON-NLS-1$
		);
        signInfoBranch.add(new DefaultMutableTreeNode(signInfo.getSignInfo().getFormat()));
        root.add(signInfoBranch);

        // Datos firmados
        final DefaultMutableTreeNode dataInfoBranch = new DefaultMutableTreeNode(
    		SimpleAfirmaMessages.getString("SignDataPanel.26") //$NON-NLS-1$
		);
        if (signInfo.getData() == null) {
            dataInfoBranch.add(
        		new DefaultMutableTreeNode(
    				SimpleAfirmaMessages.getString("SignDataPanel.27") //$NON-NLS-1$
				)
    		);
        }
        else {
            dataInfoBranch.add(
        		new DefaultMutableTreeNode(
    				new ShowFileLinkAction(
						SimpleAfirmaMessages.getString("SignDataPanel.28"),  //$NON-NLS-1$
						signInfo.getData(),
						parent
					)
				)
    		);
        }
        root.add(dataInfoBranch);

        // Arbol de firmantes
        final TreeModelManager treeManager = new TreeModelManager(signInfo.getSignsTree());
        final DefaultMutableTreeNode signersBranch = treeManager.getSwingTree();
        signersBranch.setUserObject(SimpleAfirmaMessages.getString("SignDataPanel.29")); //$NON-NLS-1$
        root.add(signersBranch);

        // Sellos de tiempo de la firma
        if (signInfo.getTimestampsInfo() != null && !signInfo.getTimestampsInfo().isEmpty()) {
        	final TreeModelManager treeManagerTimestamps = new TreeModelManager(
    			signInfo.getTimestampsTree()
			);
        	final DefaultMutableTreeNode timestampsBranch = treeManagerTimestamps.getSwingTree();
        	timestampsBranch.setUserObject(
    			SimpleAfirmaMessages.getString("SignDataPanel.42") //$NON-NLS-1$
			);
	        root.add(timestampsBranch);
        }

        //final DefaultTreeCellRenderer treeRenderer = new DefaultTreeCellRenderer();
        final LinksTreeCellRenderer treeRenderer = new LinksTreeCellRenderer();
        treeRenderer.setLeafIcon(null);
        treeRenderer.setClosedIcon(Platform.OS.WINDOWS.equals(Platform.getOS()) || Platform.OS.MACOSX.equals(Platform.getOS()) ? null : UIManager.getDefaults().getIcon("TREE.collapsedIcon")); //$NON-NLS-1$
        treeRenderer.setOpenIcon(Platform.OS.WINDOWS.equals(Platform.getOS()) || Platform.OS.MACOSX.equals(Platform.getOS()) ? null : UIManager.getDefaults().getIcon("TREE.expandedIcon")); //$NON-NLS-1$

        final JTree tree = new JTree(root);
        tree.setShowsRootHandles(true);
        tree.setCellRenderer(treeRenderer);
        tree.setRootVisible(false);
        tree.putClientProperty("JTree.lineStyle", "None"); //$NON-NLS-1$ //$NON-NLS-2$
        tree.getSelectionModel().setSelectionMode(
                TreeSelectionModel.SINGLE_TREE_SELECTION);

        final TreeFocusManager treeFocusManager = new TreeFocusManager(tree, new TreeFocusManagerAction() {
            @Override
            public void openTreeNode(final Object nodeInfo) {
                if (nodeInfo instanceof AOSimpleSignInfo) {
                    openCertificate(((AOSimpleSignInfo) nodeInfo).getCerts()[0], parent);
                }
                else if (nodeInfo instanceof ShowFileLinkAction) {
                    ((ShowFileLinkAction) nodeInfo).action();
                }
            }
        });

        tree.addMouseMotionListener(treeFocusManager);
        tree.addFocusListener(treeFocusManager);
        tree.addMouseListener(treeFocusManager);
        tree.addKeyListener(treeFocusManager);
        if (extKeyListener != null) {
        	tree.addKeyListener(extKeyListener);
        }

        for (int i = 0; i < tree.getRowCount(); i++) {
            tree.expandRow(i);
        }

        return tree;
    }

}
