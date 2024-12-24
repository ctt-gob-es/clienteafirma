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
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.security.cert.X509Certificate;
import java.util.Enumeration;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreeSelectionModel;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.standalone.DataAnalizerUtil;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.crypto.CertAnalyzer;
import es.gob.afirma.standalone.crypto.CertificateInfo;
import es.gob.afirma.standalone.crypto.CompleteSignInfo;
import es.gob.afirma.standalone.crypto.TimestampsAnalyzer;

final class SignDataPanel extends JPanel {

    private static final long serialVersionUID = 4956746943438652928L;

    static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final String FILE_ICON_PDF = "icon_pdf.png";  //$NON-NLS-1$
    private static final String FILE_ICON_SIGN = "icon_sign.png"; //$NON-NLS-1$
    private static final String FILE_ICON_FACTURAE = "icon_facturae.png"; //$NON-NLS-1$
    private static final String FILE_ICON_OOXML_WIN = "icon_office_win.png"; //$NON-NLS-1$
    private static final String FILE_ICON_ODF = "icon_openoffice.png"; //$NON-NLS-1$

    private final JLabel certDescText = new JLabel();
    private final JLabel filePathText = new JLabel();
    private final JLabel certIcon = new JLabel();
    private final JLabel holderDescCertLabel = new JLabel();
    private final JLabel holderCertLabel = new JLabel();
    private final JLabel issuerDescCertLabel = new JLabel();
    private final JLabel issuerCertLabel = new JLabel();
    private final JButton validateCertButton = null;

    private CompleteSignInfo currentSignInfo = null;

    SignDataPanel(final File signFile, final byte[] sign, final JComponent fileTypeIcon, final X509Certificate cert, final KeyListener extKeyListener, final Properties params) {
        SwingUtilities.invokeLater(() -> createUI(signFile, sign, fileTypeIcon, cert, extKeyListener, params));
    }

    void createUI(final File signFile, final byte[] sign, final JComponent fileTypeIcon, final X509Certificate cert, final KeyListener extKeyListener, final Properties params) {

    	// Color de fondo
    	if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST && Platform.getOS() == Platform.OS.WINDOWS) {
            setBackground(LookAndFeelManager.DEFAULT_COLOR);
    	}

        // Texto con la ruta del fichero
        final JLabel filePath = new JLabel();
        filePath.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("SignDataPanel.0")); //$NON-NLS-1$
        filePath.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("SignDataPanel.1")); //$NON-NLS-1$
        filePath.setBorder(BorderFactory.createEmptyBorder());

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

        final Color lineBorderColor = LookAndFeelManager.WINDOWS_HIGH_CONTRAST ? Color.WHITE : Color.GRAY;

        final JPanel filePathPanel = new JPanel();
        filePathPanel.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("SignDataPanel.2") + filePath.getText()); //$NON-NLS-1$
        filePathPanel.setFocusable(true);
        filePathPanel.setBorder(BorderFactory.createLineBorder(lineBorderColor));
        filePathPanel.setLayout(new GridBagLayout());

        final GridBagConstraints fpc = new GridBagConstraints();
        fpc.fill = GridBagConstraints.HORIZONTAL;
        fpc.insets = new Insets(5,  5,  5,  5);

        boolean isOpennable = false;

        if (fileTypeIcon != null) {
            filePathPanel.add(fileTypeIcon, fpc);
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

            final BufferedImage fileIconImage = ImageLoader.loadImage(fileIcon);
            final JLabel iconLabel = new JLabel(new ImageIcon(fileIconImage));
            iconLabel.setToolTipText(fileTooltip);
            iconLabel.setFocusable(false);
            filePathPanel.add(iconLabel, fpc);
        }

        // Boton de apertura del fichero firmado
        JButton openFileButton = null;
        if (isOpennable && signFile != null) {
            openFileButton = new JButton(SimpleAfirmaMessages.getString("SignDataPanel.3")); //$NON-NLS-1$
            openFileButton.setPreferredSize(new Dimension(150, 24));
            openFileButton.setMnemonic('c');
            openFileButton.setToolTipText(SimpleAfirmaMessages.getString("SignDataPanel.4")); //$NON-NLS-1$
            openFileButton.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("SignDataPanel.5")); //$NON-NLS-1$
            openFileButton.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("SignDataPanel.6")); //$NON-NLS-1$
            openFileButton.addActionListener(ae -> {
			    try {
			        Desktop.getDesktop().open(signFile);
			    }
			    catch (final Exception e) {
			    	LOGGER.warning(
						"Error abriendo el fichero con el visor por defecto: " + e //$NON-NLS-1$
					);
			    	AOUIFactory.showErrorMessage(
			            SimpleAfirmaMessages.getString("SignDataPanel.7"), //$NON-NLS-1$
			            SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
			            JOptionPane.ERROR_MESSAGE,
			            e
			        );
			    }
			});
        }

        fpc.weightx = 1.0;
        filePathPanel.add(filePath, fpc);

        if (openFileButton != null) {
        	fpc.weightx = 0.0;
        	fpc.ipady = 4;
            filePathPanel.add(openFileButton, fpc);
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
	            this.holderDescCertLabel.setText(SimpleAfirmaMessages.getString("CertificateInfo.1")); //$NON-NLS-1$
	            this.holderCertLabel.setText(certInfo.getHolderName());
	            this.holderCertLabel.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("SignDataPanel.46") + //$NON-NLS-1$
	            															SimpleAfirmaMessages.getString("CertificateInfo.1")); //$NON-NLS-1$
	            this.issuerDescCertLabel.setText(SimpleAfirmaMessages.getString("CertificateInfo.2")); //$NON-NLS-1$
	            this.issuerCertLabel.setText("<html><b>" + certInfo.getIssuerName() + "</b></html>"); //$NON-NLS-1$ //$NON-NLS-2$

            	// Este gestor se encargara de controlar los eventos de foco y raton
                final LabelLinkManager labelLinkManager = new LabelLinkManager(this.holderCertLabel);
                labelLinkManager.setLabelLinkListener(new CertInfoLabelLinkImpl(cert));
            }

            certDescPanel = new JPanel();
            certDescPanel.setFocusable(true);
            // Se agrega un borde y un padding al panel con la informacion del certificado
            certDescPanel.setBorder(BorderFactory.createCompoundBorder
            		(BorderFactory.createLineBorder(lineBorderColor),
            		BorderFactory.createEmptyBorder(5, 5, 5, 5)));
            certDescPanel.setLayout(new GridBagLayout());
            final GridBagConstraints c = new GridBagConstraints();
            c.fill = GridBagConstraints.BOTH;
            c.weightx = 0.0;
            c.weighty = 0.0;
            c.gridheight = 2;
            c.insets = new Insets(0, 0, 0, 10);
            certDescPanel.add(this.certIcon, c);
            c.weightx = 0.0;
            c.weighty = 0.0;
            c.gridx = 1;
            c.gridheight = 1;
            certDescPanel.add(this.holderDescCertLabel, c);
            c.weightx = 5.0;
            c.gridx = 2;
            c.insets = new Insets(0, 0, 0, 0);
            certDescPanel.add(this.holderCertLabel, c);
            c.weightx = 0.0;
            c.weighty = 1.0;
            c.gridx = 1;
            c.gridy = 1;
            c.insets = new Insets(0, 0, 0, 0);
            certDescPanel.add(this.issuerDescCertLabel, c);
            c.weightx = 5.0;
            c.gridx = 2;
            certDescPanel.add(this.issuerCertLabel, c);

            if (this.validateCertButton != null) {
                certDescPanel.add(this.validateCertButton);
                certDescPanel.add(Box.createRigidArea(new Dimension(5, 0)));
            }
            if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
                certDescPanel.setBackground(LookAndFeelManager.SECUNDARY_COLOR);
            }

            this.certDescText.setText(SimpleAfirmaMessages.getString("SignDataPanel.21")); //$NON-NLS-1$
            this.certDescText.setLabelFor(this.holderCertLabel);
            certDescPanel.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("SignDataPanel.21") //$NON-NLS-1$
            															+ this.holderDescCertLabel.getText()
            															+ this.holderCertLabel.getText()
            															+ this.issuerDescCertLabel.getText()
            															+ this.issuerCertLabel.getText());
        }

        // Panel el detalle de la firma
        CompleteSignInfo signInfo;
        try {
            signInfo = SignDataPanel.getSignInfo(sign, params);
        }
        catch (final Exception e) {
        	LOGGER.severe("Error obteniendo los datos de la firma: " + e); //$NON-NLS-1$
            signInfo = null;
        }

        this.currentSignInfo = signInfo;

        JTree signTree = null;
        String treeAccessibleDesc = ""; //$NON-NLS-1$
        if (signInfo != null) {
        	signTree = SignDataPanel.getSignDataTree(signInfo, extKeyListener, SignDataPanel.this);
        	final TreeNode root = (TreeNode) signTree.getModel().getRoot();
        	treeAccessibleDesc += root;
        	if (root.getChildCount() >= 0) {
        		for (final Enumeration<?> e = root.children(); e.hasMoreElements(); ) {
	        		final TreeNode n = (TreeNode)e.nextElement();
	        		treeAccessibleDesc += n.toString();
        		}
        	}
        }

        final JScrollPane detailPanel = new JScrollPane(signTree);
        detailPanel.setBorder(BorderFactory.createLineBorder(lineBorderColor));
        detailPanel.setFocusable(true);

        // En Apple siempre hay barras, y es el SO el que las pinta o no depende de si hacen falta
        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            detailPanel.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
            detailPanel.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
        }
        else {
        	detailPanel.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        }
        detailPanel.getVerticalScrollBar().setUnitIncrement(16);
    	detailPanel.getHorizontalScrollBar().setUnitIncrement(16);

        final JLabel detailPanelText = new JLabel(SimpleAfirmaMessages.getString("SignDataPanel.22")); //$NON-NLS-1$
        detailPanelText.setLabelFor(detailPanel);
        detailPanel.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("SignDataPanel.22") //$NON-NLS-1$
        															+ treeAccessibleDesc);

        // Establecemos la configuracion de color
        if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
            filePathPanel.setBackground(LookAndFeelManager.SECUNDARY_COLOR);
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

	public static void openCertificate(final X509Certificate cert) {
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
                SimpleAfirmaMessages.getString("SignDataPanel.23"), //$NON-NLS-1$
                SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
                JOptionPane.ERROR_MESSAGE,
                e
            );
        }
    }

    /** Recupera la informaci&oacute;n de la firma indicada.
     * @param signData Firma.
     * @param params Par&aacute;metros de la firma.
     * @return Informaci&oacute;n de la firma.
     * @throws IOException Si ocurren problemas relacionados con la lectura de los datos */
    private static CompleteSignInfo getSignInfo(final byte[] signData, final Properties params) throws IOException {
        final CompleteSignInfo signInfo = new CompleteSignInfo();
        signInfo.setSignData(signData);
        final AOSigner signer = AOSignerFactory.getSigner(signData, params);
        if (signer == null) {
        	LOGGER.warning("Formato de firma no reconocido"); //$NON-NLS-1$
            throw new IllegalArgumentException("Formato de firma no reconocido"); //$NON-NLS-1$
        }
        try {
            if (signer instanceof AOPDFSigner) {
            	signInfo.setSignInfo(((AOPDFSigner)signer).getSignInfo(signData, params));
        	} else {
        		signInfo.setSignInfo(signer.getSignInfo(signData));
        	}
        }
        catch (final Exception e) {
        	LOGGER.log(Level.WARNING, "Error al leer la informacion de la firma", e); //$NON-NLS-1$
        }
        try {
        	if (signer instanceof AOPDFSigner) {
        		signInfo.setSignsTree(((AOPDFSigner)signer).getSignersStructure(signData, params, true));
        	} else {
        		signInfo.setSignsTree(signer.getSignersStructure(signData, true));
        	}
        }
        catch (final Exception e) {
        	LOGGER.log(Level.WARNING, "Error al extraer el arbol de firmantes", e);  //$NON-NLS-1$
        	signInfo.setSignsTree(null);
        }
        try {
        	if (signer instanceof AOPDFSigner) {
        		signInfo.setData(((AOPDFSigner)signer).getData(signData, params));
        	} else {
        		signInfo.setData(signer.getData(signData));
        	}
        }
        catch (final Exception e) {
        	LOGGER.log(Level.WARNING, "Error al extraer los datos firmados", e);  //$NON-NLS-1$
        }
        try {
        	if (signer instanceof AOPDFSigner) {
        		signInfo.setTimestampsInfo(
            			TimestampsAnalyzer.getTimestamps(signData, params)
        			);
        	} else {
        		signInfo.setTimestampsInfo(
            			TimestampsAnalyzer.getTimestamps(signData)
        			);
        	}
        }
        catch (final Exception e) {
        	LOGGER.log(Level.WARNING, "Error al extraer los sellos de tiempo", e);  //$NON-NLS-1$
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
        DefaultMutableTreeNode signersBranch;
        final AOTreeModel signersTree = signInfo.getSignsTree();
        if (signersTree != null) {
        	final TreeModelManager treeManager = new TreeModelManager(signersTree);
        	signersBranch = treeManager.getSwingTree();
        	signersBranch.setUserObject(SimpleAfirmaMessages.getString("SignDataPanel.29")); //$NON-NLS-1$
        	root.add(signersBranch);
        }
        else {
        	signersBranch = new DefaultMutableTreeNode(
        			SimpleAfirmaMessages.getString("SignDataPanel.29")); //$NON-NLS-1$
        	final DefaultMutableTreeNode noSignersNode = new DefaultMutableTreeNode(
        			SimpleAfirmaMessages.getString("SignDataPanel.32")); //$NON-NLS-1$
        	signersBranch.add(noSignersNode);
        }
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

        final TreeFocusManager treeFocusManager = new TreeFocusManager(tree, nodeInfo -> {
		    if (nodeInfo instanceof AOSimpleSignInfo) {
		        openCertificate(((AOSimpleSignInfo) nodeInfo).getCerts()[0]);
		    }
		    else if (nodeInfo instanceof ShowFileLinkAction) {
		        ((ShowFileLinkAction) nodeInfo).action();
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

    public CompleteSignInfo getCurrentSignInfo() {
		return this.currentSignInfo;
	}

}
