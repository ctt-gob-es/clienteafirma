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

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
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
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.standalone.DataAnalizerUtil;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.Messages;
import es.gob.afirma.standalone.crypto.CertAnalyzer;
import es.gob.afirma.standalone.crypto.CertificateInfo;
import es.gob.afirma.standalone.crypto.CompleteSignInfo;

final class SignDataPanel extends JPanel {

    private static final long serialVersionUID = 4956746943438652928L;

    private static final String FILE_ICON_PDF = "/resources/icon_pdf.png";  //$NON-NLS-1$
    private static final String FILE_ICON_SIGN = "/resources/icon_sign.png"; //$NON-NLS-1$
    private static final String FILE_ICON_FACTURAE = "/resources/icon_facturae.png"; //$NON-NLS-1$

    private final JLabel certDescText = new JLabel();
    private final JLabel filePathText = new JLabel();
    private final JLabel certIcon = new JLabel();
    private final JEditorPane certDescription = new JEditorPane();
    private JButton validateCertButton = null;

    SignDataPanel(final File signFile, final byte[] sign, final JComponent fileTypeIcon, final X509Certificate cert) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                createUI(signFile, sign, fileTypeIcon, cert);
            }
        });
    }

    void createUI(final File signFile, final byte[] sign, final JComponent fileTypeIcon, final X509Certificate cert) {

        // Texto con la ruta del fichero
        final JTextField filePath = new JTextField();
        filePath.getAccessibleContext().setAccessibleName(Messages.getString("SignDataPanel.0")); //$NON-NLS-1$
        filePath.getAccessibleContext().setAccessibleDescription(Messages.getString("SignDataPanel.1")); //$NON-NLS-1$
        filePath.setBorder(BorderFactory.createEmptyBorder());
        
        filePath.setEditable(false);
        filePath.setFocusable(false);
        filePath.setText(signFile == null ? Messages.getString("SignDataPanel.24") : signFile.getAbsolutePath());  //$NON-NLS-1$
        filePath.addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(final MouseEvent me) {
                // me.isPopupTrigger() depende del Look & Feel y no se puede usar
                if (me.getButton() == MouseEvent.BUTTON3 && me.getClickCount() == 1) {
                    new CopyMenuItem(filePath, Messages.getString("SignDataPanel.30")).show(me.getComponent(), me.getX(), me.getY()); //$NON-NLS-1$
                }
            }
        });

        // Etiqueta encima del cuadro con la ruta de fichero
        this.filePathText.setText(Messages.getString("SignDataPanel.2")); //$NON-NLS-1$
        this.filePathText.setLabelFor(filePath);

        final JPanel filePathPanel = new JPanel();
        filePathPanel.setBorder(BorderFactory.createLineBorder(Color.GRAY));
        filePathPanel.setLayout(new BoxLayout(filePathPanel, BoxLayout.X_AXIS));
        filePathPanel.add(Box.createRigidArea(new Dimension(0, 40)));
        filePathPanel.add(Box.createRigidArea(new Dimension(5, 0)));

        final boolean isPDF = new AOPDFSigner().isValidDataFile(sign);
        
        if (fileTypeIcon != null) {
            filePathPanel.add(fileTypeIcon);
        }
        else {
            final String fileIcon;
            final String fileTooltip;
            if (DataAnalizerUtil.isPDF(sign)) {
                fileIcon = FILE_ICON_PDF;
                fileTooltip = Messages.getString("SignDataPanel.9"); //$NON-NLS-1$
            }
            else if (DataAnalizerUtil.isFacturae(sign)) {
                fileIcon = FILE_ICON_FACTURAE;
                fileTooltip = Messages.getString("SignDataPanel.10"); //$NON-NLS-1$
            }
            else {
                fileIcon = FILE_ICON_SIGN;
                fileTooltip = Messages.getString("SignDataPanel.31"); //$NON-NLS-1$
            }
            final JLabel iconLabel = new JLabel(new ImageIcon(SignDetailPanel.class.getResource(fileIcon)));
            iconLabel.setToolTipText(fileTooltip);
            iconLabel.setFocusable(false);
            filePathPanel.add(iconLabel);
        }
        
        // Boton de apertura del fichero firmado
        JButton openFileButton = null;
        if (isPDF && signFile != null && UIUtils.hasAssociatedApplication(signFile.getName().substring(signFile.getName().lastIndexOf(".")))) { //$NON-NLS-1$
            openFileButton = new JButton(Messages.getString("SignDataPanel.3")); //$NON-NLS-1$
            openFileButton.setPreferredSize(new Dimension(150, 24));
            openFileButton.setMnemonic('v');
            openFileButton.setToolTipText(Messages.getString("SignDataPanel.4")); //$NON-NLS-1$
            openFileButton.getAccessibleContext().setAccessibleName(Messages.getString("SignDataPanel.5")); //$NON-NLS-1$
            openFileButton.getAccessibleContext().setAccessibleDescription(Messages.getString("SignDataPanel.6")); //$NON-NLS-1$
            openFileButton.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent ae) {
                    try {
                        Desktop.getDesktop().open(signFile);
                    }
                    catch (final Exception e) {
                        UIUtils.showErrorMessage(
                                SignDataPanel.this,
                                Messages.getString("SignDataPanel.7"), //$NON-NLS-1$
                                Messages.getString("SignDataPanel.8"), //$NON-NLS-1$
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

	            // Para que se detecten apropiadamente los hipervinculos hay que establecer
	            // el tipo de contenido antes que el contenido
	            this.certDescription.setContentType("text/html"); //$NON-NLS-1$
	                            
	            this.certDescription.setEditable(false);
	            this.certDescription.setOpaque(false);
	            this.certDescription.setText(certInfo.getDescriptionText());
	            this.certDescription.setToolTipText(Messages.getString("SignDataPanel.12")); //$NON-NLS-1$
	            this.certDescription.getAccessibleContext().setAccessibleName(Messages.getString("SignDataPanel.13")); //$NON-NLS-1$
	            this.certDescription.getAccessibleContext().setAccessibleDescription(Messages.getString("SignDataPanel.14")); //$NON-NLS-1$
	            
	            final EditorFocusManager editorFocusManager = new EditorFocusManager (this.certDescription, new EditorFocusManagerAction() {
                    @Override
                    public void openHyperLink(final HyperlinkEvent he, final int linkIndex) {
                        openCertificate(cert);
                    }
                });
                this.certDescription.addFocusListener(editorFocusManager);
                this.certDescription.addKeyListener(editorFocusManager);
	            this.certDescription.addHyperlinkListener(editorFocusManager);
	            
	            if (certInfo.getCertVerifier() != null) {
	                this.validateCertButton = new JButton();
	                this.validateCertButton.setPreferredSize(new Dimension(150, 24));
	                this.validateCertButton.setText(Messages.getString("SignDataPanel.15")); //$NON-NLS-1$
	                this.validateCertButton.setMnemonic('c');
	                this.validateCertButton.setToolTipText(Messages.getString("SignDataPanel.16")); //$NON-NLS-1$
	                this.validateCertButton.getAccessibleContext().setAccessibleName(Messages.getString("SignDataPanel.17")); //$NON-NLS-1$
	                this.validateCertButton.getAccessibleContext()
	                .setAccessibleDescription(Messages.getString("SignDataPanel.18")); //$NON-NLS-1$
	                this.validateCertButton.addActionListener(new ActionListener() {
						@Override
						public void actionPerformed(final ActionEvent ae) {
						    SignDataPanel.this.setCursor(new Cursor(Cursor.WAIT_CURSOR));
							try {
								certInfo.getCertVerifier().checkCertificate(new X509Certificate[] { cert }, true);
								JOptionPane.showMessageDialog(SignDataPanel.this, Messages.getString("SignDataPanel.19"), Messages.getString("SignDataPanel.20"), JOptionPane.INFORMATION_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
							}
							catch(final Exception e) {
								e.printStackTrace();
							}
							finally {
							    SignDataPanel.this.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
							}
						}
					});
	            }

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

            this.certDescText.setText(Messages.getString("SignDataPanel.21")); //$NON-NLS-1$
            this.certDescText.setLabelFor(this.certDescription);
        }

        // Panel el detalle de la firma
        CompleteSignInfo signInfo;
        try {
            signInfo = this.getSignInfo(sign);
        } 
        catch (final Exception e) {
            signInfo = null;
        }
        final JScrollPane detailPanel = new JScrollPane(signInfo == null ? null : this.getSignDataTree(signInfo));
        // En Apple siempre hay barras, y es el SO el que las pinta o no si hacen o no falta
        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            detailPanel.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
            detailPanel.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
        }

        final JLabel detailPanelText = new JLabel(Messages.getString("SignDataPanel.22")); //$NON-NLS-1$
        detailPanelText.setLabelFor(detailPanel);

        // Establecemos la configuracion de color
        if (!LookAndFeelManager.HIGH_CONTRAST) {
            setBackground(LookAndFeelManager.WINDOW_COLOR);
            filePath.setBackground(LookAndFeelManager.WINDOW_COLOR);
            filePathPanel.setBackground(LookAndFeelManager.WINDOW_COLOR);
        }
        
        this.setLayout(new GridBagLayout());
        
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

    void openCertificate(final X509Certificate cert) {
        try {
            final File tmp = File.createTempFile("afirma", ".cer");  //$NON-NLS-1$//$NON-NLS-2$
            tmp.deleteOnExit();
            final OutputStream fos = new FileOutputStream(tmp);
            final OutputStream bos = new BufferedOutputStream(fos);
            bos.write(cert.getEncoded());
            try { bos.flush(); } catch(final Exception e) { /* Ignoramos los errores */ }
            try { bos.close(); } catch(final Exception e) { /* Ignoramos los errores */ }
            try { fos.close(); } catch(final Exception e) { /* Ignoramos los errores */ }
            Desktop.getDesktop().open(tmp);
        }
        catch(final Exception e) {
            UIUtils.showErrorMessage(
                    SignDataPanel.this,
                    Messages.getString("SignDataPanel.23"), //$NON-NLS-1$
                    Messages.getString("SignDataPanel.8"), //$NON-NLS-1$
                    JOptionPane.ERROR_MESSAGE
            );
        }
    }

    /**
     * Recupera la informaci&oacute;n de la firma indicada.
     * @param signData Firma.
     * @return Informaci&oacute;n de la firma.
     */
    private CompleteSignInfo getSignInfo(final byte[] signData) throws IllegalArgumentException {
        final CompleteSignInfo signInfo = new CompleteSignInfo();
        signInfo.setSignData(signData);
        final AOSigner signer = AOSignerFactory.getSigner(signData);
        if (signer == null) {
            Logger.getLogger("es.gob.afirma").warning("Formato de firma no reconocido"); //$NON-NLS-1$ //$NON-NLS-2$
            throw new IllegalArgumentException("Formato de firma no reconocido"); //$NON-NLS-1$
        }
        try {
            signInfo.setSignInfo(signer.getSignInfo(signData));
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("Error al leer la informacion de la firma: " + e); //$NON-NLS-1$ //$NON-NLS-2$
        }
        signInfo.setSignsTree(signer.getSignersStructure(signData, true));
        try {
            signInfo.setData(signer.getData(signData));
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("Error al extraer los datos firmados: " + e);  //$NON-NLS-1$//$NON-NLS-2$
        }
        return signInfo;
    }

    private JTree getSignDataTree(final CompleteSignInfo signInfo) {
        final DefaultMutableTreeNode root = new DefaultMutableTreeNode();

        // Formato de firma
        final DefaultMutableTreeNode signInfoBranch = new DefaultMutableTreeNode(Messages.getString("SignDataPanel.25")); //$NON-NLS-1$
        signInfoBranch.add(new DefaultMutableTreeNode(signInfo.getSignInfo().getFormat()));
        root.add(signInfoBranch);

        // Datos firmados
        final DefaultMutableTreeNode dataInfoBranch = new DefaultMutableTreeNode(Messages.getString("SignDataPanel.26")); //$NON-NLS-1$
        if (signInfo.getData() == null) {
            dataInfoBranch.add(new DefaultMutableTreeNode(Messages.getString("SignDataPanel.27"))); //$NON-NLS-1$
        }
        else {
            dataInfoBranch.add(new DefaultMutableTreeNode(new ShowFileLinkAction(Messages.getString("SignDataPanel.28"), signInfo.getData()))); //$NON-NLS-1$
        }
        root.add(dataInfoBranch);

        // Arbol de firmantes
        final TreeModelManager treeManager = new TreeModelManager(signInfo.getSignsTree());
        final DefaultMutableTreeNode signersBranch = treeManager.getSwingTree();
        signersBranch.setUserObject(Messages.getString("SignDataPanel.29")); //$NON-NLS-1$
        root.add(signersBranch);

        //final DefaultTreeCellRenderer treeRenderer = new DefaultTreeCellRenderer();
        final LinksTreeCellRenderer treeRenderer = new LinksTreeCellRenderer();
        treeRenderer.setLeafIcon(null);
        treeRenderer.setClosedIcon((Platform.OS.WINDOWS.equals(Platform.getOS()) || Platform.OS.MACOSX.equals(Platform.getOS())) ? null : UIManager.getDefaults().getIcon("TREE.collapsedIcon")); //$NON-NLS-1$
        treeRenderer.setOpenIcon((Platform.OS.WINDOWS.equals(Platform.getOS()) || Platform.OS.MACOSX.equals(Platform.getOS())) ? null : UIManager.getDefaults().getIcon("TREE.expandedIcon")); //$NON-NLS-1$

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
                    openCertificate(((AOSimpleSignInfo) nodeInfo).getCerts()[0]);
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

        for (int i = 0; i < tree.getRowCount(); i++) {
            tree.expandRow(i);
        }

        return tree;
    }

}
