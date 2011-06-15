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
import java.awt.event.MouseListener;
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
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeSelectionModel;

import es.gob.afirma.misc.Platform;
import es.gob.afirma.signers.AOPDFSigner;
import es.gob.afirma.signers.AOSigner;
import es.gob.afirma.signers.AOSignerFactory;
import es.gob.afirma.standalone.DataAnalizerUtil;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.crypto.CertificateAnalizer;
import es.gob.afirma.standalone.crypto.CertificateInfo;
import es.gob.afirma.standalone.crypto.CompleteSignInfo;

final class SignDataPanel extends JPanel {

    private static final long serialVersionUID = 4956746943438652928L;
    
    private static final String FILE_ICON_PDF = "/resources/icon_pdf.png";
    private static final String FILE_ICON_XML = "/resources/icon_xml.png";
    private static final String FILE_ICON_BINARY = "/resources/icon_binary.png";
    
    private final JLabel certDescText = new JLabel();
    private final JLabel filePathText = new JLabel();
    private final JLabel certIcon = new JLabel();
    private final JEditorPane certDescription = new JEditorPane();
    private JButton validateCertButton = null;
    
    public SignDataPanel(final String path, final byte[] sign, final JComponent fileTypeIcon, final X509Certificate cert) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                createUI(path, sign, fileTypeIcon, cert);
            }
        });
    }
    
    private void createUI(final String path, final byte[] sign, final JComponent fileTypeIcon, final X509Certificate cert) {

        setBackground(SimpleAfirma.WINDOW_COLOR);
        
        // Texto con la ruta del fichero
        final JTextField filePath = new JTextField();
        filePath.getAccessibleContext().setAccessibleName("Fichero de firma");
        filePath.getAccessibleContext().setAccessibleDescription("Ruta del fichero de firma analizado");
        filePath.setFont(this.getFont().deriveFont(this.getFont().getSize() + 2));
        filePath.setBorder(BorderFactory.createEmptyBorder());
        filePath.setEditable(false);
        filePath.setCursor(new Cursor(Cursor.TEXT_CURSOR));
        filePath.setText(path);
        filePath.addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(final MouseEvent me) {
                // me.isPopupTrigger() depende del Look & Feel y no se puede usar
                if (me.getButton() == MouseEvent.BUTTON3 && me.getClickCount() == 1) {
                    new CopyMenuItem(filePath).show(me.getComponent(), me.getX(), me.getY());
                }
            }
        });

        // Etiqueta encima del cuadro con la ruta de fichero
        this.filePathText.setText("Fichero firmado:");
        this.filePathText.setLabelFor(filePath);

        // Boton de apertura del fichero firmado
        final JButton openFileButton = new JButton("Ver fichero");
        openFileButton.setMnemonic('v');
        openFileButton.setToolTipText("Abre el fichero firmado con la aplicaci\u00F3n configurada en su sistema operativo");
        openFileButton.getAccessibleContext().setAccessibleName("Bot\u00F3n para abrir los datos firmados");
        openFileButton.getAccessibleContext().setAccessibleDescription("Abre el documento firmado con la aplicaci\u00F3n configurada en el sistema");
        openFileButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent ae) {
                try {
                    Desktop.getDesktop().open(new File(path));
                }
                catch (final Exception e) {
                    JOptionPane.showOptionDialog(SignDataPanel.this,
                         "No se ha podido abrir el fichero,\ncompruebe que no ha sido manipulado mientras se ejecutaba esta aplicaci\u00F3n\ny que dispone de una aplicación instalada para visualizarlo",
                         "Error",
                         JOptionPane.OK_OPTION,
                         JOptionPane.ERROR_MESSAGE,
                         null,
                         new Object[] {
                             "Cerrar"
                         },
                         null);
                }
            }
        });

        final JPanel filePathPanel = new JPanel();
        filePathPanel.setBorder(BorderFactory.createLineBorder(Color.GRAY));
        filePathPanel.setLayout(new BoxLayout(filePathPanel, BoxLayout.X_AXIS));
        filePathPanel.add(Box.createRigidArea(new Dimension(0, 40)));
        filePathPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        if (fileTypeIcon != null) {
            filePathPanel.add(fileTypeIcon);
        } 
        else {
            String fileIcon;
            String fileTooltip;
            if (new AOPDFSigner().isValidDataFile(sign)) {
                fileIcon = FILE_ICON_PDF;
                fileTooltip = "Fichero de tipo Portable Document Format (PDF)";
            }
            else if (DataAnalizerUtil.isXML(sign)) {
                fileIcon = FILE_ICON_XML;
                fileTooltip = "Fichero de tipo XML";
            }
            else {
                fileIcon = FILE_ICON_BINARY;
                fileTooltip = "Fichero binario genŽrico";
            }
            final JLabel iconLabel = new JLabel(new ImageIcon(SignDetailPanel.class.getResource(fileIcon)));
            iconLabel.setToolTipText(fileTooltip);
            filePathPanel.add(iconLabel);
        }
        
        filePathPanel.add(Box.createRigidArea(new Dimension(11, 0)));
        filePathPanel.add(filePath);
        filePathPanel.add(Box.createRigidArea(new Dimension(11, 0)));
        filePathPanel.add(openFileButton);
        filePathPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        filePathPanel.setBackground(SimpleAfirma.WINDOW_COLOR);

        JPanel certDescPanel = null;
        
        // Panel con los datos del certificado
        if (cert != null) {
            CertificateInfo certInfo = CertificateAnalizer.getCertInformation(cert);
            this.certIcon.setIcon(certInfo.getIcon());

            this.certDescription.setEditable(false);
            this.certDescription.setContentType("text/html");
            this.certDescription.setOpaque(false);
            this.certDescription.setText(certInfo.getDescriptionText());
            this.certDescription.setToolTipText("Pulse en la descripción del certificado para obtener información adicional sobre él o anadirla al almacen de certificados de su sistema operativo");
            this.certDescription.getAccessibleContext().setAccessibleName("Descripción del certificado");
            this.certDescription.getAccessibleContext().setAccessibleDescription("Información resumida del certificado, pulse sobre esta para abrir una nueva ventana con información adicional");
            this.certDescription.addHyperlinkListener(new HyperlinkListener() {
                @Override
                public void hyperlinkUpdate(final HyperlinkEvent he) {
                    if (HyperlinkEvent.EventType.ACTIVATED.equals(he.getEventType())) {
                        try {
                            File tmp = File.createTempFile("afirma", ".cer");
                            tmp.deleteOnExit();
                            final OutputStream fos = new FileOutputStream(tmp);
                            final OutputStream bos = new BufferedOutputStream(fos);
                            bos.write(cert.getEncoded());
                            try { bos.flush(); } catch(final Exception e) {}
                            try { bos.close(); } catch(final Exception e) {}
                            try { fos.close(); } catch(final Exception e) {}
                            Desktop.getDesktop().open(tmp);
                        }
                        catch(final Exception e) {
                            JOptionPane.showOptionDialog(SignDataPanel.this,
                                 "No se ha podido abrir el fichero,\ncompruebe que de una aplicación instalada para visualizar Certificados X.509",
                                 "Error",
                                 JOptionPane.OK_OPTION,
                                 JOptionPane.ERROR_MESSAGE,
                                 null,
                                 new Object[] {
                                     "Cerrar"
                                 },
                                 null
                             );
                        }
                    }
                }
            });

            if (certInfo.getOcspConfig() != null) {
                this.validateCertButton = new JButton();
                this.validateCertButton.setText("Comprobar validez");
                this.validateCertButton.setMnemonic('c');
                this.validateCertButton.setToolTipText("Comprueba la validez del certificado contra el OCSP del su autoridad de certificaci\u00F3n");
                this.validateCertButton.getAccessibleContext().setAccessibleName("Bot\u00F3n para validar el certificado");
                this.validateCertButton.getAccessibleContext()
                .setAccessibleDescription("Comprueba la validez del certificado contra el OCSP de su autoridad de certificaci&oacute;n");
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
            certDescPanel.setBackground(SimpleAfirma.WINDOW_COLOR);

            this.certDescText.setText("Certificado de firma utilizado:");
            this.certDescText.setLabelFor(this.certDescription);
        }

        // Panel el detalle de la firma
        final CompleteSignInfo signInfo = this.getSignInfo(sign);
        final JScrollPane detailPanel = new JScrollPane(this.getSignDataTree(signInfo));
        // En Apple siempre hay barras, y es el SO el que las pinta o no si hacen o no falta
        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            detailPanel.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
            detailPanel.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
        }

        final JLabel detailPanelText = new JLabel("Datos de la firma:");
        detailPanelText.setLabelFor(detailPanel);

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

    /**
     * Recupera la informaci&oacute;n de la firma indicada.
     * @param signData Firma.
     * @return Informaci&oacute;n de la firma.
     */
    private CompleteSignInfo getSignInfo(final byte[] signData) {
        final CompleteSignInfo signInfo = new CompleteSignInfo();
        signInfo.setSignData(signData);
        final AOSigner signer = AOSignerFactory.getSigner(signData);
        if (signer == null) {
            Logger.getLogger("es.gob.afirma").warning("Formato de firma no reconocido");
        } 
        else {
            try {
                signInfo.setSignInfo(signer.getSignInfo(signData));
            } 
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").warning("Error al leer la informacion de la firma: " + e);
            }
            signInfo.setSignsTree(signer.getSignersStructure(signData, true));
        }
        return signInfo;
    }

    private JTree getSignDataTree(final CompleteSignInfo signInfo) {
        final DefaultMutableTreeNode root = new DefaultMutableTreeNode();

        // Formato de firma
        final DefaultMutableTreeNode signInfoBranch = new DefaultMutableTreeNode("Formato de firma");
        signInfoBranch.add(new DefaultMutableTreeNode(signInfo.getSignInfo().getFormat()));
        root.add(signInfoBranch);

        // Datos firmados
        final DefaultMutableTreeNode dataInfoBranch = new DefaultMutableTreeNode("Datos firmados");
        dataInfoBranch.add(new DefaultMutableTreeNode("Ver datos de firma"));
        root.add(dataInfoBranch);

        // Arbol de firmantes
        final TreeModelManager treeManager = new TreeModelManager(signInfo.getSignsTree());
        final DefaultMutableTreeNode signersBranch = treeManager.getSwingTree();
        signersBranch.setUserObject("\u00C1rbol de firmas del documento");
        root.add(signersBranch);

        final DefaultTreeCellRenderer treeRenderer = new DefaultTreeCellRenderer();
        treeRenderer.setLeafIcon(null);
        treeRenderer.setClosedIcon(null);
        treeRenderer.setOpenIcon(null);
        
        final JTree tree = new JTree(root);
        tree.setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));
        
        tree.addMouseListener(new MouseListener() {
            @Override public void mouseReleased(MouseEvent e) {}
            @Override public void mouseExited(MouseEvent e) { }
            @Override public void mouseEntered(MouseEvent e) { }
            @Override public void mouseClicked(MouseEvent e) { }
            
            @Override
            public void mousePressed(MouseEvent e) {
//                ((JTree) e.getSource()).get
//              System.out.println("Clicamos elemento en: " + .);   
            }
        });
        tree.addTreeSelectionListener(new TreeSelectionListener() {
            
            @Override
            public void valueChanged(TreeSelectionEvent e) {
                DefaultMutableTreeNode node = (DefaultMutableTreeNode) tree.getLastSelectedPathComponent();

                if (node == null) {
                    return;
                }

                Object nodeInfo = node.getUserObject();
                System.out.println(nodeInfo);
            }
        });
        tree.setCellRenderer(treeRenderer);
        
        tree.setRootVisible(false);
        tree.putClientProperty("JTree.lineStyle", "None");
        tree.getSelectionModel().setSelectionMode(
                TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);
        for (int i = 0; i < tree.getRowCount(); i++) tree.expandRow(i);
        
        return tree;
    }
    
}
