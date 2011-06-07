package es.gob.afirma.standalone.ui;

import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeSelectionModel;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.batik.swing.JSVGCanvas;
import org.w3c.dom.Document;

import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.signers.AOPDFSigner;
import es.gob.afirma.signers.AOSigner;
import es.gob.afirma.signers.AOSignerFactory;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.crypto.CertificateAnalizer;
import es.gob.afirma.standalone.crypto.CertificateInfo;
import es.gob.afirma.standalone.crypto.CompleteSignInfo;

public final class SignDetailPanel extends JPanel {

    /** Serial ID */
    private static final long serialVersionUID = 7567869419737753210L;

    private static final String FILE_ICON_PDF = "/resources/icon_pdf.png";
    private static final String FILE_ICON_XML = "/resources/icon_xml.png";
    private static final String FILE_ICON_BINARY = "/resources/icon_binary.png";
    
    /** Contenedor padre de la ventana. */
    private final Container parent; 
    
    /** Configuraci&oacute;n general para la ventana. */
    private final Properties config;

    /** Firma electr&oacute;nica que se desea visualizar. */
    private byte[] sign = null;
    
    /** Certificado utilizado para firmar el documento. */
    private X509Certificate cert = null;
    
    /** Ruta del fichero firmado. */
    private String dataFilePath = null;
    
    /**
     * Construye el panel para mostrar el detalle de una firma electr&oacute;nica.
     * @param parent Componente padre sobre el que se muestra el panel.
     * @param configuration Configuraci&oacute;n a aplicar a la interfaz.
     */
    public SignDetailPanel(final Container parent, final Properties configuration) throws IOException {
        this(parent, configuration, null);
    }
    
    /**
     * Construye el panel para mostrar el detalle de una firma electr&oacute;nica.
     * @param parent Componente padre sobre el que se muestra el panel.
     * @param configuration Configuraci&oacute;n a aplicar a la interfaz.
     * @param sign Firma electr&oacute;nica que se desea visualizar.
     */
    public SignDetailPanel(final Container parent, final Properties configuration, final byte[] sign) throws IOException {
        this.parent = parent;
        this.config = configuration;
        this.sign = sign;
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                createUI();
            }
        });
    }
    
    /**
     * Agrega el contenido gr&aacute;fico al panel. 
     */
    private void createUI() {
        
        this.setBackground(SimpleAfirma.WINDOW_COLOR);
        
        final JPanel infoPanel = createResultMessagePanel();
        final JPanel componentPanel = createSignDataPanel();
        
        setLayout(new GridBagLayout());
        
        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.insets = new Insets(11, 11, 0, 11);
        add(infoPanel, c);
        c.weighty = 1.0;
        c.gridy = 1;
        c.insets = new Insets(11, 11, 11, 11);
        add(componentPanel, c);
    }
   
    private JSVGCanvas resultOperationIcon;
    private JLabel resultTextLabel;
    private JLabel descTextLabel;
    
    private JPanel createResultMessagePanel() {
        resultOperationIcon = new JSVGCanvas();
        resultTextLabel = new JLabel();
        resultTextLabel.setFont(this.getFont().deriveFont(Font.BOLD, this.getFont().getSize() + 8));
        descTextLabel = new JLabel();
        descTextLabel.setVerticalAlignment(SwingConstants.TOP);

        final JPanel infoPanel = new JPanel(new GridBagLayout());
        infoPanel.setBackground(new Color(0, 0, 0, 0));
        
        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 0.0;
        c.weighty = 0.0;
        c.ipadx = 20;
        c.ipady = 20;
        c.gridheight = 2;
        c.insets = new Insets(11, 11, 11, 5);
        infoPanel.add(resultOperationIcon, c);
        c.weightx = 1.0;
        c.gridx = 1;
        c.gridheight = 1;
        c.insets = new Insets(11, 6, 0, 11);
        infoPanel.add(resultTextLabel, c);
        c.weighty = 1.0;
        c.gridy = 1;
        c.insets = new Insets(0, 6, 5, 11);
        infoPanel.add(descTextLabel, c);
        
        return infoPanel;
    }

    private JLabel filePathText;
    private JPanel filePathPanel;
    private JLabel fileIcon;
    private JTextField filePath;
    
    private JLabel certDescText;
    private JPanel certDescPanel;
    private JLabel certIcon;
    private JLabel certDescription;
    private JButton validateCertButton;
    
    private JScrollPane detailPanel;
    
    /**
     * Crea el panel que almacenar&aacute; la informaci&oacute;n de la firma
     * electr&oacute;nica.
     * @return Panel para almacenar la informaci&oacute;n de la firma.
     */
    private JPanel createSignDataPanel() {
        
        // Panel con la ruta del fichero
        fileIcon = new JLabel();
        filePath = new JTextField();
        filePath.setFont(this.getFont().deriveFont(this.getFont().getSize() + 2));
        filePath.setBorder(BorderFactory.createEmptyBorder());
        filePath.setEditable(false);
        final JButton showFileButton = new JButton("Ver Fichero Firmado");
        
        filePathPanel = new JPanel();
        filePathPanel.setBorder(BorderFactory.createLineBorder(Color.GRAY));
        filePathPanel.setLayout(new BoxLayout(filePathPanel, BoxLayout.X_AXIS));
        filePathPanel.add(Box.createRigidArea(new Dimension(0, 40)));
        filePathPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        filePathPanel.add(fileIcon);
        filePathPanel.add(Box.createRigidArea(new Dimension(11, 0)));
        filePathPanel.add(filePath);
        filePathPanel.add(Box.createRigidArea(new Dimension(11, 0)));
        filePathPanel.add(showFileButton);
        filePathPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        
        filePathText = new JLabel("Fichero firmado:");
        filePathText.setLabelFor(filePathPanel);

        // Panel con los datos del certificado
        certIcon = new JLabel();
        certDescription = new JLabel();
        validateCertButton = new JButton("Comprobar validez");
        
        certDescPanel = new JPanel();
        certDescPanel.setBorder(BorderFactory.createLineBorder(Color.GRAY));
        certDescPanel.setLayout(new BoxLayout(certDescPanel, BoxLayout.X_AXIS));
        certDescPanel.add(Box.createRigidArea(new Dimension(0, 40)));
        certDescPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        certDescPanel.add(certIcon);
        certDescPanel.add(Box.createRigidArea(new Dimension(11, 0)));
        certDescPanel.add(certDescription);
        certDescPanel.add(Box.createRigidArea(new Dimension(11, 0)));
        certDescPanel.add(validateCertButton);
        certDescPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        
        certDescText = new JLabel("Certificado de firma utilizado:");
        certDescText.setLabelFor(certDescPanel);
        
        // Panel el detalle de la firma
        detailPanel = new JScrollPane();
        
        
        final JLabel detailPanelText = new JLabel("Datos de la firma:");
        detailPanelText.setLabelFor(detailPanel);

        final JPanel certDataPanel = new JPanel(new GridBagLayout());
        certDataPanel.setBackground(new Color(0, 0, 0, 0));
        
        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.weightx = 1.0;
        c.gridy = 0;
        c.insets = new Insets(11, 0, 0, 0);
        certDataPanel.add(filePathText, c);
        c.gridy = 1;
        c.insets = new Insets(0, 0, 0, 0);
        certDataPanel.add(filePathPanel, c);
        c.gridy = 2;
        c.insets = new Insets(11, 0, 0, 0);
        certDataPanel.add(certDescText, c);
        c.gridy = 3;
        c.insets = new Insets(0, 0, 0, 0);
        certDataPanel.add(certDescPanel, c);
        c.gridy = 4;
        c.insets = new Insets(11, 0, 0, 0);
        certDataPanel.add(detailPanelText, c);
        c.fill = GridBagConstraints.BOTH;
        c.weighty = 1.0;
        c.gridy = 5;
        c.insets = new Insets(0, 0, 0, 0);
        certDataPanel.add(detailPanel, c);
        
        return certDataPanel;
    }
    
    /**
     * Establece la firma que se desea visualizar.
     * @param sign Firma que se desea visualizar.
     */
    public void setSign(final byte[] sign) {
        this.sign = sign;
    }

    /**
     * Establece la ruta de la firma que se desea visualizar.
     * @param signPath Ruta del fichero de firma.
     * @throws IOException Cuando ocurre un error al cargar el fichero de firma.
     */
    public void setSign(final String signPath) throws IOException {
        
        if (signPath == null)
            throw new NullPointerException("No se ha indicado la ruta del fichero de firma");
        
        final URI uri;
        final InputStream is;
        try {
            uri = AOUtil.createURI(signPath);
            is = AOUtil.loadFile(uri, this, true);
        } catch (final Exception e) {
            throw new IOException("Error en la carga del fichero de firma", e);
        }
        this.sign = AOUtil.getDataFromInputStream(is);
        try { is.close(); } catch (final Exception e) { }
    }
    
    
    /**
     * Establece el certificado utilizado para la firma.
     * @param certificate Certificado de firma.
     */
    public void setCertificate(final X509Certificate certificate) {
        this.cert = certificate;
    }
    
    /**
     * Establece el certificado utilizado para la firma.
     * @param certificate Certificado de firma.
     */
    public void setDataFilePath(final String path) {
        this.dataFilePath = path;
    }

    /**
     * Carga en los distintos campos del panel, la informaci&oacute;n de la firma
     * establecida.
     * @throws NullPointerException Si no se configur&oacute; una firma electr&oacute;nica. 
     */
    public void loadSignData() {
        if (this.sign == null)
            throw new NullPointerException("No se configur&oacute; la firma electronica que se desea visualizar");
        
        
        final CompleteSignInfo signInfo;
        try {
            signInfo = this.getSignInfo(this.sign);
        } catch (final Exception e) {
            this.showOperationResult(false);
            return;
        }
        
        this.showOperationResult(true);
        this.showInfo(signInfo);
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
        } else {
            try {
                signInfo.setSignInfo(signer.getSignInfo(signData));
            } catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").warning("Error al leer la informacion de la firma: " + e);
            }
            signInfo.setSignsTree(signer.getSignersStructure(signData, true));
        }
        
        return signInfo;
    }
    
    /**
     * Muestra la cabecera correspondiente al resultado de an&aacute;lisis de la firma.
     * @param isOK Resultado de la operaci&oacute;n.
     */
    private void showOperationResult(final boolean isOK) {
        
     // Imagen central
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        final Document docum;
        try {
            docum = dbf.newDocumentBuilder().parse(
                this.getClass().getResourceAsStream(
                        "/resources/" + (isOK ? "ok_icon.svg" : "ko_icon.svg"))
            );
        }
        catch(final Exception e) {
            Logger.getLogger("es.gob.afirma").warning(
                "No se ha podido cargar la imagen explicativa de insercion de DNIe, esta no se mostrara: " + e
            );
            return;
        }
        
        
        if (isOK) {
            resultOperationIcon.setDocument(docum);
            resultTextLabel.setText("Proceso de firma completado satisfactoriamente");
            descTextLabel.setText(
                "<html><p>La firma cumple con los requisitos del esquema nacional de interoperabilidad en cuanto a firmas digitales y documentos firmados. <a href='#'>M&aacute;s informaci&oacute;n</a>"
            );
        } else {
            resultOperationIcon.setDocument(docum);
            resultTextLabel.setText("No se reconoce el formato de firma");
            descTextLabel.setText(
                "<html><p>No ha sido posible identificar el formato de la firma introducida."
            );
        }

    }
    
    /**
     * Muestra los datos extra&iacute;dos de la firma en el panel.
     * @param signInfo Informaci&oacute;n extra&iacute;da de la firma.
     */
    private void showInfo(final CompleteSignInfo signInfo) {
        
        if (this.dataFilePath != null) {
            this.filePath.setText(this.dataFilePath);
            
            try {
                final InputStream is = AOUtil.loadFile(AOUtil.createURI(this.dataFilePath), null, false);
                final byte[] data = AOUtil.getDataFromInputStream(is);
                try { is.close(); } catch (Exception e) { }

                String iconPath;
                if (new AOPDFSigner().isValidDataFile(data)) iconPath = FILE_ICON_PDF;
                else if (isXML(data)) iconPath = FILE_ICON_XML;
                else iconPath = FILE_ICON_BINARY;
                
                fileIcon.setIcon(new ImageIcon(Toolkit.getDefaultToolkit().getImage(
                        this.getClass().getResource(iconPath))));
            } catch (final Exception e) {
                Logger.getLogger("Error al leer el fichero de datos, se omitira esta informacion: " + e);
                this.dataFilePath = null;
            }
        }
        this.filePathText.setVisible(this.dataFilePath != null);
        this.filePathPanel.setVisible(this.dataFilePath != null);
        
        if (this.cert != null) {
            final CertificateInfo certInfo = CertificateAnalizer.getCertInformation(this.cert);
            this.certDescription.setText(certInfo.getDescriptionText());
            this.certIcon.setIcon(new ImageIcon(certInfo.getIcon()));
            if (certInfo.getOcspConfig() != null) {
                this.validateCertButton.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(ActionEvent e) {
                        certInfo.getOcspConfig();
                    }
                });
            }
            this.validateCertButton.setVisible(certInfo.getOcspConfig() != null);
        }
        this.certDescText.setVisible(this.cert != null);
        this.certDescPanel.setVisible(this.cert != null);


        detailPanel.setViewportView(this.getSignDataTree(signInfo));
    }
    
    private JTree getSignDataTree(final CompleteSignInfo signInfo) {
        final DefaultMutableTreeNode root = new DefaultMutableTreeNode();
        
        final DefaultMutableTreeNode signInfoBranch = new DefaultMutableTreeNode("Formato de firma");
        signInfoBranch.add(new DefaultMutableTreeNode(signInfo.getSignInfo().getFormat()));
        root.add(signInfoBranch);
        
        final TreeModelManager treeManager = new TreeModelManager(signInfo.getSignsTree());
        final DefaultMutableTreeNode signersBranch = treeManager.getSwingTree();
        signersBranch.setUserObject("Árbol de firmas del documento");
        root.add(signersBranch);
        
        final DefaultTreeCellRenderer treeRenderer = new DefaultTreeCellRenderer();
        treeRenderer.setLeafIcon(null);
        treeRenderer.setClosedIcon(null);
        treeRenderer.setOpenIcon(null);
        
        final JTree tree = new JTree(root);
        tree.setCellRenderer(treeRenderer);
        tree.setRootVisible(false);
        tree.putClientProperty("JTree.lineStyle", "None");
        tree.getSelectionModel().setSelectionMode(
                TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);
        for (int i = 0; i < tree.getRowCount(); i++) tree.expandRow(i);
        return tree;
    }

    private boolean isXML(final byte[] data) {
        try {
            DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new ByteArrayInputStream(data));
        }
        catch(final Exception e) {
            return false;
        }
        return true;
    }
    
    public static void main(String[] args) throws Exception {

        JDialog dialog = new JDialog();

        InputStream certIs = AOUtil.loadFile(AOUtil.createURI("C:/pruebas/cert.cer"), null, false);
        X509Certificate cert = (X509Certificate) CertificateFactory.getInstance("X509").generateCertificate(certIs);
        try { certIs.close(); } catch (Exception e) { }
        
        SignDetailPanel detailPanel = new SignDetailPanel(dialog, null);
        detailPanel.setSign("C:/salida/Entrada.odp.signed.csig");
        detailPanel.setDataFilePath("C:/pruebas/Entrada.pdf");
        detailPanel.setCertificate(cert);
        detailPanel.loadSignData();
        
        dialog.add(detailPanel);
        
        dialog.pack();
        
        dialog.setVisible(true);
    }
}
