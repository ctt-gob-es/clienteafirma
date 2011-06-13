/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo las licencias EUPL version 1.1 y GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.standalone.ui;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeSelectionModel;
import javax.xml.parsers.DocumentBuilderFactory;

import net.sf.jmimemagic.Magic;

import org.apache.batik.swing.JSVGCanvas;

import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.signers.AOSigner;
import es.gob.afirma.signers.AOSignerFactory;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.crypto.CompleteSignInfo;

/** Panel con detalles de una firma electr&oacute;nica. */
public final class SignDetailPanel extends JPanel {

    /** Tipo del resultado de la firma. */
    public enum SIGN_DETAIL_TYPE {
        /** Firma v&aacute;lida. */
        OK,
        /** Firma inv&aacute;lida. */
        KO,
        /** Firma generada en la misma aplicaci&oacute;n, se considera siempre v&aacute;lida. */
        GENERATED
    }

    /** Serial ID */
    private static final long serialVersionUID = 7567869419737753210L;

    private static final String FILE_ICON_PDF = "/resources/icon_pdf.png";
    private static final String FILE_ICON_XML = "/resources/icon_xml.png";
    private static final String FILE_ICON_BINARY = "/resources/icon_binary.png";

    private final JLabel resultTextLabel = new JLabel();
    private final JEditorPane descTextLabel = new JEditorPane();
    private JLabel filePathText;
    private final JLabel fileIcon = new JLabel();

    private JLabel certDescText;
    private JPanel certDescPanel;
    private JLabel certIcon;
    private JLabel certDescription;
    private JButton validateCertButton;

    private JScrollPane detailPanel;

    /** Construye el panel para mostrar el detalle de una firma electr&oacute;nica.
     * @param sig Firma electr&oacute;nica que se desea visualizar
     * @param sigPath Ruta del fichero de firma, si no se proporciona la firma en s&iacute; se
     *        usa para cargarla
     * @param signingCert Certificado usado para generar la &uacute;ltima firma
     * @param panelType Tipo de panel informativo de cabecera
     * @param fileTypeIcon Icono vectorial indicativo del tipo de contenido. Si es <code>null</code> se determina al vuelo y se usa una version
     *        <i>raster</i> */
    public SignDetailPanel(final byte[] sig,
                           final String sigPath,
                           final X509Certificate signingCert,
                           final SignDetailPanel.SIGN_DETAIL_TYPE panelType,
                           final JSVGCanvas fileTypeIcon) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                createUI(sig, sigPath, signingCert, panelType, fileTypeIcon);
            }
        });
    }

    /** Agrega el contenido gr&aacute;fico al panel. */
    private void createUI(byte[] sig,
                          final String sigPath,
                          final X509Certificate signingCert,
                          final SignDetailPanel.SIGN_DETAIL_TYPE panelType,
                          final JSVGCanvas fileTypeIcon) {

        this.setBackground(SimpleAfirma.WINDOW_COLOR);

        // Cargamos los datos de firma si no nos los proporcionaron en el constructor
        if (sig == null && sigPath != null) {
            final File signFile = new File(sigPath);
            if (!signFile.exists()) {
                Logger.getLogger("es.gob.afirma").severe("La ruta de firma proporcionada no corresponde a ningun fichero");
            }
            else if (signFile.canRead()) {
                Logger.getLogger("es.gob.afirma").severe("No se tienen permisos de lectura del fichero indicado");
            }
            else {
                InputStream fis = null;
                InputStream bis = null;
                try {
                    fis = new FileInputStream(signFile);
                    bis = new BufferedInputStream(fis);
                    sig = AOUtil.getDataFromInputStream(bis);
                }
                catch (final IOException e) {
                    Logger.getLogger("es.gob.afirma").severe("No se ha podido leer el fichero de firma: " + e);
                }
                finally {
                    try {
                        if (fis != null) fis.close();
                    }
                    catch (final Exception e) {}
                    try {
                        if (bis != null) bis.close();
                    }
                    catch (final Exception e) {}
                }
            }
        }

        final JPanel infoPanel = createResultMessagePanel(panelType);
        final JPanel componentPanel = createSignDataPanel(sigPath, sig);

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

    private JPanel createResultMessagePanel(final SIGN_DETAIL_TYPE type) {

        final JSVGCanvas resultOperationIcon = new JSVGCanvas();
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        try {
            resultOperationIcon.setDocument(dbf.newDocumentBuilder()
                                               .parse(this.getClass()
                                                          .getResourceAsStream("/resources/" + (type.equals(SIGN_DETAIL_TYPE.KO)
                                                                                                                                ? "ko_icon.svg"
                                                                                                                                : "ok_icon.svg"))));
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido cargar el icono de resultado o validez de firma, este no se mostrara: " + e);
        }

        this.descTextLabel.setContentType("text/html");
        this.descTextLabel.addHyperlinkListener(new HyperlinkListener() {
            @Override
            public void hyperlinkUpdate(final HyperlinkEvent he) {
                if (HyperlinkEvent.EventType.ACTIVATED.equals(he.getEventType())) {
                    try {
                        Desktop.getDesktop().browse(he.getURL().toURI());
                    }
                    catch (final Exception e) {
                        JOptionPane.showOptionDialog(SignDetailPanel.this,
                                                     "Error",
                                                     "No ha sido posible recuperar la informaci—n adicional,\n,pruebe a abrir la siguiente URL desde un navegador Web:\n" + he.getURL(),
                                                     JOptionPane.OK_OPTION,
                                                     JOptionPane.ERROR_MESSAGE,
                                                     null,
                                                     new Object[] {
                                                         "Cerrar "
                                                     },
                                                     null);
                    }
                }
            }
        });
        this.descTextLabel.setEditable(false);
        this.descTextLabel.setOpaque(false);

        switch (type) {
            case GENERATED:
                this.resultTextLabel.setText("Proceso de firma completado satisfactoriamente");
                this.descTextLabel.setText("<html><p>La firma cumple con los requisitos del esquema nacional de interoperabilidad en cuanto a firmas digitales y documentos firmados. <a href=\"http://www.google.com/\">M&aacute;s informaci&oacute;n</a>.</p></html>");
                break;
            case KO:
                this.resultTextLabel.setText("La firma no es v‡lida o no es una firma compatible con @firma");
                this.descTextLabel.setText("<html><p>Blah, blah, blah</p></html>");
                break;
            case OK:
                this.resultTextLabel.setText("La firma es v‡lida");
                this.descTextLabel.setText("<html><p>Para determinar la completa validez legal debe comprobar adem‡s la validez de los certificados usados para firmar</p></html>");
                break;
        }
        this.resultTextLabel.setFont(this.getFont().deriveFont(Font.BOLD, this.getFont().getSize() + 8));

        final JPanel infoPanel = new JPanel(new GridBagLayout());
        infoPanel.setBackground(SimpleAfirma.WINDOW_COLOR);

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
        infoPanel.add(this.resultTextLabel, c);
        c.weighty = 1.0;
        c.gridy = 1;
        c.insets = new Insets(0, 6, 5, 11);
        infoPanel.add(this.descTextLabel, c);

        return infoPanel;
    }

    /** Crea el panel que almacenar&aacute; la informaci&oacute;n de la firma
     * electr&oacute;nica.
     * @return Panel para almacenar la informaci&oacute;n de la firma. */
    private JPanel createSignDataPanel(final String path, final byte[] sign) {

        // Panel con la ruta del fichero

        // Texto con la ruta del fichero
        final JTextField filePath = new JTextField();
        filePath.getAccessibleContext().setAccessibleName("Fichero de firma");
        filePath.getAccessibleContext().setAccessibleDescription("Ruta del fichero de firma analizado");
        filePath.setFocusable(path != null);
        filePath.setFont(this.getFont().deriveFont(this.getFont().getSize() + 2));
        filePath.setBorder(BorderFactory.createEmptyBorder());
        filePath.setEditable(false);
        filePath.setCursor(new Cursor(Cursor.TEXT_CURSOR));
        filePath.setText((path != null) ? path : "Fichero de firma en memoria");

        // Etiqueta encima del cuadro con la ruta de fichero
        this.filePathText = new JLabel("Fichero firmado:");
        this.filePathText.setLabelFor(filePath);

        // Boton de apertura del fichero firmado
        final JButton openFileButton = new JButton("Abrir Fichero Firmado");
        openFileButton.setToolTipText("Abre el fichero firmado con la aplicaci\u00F3n configurada en su sistema operativo");
        openFileButton.getAccessibleContext().setAccessibleName("Bot\u00F3n para abrir los datos firmados");
        openFileButton.getAccessibleContext().setAccessibleDescription("Abre el documento firmado con la aplicaci\u00F3n configurada en el sistema");
        openFileButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent ae) {
                File openFile = null;
                if (path != null) {
                    openFile = new File(path);
                }
                if (!(openFile != null && openFile.exists())) {
                    // TODO: Detectar extension, crear el temporal y actualizar openFile
                    String extension = "txt";
                    try {
                        extension = Magic.getMagicMatch(sign).getExtension();
                    }
                    catch (final Exception e) {
                        Logger.getLogger("es.gob.afirma").warning("No se ha podido determinar el tipo de fichero, se considerara textual: " + e);
                    }
                    try {
                        openFile = File.createTempFile("afirma", extension);
                        openFile.deleteOnExit();
                    }
                    catch (final Exception e) {

                    }
                    System.out.println("POR HACER!!!");
                }

                try {
                    Desktop.getDesktop().open(openFile);
                }
                catch (final Exception e) {
                    JOptionPane.showOptionDialog(SignDetailPanel.this,
                                                 "Error",
                                                 "No se ha podido abrir el fichero,\ncompruebe que no ha sido manipulado mientras se ejecutaba esta aplicaci—n",
                                                 JOptionPane.OK_OPTION,
                                                 JOptionPane.ERROR_MESSAGE,
                                                 null,
                                                 new Object[] {
                                                     "Cerrar "
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
        filePathPanel.add(this.fileIcon);
        filePathPanel.add(Box.createRigidArea(new Dimension(11, 0)));
        filePathPanel.add(filePath);
        filePathPanel.add(Box.createRigidArea(new Dimension(11, 0)));
        filePathPanel.add(openFileButton);
        filePathPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        filePathPanel.setBackground(SimpleAfirma.WINDOW_COLOR);

        // Panel con los datos del certificado
        this.certIcon = new JLabel();
        this.certDescription = new JLabel();
        this.validateCertButton = new JButton("Comprobar validez");
        this.validateCertButton.setToolTipText("Comprueba la validez del certificado contra el OCSP del su autoridad de certificaci\u00F3n");
        this.validateCertButton.getAccessibleContext().setAccessibleName("Bot\u00F3n para validar el certificado");
        this.validateCertButton.getAccessibleContext()
                               .setAccessibleDescription("Comprueba la validez del certificado contra el OCSP de su autoridad de certificaci&oacute;n");

        this.certDescPanel = new JPanel();
        this.certDescPanel.setBorder(BorderFactory.createLineBorder(Color.GRAY));
        this.certDescPanel.setLayout(new BoxLayout(this.certDescPanel, BoxLayout.X_AXIS));
        this.certDescPanel.add(Box.createRigidArea(new Dimension(0, 40)));
        this.certDescPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        this.certDescPanel.add(this.certIcon);
        this.certDescPanel.add(Box.createRigidArea(new Dimension(11, 0)));
        this.certDescPanel.add(this.certDescription);
        this.certDescPanel.add(Box.createRigidArea(new Dimension(11, 0)));
        this.certDescPanel.add(this.validateCertButton);
        this.certDescPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        this.certDescPanel.setBackground(SimpleAfirma.WINDOW_COLOR);

        this.certDescText = new JLabel("Certificado de firma utilizado:");
        this.certDescText.setLabelFor(this.certDescPanel);

        // Panel el detalle de la firma
        this.detailPanel = new JScrollPane();

        final JLabel detailPanelText = new JLabel("Datos de la firma:");
        detailPanelText.setLabelFor(this.detailPanel);

        final JPanel certDataPanel = new JPanel(new GridBagLayout());
        certDataPanel.setBackground(new Color(0, 0, 0, 0));

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.weightx = 1.0;
        c.gridy = 0;
        c.insets = new Insets(11, 0, 0, 0);
        certDataPanel.add(this.filePathText, c);
        c.gridy = 1;
        c.insets = new Insets(0, 0, 0, 0);
        certDataPanel.add(filePathPanel, c);
        c.gridy = 2;
        c.insets = new Insets(11, 0, 0, 0);
        certDataPanel.add(this.certDescText, c);
        c.gridy = 3;
        c.insets = new Insets(0, 0, 0, 0);
        certDataPanel.add(this.certDescPanel, c);
        c.gridy = 4;
        c.insets = new Insets(11, 0, 0, 0);
        certDataPanel.add(detailPanelText, c);
        c.fill = GridBagConstraints.BOTH;
        c.weighty = 1.0;
        c.gridy = 5;
        c.insets = new Insets(0, 0, 0, 0);
        certDataPanel.add(this.detailPanel, c);

        return certDataPanel;
    }

    // /**
    // * Recupera la informaci&oacute;n de la firma indicada.
    // * @param signData Firma.
    // * @return Informaci&oacute;n de la firma.
    // */
    // private CompleteSignInfo getSignInfo(final byte[] signData) {
    // final CompleteSignInfo signInfo = new CompleteSignInfo();
    // signInfo.setSignData(signData);
    //
    // final AOSigner signer = AOSignerFactory.getSigner(signData);
    // if (signer == null) {
    // Logger.getLogger("es.gob.afirma").warning("Formato de firma no reconocido");
    // } else {
    // try {
    // signInfo.setSignInfo(signer.getSignInfo(signData));
    // } catch (final Exception e) {
    // Logger.getLogger("es.gob.afirma").warning("Error al leer la informacion de la firma: " + e);
    // }
    // signInfo.setSignsTree(signer.getSignersStructure(signData, true));
    // }
    //
    // return signInfo;
    // }

    // /**
    // * Muestra los datos extra&iacute;dos de la firma en el panel.
    // * @param signInfo Informaci&oacute;n extra&iacute;da de la firma.
    // */
    // private void showInfo(final CompleteSignInfo signInfo) {
    //
    // if (this.cert != null) {
    // final CertificateInfo certInfo = CertificateAnalizer.getCertInformation(this.cert);
    // this.certDescription.setText(certInfo.getDescriptionText());
    // this.certIcon.setIcon(new ImageIcon(certInfo.getIcon()));
    // if (certInfo.getOcspConfig() != null) {
    // this.validateCertButton.addActionListener(new ActionListener() {
    // @Override
    // public void actionPerformed(ActionEvent e) {
    // certInfo.getOcspConfig();
    // }
    // });
    // }
    // this.validateCertButton.setVisible(certInfo.getOcspConfig() != null);
    // }
    // this.certDescText.setVisible(this.cert != null);
    // this.certDescPanel.setVisible(this.cert != null);
    //
    // this.detailPanel.setViewportView(this.getSignDataTree(signInfo));
    // }

    // private JTree getSignDataTree(final CompleteSignInfo signInfo) {
    // final DefaultMutableTreeNode root = new DefaultMutableTreeNode();
    //
    // final DefaultMutableTreeNode signInfoBranch = new DefaultMutableTreeNode("Formato de firma");
    // signInfoBranch.add(new DefaultMutableTreeNode(signInfo.getSignInfo().getFormat()));
    // root.add(signInfoBranch);
    //
    // final TreeModelManager treeManager = new TreeModelManager(signInfo.getSignsTree());
    // final DefaultMutableTreeNode signersBranch = treeManager.getSwingTree();
    // signersBranch.setUserObject("Árbol de firmas del documento");
    // root.add(signersBranch);
    //
    // final DefaultTreeCellRenderer treeRenderer = new DefaultTreeCellRenderer();
    // treeRenderer.setLeafIcon(null);
    // treeRenderer.setClosedIcon(null);
    // treeRenderer.setOpenIcon(null);
    //
    // final JTree tree = new JTree(root);
    // tree.setCellRenderer(treeRenderer);
    // tree.setRootVisible(false);
    // tree.putClientProperty("JTree.lineStyle", "None");
    // tree.getSelectionModel().setSelectionMode(
    // TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);
    // for (int i = 0; i < tree.getRowCount(); i++) tree.expandRow(i);
    // return tree;
    // }

    // private boolean isXML(final byte[] data) {
    // try {
    // DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new ByteArrayInputStream(data));
    // }
    // catch(final Exception e) {
    // return false;
    // }
    // return true;
    // }

}
