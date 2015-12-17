package es.gob.afirma.ui.visor.ui;

import java.awt.Color;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
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
import javax.swing.UIManager;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeSelectionModel;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.visor.crypto.CompleteSignInfo;

final class SignDataPanel extends JPanel {

    private static final long serialVersionUID = 4956746943438652928L;

    private static final String FILE_ICON_PDF = "/resources/images/icon_pdf_small.png";  //$NON-NLS-1$
    private static final String FILE_ICON_SIGN = "/resources/images/icon_sign_small.png"; //$NON-NLS-1$

    static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private final JLabel certDescText = new JLabel();
	final JTextField filePath = new JTextField();
    private final JPanel filePathPanel = new JPanel();
    private JPanel certDescPanel = null;
    private final JScrollPane detailPanel = new JScrollPane();
    private final JLabel certIcon = new JLabel();
    private final JEditorPane certDescription = new JEditorPane();
    private final JButton validateCertButton = null;

    SignDataPanel(final File signFile, final byte[] sign, final JComponent fileTypeIcon, final X509Certificate cert) {
    	this(signFile, sign, null, fileTypeIcon, cert);
    }

    SignDataPanel(final File signFile, final byte[] sign, final File dataFile, final JComponent fileTypeIcon, final X509Certificate cert) {
    	createUI(signFile, sign, dataFile, fileTypeIcon, cert);
    }

    SignDataPanel() {
    	createUI(null, null, null, null, null);
    }

    void createUI(final File signFile, final byte[] sign, final File dataFile, final JComponent fileTypeIcon, final X509Certificate cert) {

    	// Etiqueta encima del cuadro con la ruta de fichero
        final JLabel filePathText = new JLabel();
    	filePathText.setText(Messages.getString("SignDataPanel.2")); //$NON-NLS-1$
    	filePathText.setLabelFor(this.filePath);
    	filePathText.setDisplayedMnemonic(KeyEvent.VK_F);
    	Utils.setContrastColor(filePathText);
    	Utils.setFontBold(filePathText);

    	// Panel con la informacion de los datos cargados
        this.filePathPanel.setBorder(BorderFactory.createLineBorder(Color.GRAY));

        loadSignatureFileProperties(signFile, sign, fileTypeIcon);

        // Panel con los datos del certificado. Si este es nulo, no se creara
       	createCertificateDescriptionPanel(cert);

        // Panel con el detalle de la firma
        final JLabel detailPanelText = new JLabel(Messages.getString("SignDataPanel.22")); //$NON-NLS-1$
        detailPanelText.setLabelFor(this.detailPanel);
        detailPanelText.setDisplayedMnemonic(KeyEvent.VK_D);
        Utils.setContrastColor(detailPanelText);
        Utils.setFontBold(detailPanelText);

        loadSignatureDataProperties(sign, dataFile);

        this.setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.weightx = 1.0;
        c.gridy = 0;
        c.insets = new Insets(11, 0, 0, 0);
        this.add(filePathText, c);
        c.gridy = 1;
        c.insets = new Insets(0, 0, 0, 0);
        this.add(this.filePathPanel, c);
        c.gridy = 2;
        c.insets = new Insets(11, 0, 0, 0);
        if (cert == null) {
        	if (this.certDescPanel != null) {
        		this.remove(this.certDescPanel);
        		this.certDescPanel = null;
        	}
        }
        else {
            this.add(this.certDescText, c);
            c.gridy = 3;
            c.insets = new Insets(0, 0, 0, 0);
            this.add(this.certDescPanel, c);
            c.gridy = 4;
            c.insets = new Insets(11, 0, 0, 0);
        }
        this.add(detailPanelText, c);
        c.fill = GridBagConstraints.BOTH;
        c.weighty = 1.0;
        c.gridy = 5;
        c.insets = new Insets(0, 0, 0, 0);
        this.add(this.detailPanel, c);
    }

    private void createCertificateDescriptionPanel(final X509Certificate cert) {

    	if (cert == null) {
    		return;
    	}

    	final JPanel panelValidateCertButton = new JPanel(new GridLayout(1, 1));

        this.certDescPanel = new JPanel();
        this.certDescPanel.setBorder(BorderFactory.createLineBorder(Color.GRAY));
        this.certDescPanel.setLayout(new BoxLayout(this.certDescPanel, BoxLayout.X_AXIS));
        this.certDescPanel.add(Box.createRigidArea(new Dimension(0, 40)));
        this.certDescPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        this.certDescPanel.add(this.certIcon);
        this.certDescPanel.add(Box.createRigidArea(new Dimension(11, 0)));
        this.certDescPanel.add(this.certDescription);
        this.certDescPanel.add(Box.createRigidArea(new Dimension(11, 0)));
        if (this.validateCertButton != null) {
        	panelValidateCertButton.add(this.validateCertButton);
            this.certDescPanel.add(panelValidateCertButton);
            this.certDescPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        }

        this.certDescText.setText(Messages.getString("SignDataPanel.21")); //$NON-NLS-1$
        this.certDescText.setLabelFor(this.certDescription);
        this.certDescText.setDisplayedMnemonic(KeyEvent.VK_U);
	}

	private void loadSignatureFileProperties(final File signFile, final byte[] sign, final JComponent fileTypeIcon) {

		this.filePathPanel.removeAll();

        this.filePathPanel.setLayout(new BoxLayout(this.filePathPanel, BoxLayout.X_AXIS));
        this.filePathPanel.add(Box.createRigidArea(new Dimension(0, 40)));
        this.filePathPanel.add(Box.createRigidArea(new Dimension(5, 0)));

    	// Si no se indica una firma
    	if (signFile == null && sign == null) {
    		return;
    	}


    	// Texto con la ruta del fichero
    	this.filePath.getAccessibleContext().setAccessibleName(Messages.getString("SignDataPanel.0")); //$NON-NLS-1$
    	this.filePath.getAccessibleContext().setAccessibleDescription(Messages.getString("SignDataPanel.1")); //$NON-NLS-1$
    	this.filePath.setBorder(BorderFactory.createEmptyBorder());

    	this.filePath.setEditable(false);
    	this.filePath.setFocusable(true);
    	this.filePath.setText(signFile == null ? Messages.getString("SignDataPanel.24") : signFile.getAbsolutePath());  //$NON-NLS-1$
    	this.filePath.addMouseListener(new MouseAdapter() {
    		@Override
    		public void mousePressed(final MouseEvent me) {
    			// me.isPopupTrigger() depende del Look & Feel y no se puede usar
    			if (me.getButton() == MouseEvent.BUTTON3 && me.getClickCount() == 1) {
    				new CopyMenuItem(SignDataPanel.this.filePath, Messages.getString("SignDataPanel.30")).show(me.getComponent(), me.getX(), me.getY()); //$NON-NLS-1$
    			}
    		}
    	});
    	Utils.remarcar(this.filePath);
    	Utils.setFontBold(this.filePath);
    	Utils.setContrastColor(this.filePath);

    	this.filePath.addFocusListener(new FocusListener() {
    		@Override
    		public void focusLost(final FocusEvent e) {
    			SignDataPanel.this.filePath.setBorder(BorderFactory.createEmptyBorder());
    		}
    		@Override
    		public void focusGained(final FocusEvent e) { /* Metodo vacio */ }
    	});

    	final boolean isPDF = new AOPDFSigner().isValidDataFile(sign);

    	if (fileTypeIcon != null) {
    		this.filePathPanel.add(fileTypeIcon);
    	}
    	else {
    		final String fileIcon;
    		final String fileTooltip;
    		if (isPDF) {
    			fileIcon = FILE_ICON_PDF;
    			fileTooltip = Messages.getString("SignDataPanel.9"); //$NON-NLS-1$
    		}
    		else {
    			fileIcon = FILE_ICON_SIGN;
    			fileTooltip = Messages.getString("SignDataPanel.31"); //$NON-NLS-1$
    		}
    		final JLabel iconLabel = new JLabel(new ImageIcon(SignDataPanel.class.getResource(fileIcon)));
    		iconLabel.setToolTipText(fileTooltip);
    		iconLabel.setFocusable(false);
    		this.filePathPanel.add(iconLabel);
    	}

    	final JPanel panelOpenFileButton = new JPanel(new GridLayout(1, 1));
    	// Boton de apertura del fichero firmado
    	JButton openFileButton = null;
    	if (isPDF && signFile != null) {
    		openFileButton = new JButton(Messages.getString("SignDataPanel.3")); //$NON-NLS-1$
    		//openFileButton.setPreferredSize(new Dimension(150, 24));
    		openFileButton.setMnemonic(KeyEvent.VK_E);
    		openFileButton.setToolTipText(Messages.getString("SignDataPanel.4")); //$NON-NLS-1$
    		openFileButton.getAccessibleContext().setAccessibleName(Messages.getString("SignDataPanel.3")+ ". " + Messages.getString("SignDataPanel.5")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    		openFileButton.getAccessibleContext().setAccessibleDescription(Messages.getString("SignDataPanel.6")); //$NON-NLS-1$
    		openFileButton.addActionListener(new ActionListener() {
    			@Override
    			public void actionPerformed(final ActionEvent ae) {
    				try {
    					Desktop.getDesktop().open(signFile);
    				}
    				catch (final Exception e) {
    					CustomDialog.showMessageDialog(SignDataPanel.this, true, Messages.getString("SignDataPanel.7"), Messages.getString("SignDataPanel.8"), JOptionPane.ERROR_MESSAGE);  //$NON-NLS-1$//$NON-NLS-2$
    				}
    			}
    		});
    		Utils.setFontBold(openFileButton);
    		Utils.remarcar(openFileButton);
    	}

    	this.filePathPanel.add(Box.createRigidArea(new Dimension(11, 0)));
    	this.filePathPanel.add(this.filePath);
    	this.filePathPanel.add(Box.createRigidArea(new Dimension(11, 0)));
    	if (openFileButton != null) {
    		panelOpenFileButton.add(openFileButton);
    		this.filePathPanel.add(panelOpenFileButton);
    		this.filePathPanel.add(Box.createRigidArea(new Dimension(5, 0)));
    	}
    }

    private void loadSignatureDataProperties(final byte[] sign, final File dataFile) {

    	// Si no se indica una firma
    	if (sign == null) {
    		this.detailPanel.setViewportView(null);
    		return;
    	}

        CompleteSignInfo signInfo;
        try {
            signInfo = SignDataPanel.getSignInfo(sign);
        } catch (final Exception e) {
            signInfo = null;
        }

        this.detailPanel.setViewportView(signInfo == null ? null : this.getSignDataTree(signInfo, dataFile));

        // En Apple siempre hay barras, y es el SO el que las pinta o no si hacen o no falta
        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            this.detailPanel.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
            this.detailPanel.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
        }
    }

	void openCertificate(final X509Certificate cert) {
        try {
            final File tmp = File.createTempFile("afirma", ".cer");  //$NON-NLS-1$//$NON-NLS-2$
            tmp.deleteOnExit();
            try (
        		final OutputStream fos = new FileOutputStream(tmp);
        		final OutputStream bos = new BufferedOutputStream(fos);
    		) {
            	bos.write(cert.getEncoded());
            	bos.flush();
            }
            Desktop.getDesktop().open(tmp);
        }
        catch(final Exception e) {
        	CustomDialog.showMessageDialog(SignDataPanel.this, true, Messages.getString("SignDataPanel.23"), Messages.getString("SignDataPanel.8"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
        }
    }

    /** Recupera la informaci&oacute;n de la firma indicada.
     * @param signData Firma.
     * @return Informaci&oacute;n de la firma.
     * @throws IOException Cuando ocurre alg&uacute;n error durante la lectura de los datos.
     */
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
        	LOGGER.warning("Error al extraer la estructura de firmantes: " + e);  //$NON-NLS-1$
        	signInfo.setSignsTree(null);
        }
        try {
            signInfo.setData(signer.getData(signData));
        }
        catch (final Exception e) {
        	LOGGER.warning("Error al extraer los datos firmados: " + e);  //$NON-NLS-1$
        }
        return signInfo;
    }

    private JTree getSignDataTree(final CompleteSignInfo signInfo, final File dataFile) {
        final DefaultMutableTreeNode root = new DefaultMutableTreeNode();

        // Formato de firma
        final DefaultMutableTreeNode signInfoBranch = new DefaultMutableTreeNode(Messages.getString("SignDataPanel.25")); //$NON-NLS-1$
        signInfoBranch.add(new DefaultMutableTreeNode(signInfo.getSignInfo().getFormat()));
        root.add(signInfoBranch);

        // Datos firmados
        final DefaultMutableTreeNode dataInfoBranch = new DefaultMutableTreeNode(Messages.getString("SignDataPanel.26")); //$NON-NLS-1$
        if (dataFile != null) {
        	dataInfoBranch.add(new DefaultMutableTreeNode(new ShowFileLinkAction(Messages.getString("SignDataPanel.28"), dataFile, this))); //$NON-NLS-1$
        }
        else if (signInfo.getData() != null) {
            dataInfoBranch.add(new DefaultMutableTreeNode(new ShowFileLinkAction(Messages.getString("SignDataPanel.28"), signInfo.getData(), this))); //$NON-NLS-1$
        }
        else {
        	dataInfoBranch.add(new DefaultMutableTreeNode(new LoadFileLinkAction(Messages.getString("SignDataPanel.27"), this))); //$NON-NLS-1$
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
        treeRenderer.setClosedIcon(Platform.OS.WINDOWS.equals(Platform.getOS()) || Platform.OS.MACOSX.equals(Platform.getOS()) ? null : UIManager.getDefaults().getIcon("TREE.collapsedIcon")); //$NON-NLS-1$
        treeRenderer.setOpenIcon(Platform.OS.WINDOWS.equals(Platform.getOS()) || Platform.OS.MACOSX.equals(Platform.getOS()) ? null : UIManager.getDefaults().getIcon("TREE.expandedIcon")); //$NON-NLS-1$

        final JTree tree = new JTree(root);
        tree.setShowsRootHandles(true);
        tree.setRowHeight(25);
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
                else if (nodeInfo instanceof LoadFileLinkAction) {
                    ((LoadFileLinkAction) nodeInfo).action();
                    SignDataPanel.this.firePropertyChange(
                    		"dataLoaded", null, ((LoadFileLinkAction) nodeInfo).getFile()); //$NON-NLS-1$
                }
            }
        });

        tree.addMouseMotionListener(treeFocusManager);
        tree.addFocusListener(treeFocusManager);
        tree.addMouseListener(treeFocusManager);
        tree.addKeyListener(treeFocusManager);
        Utils.remarcar(tree);
		Utils.setContrastColor(tree);
		Utils.setFontBold(tree);

        for (int i = 0; i < tree.getRowCount(); i++) {
            tree.expandRow(i);
        }

        return tree;
    }

    void load(final File signFile, final byte[] sign, final File dataFile) {
    	this.loadSignatureFileProperties(signFile, sign, null);	//TODO: Introducir el tipo de icono
    	this.loadSignatureDataProperties(sign, dataFile);
    }
}
