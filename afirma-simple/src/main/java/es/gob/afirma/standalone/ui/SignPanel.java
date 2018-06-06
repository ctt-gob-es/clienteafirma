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
import java.awt.EventQueue;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTarget;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;
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
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.xades.AOFacturaESigner;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.DataAnalizerUtil;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.plugins.DataProcessAction;
import es.gob.afirma.standalone.plugins.InputData;
import es.gob.afirma.standalone.plugins.PluginIntegrationWindow;
import es.gob.afirma.standalone.ui.SignOperationConfig.CryptoOperation;
import es.gob.afirma.standalone.ui.pdf.VisiblePdfSignatureManager;
import es.gob.afirma.standalone.ui.preferences.PreferencesManager;

/** Panel de selecci&oacute;n y firma del fichero objetivo.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class SignPanel extends JPanel implements LoadDataFileListener, SignatureExecutor, PluginButtonsContainer {

    private static final long serialVersionUID = -4828575650695534417L;

    static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static String[][] signersTypeRelation = new String[][] {
    	{"es.gob.afirma.signers.pades.AOPDFSigner", SimpleAfirmaMessages.getString("SignPanel.104")}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"es.gob.afirma.signers.xades.AOFacturaESigner", SimpleAfirmaMessages.getString("SignPanel.105")}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"es.gob.afirma.signers.xades.AOXAdESSigner", SimpleAfirmaMessages.getString("SignPanel.106")}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"es.gob.afirma.signers.cades.AOCAdESSigner", SimpleAfirmaMessages.getString("SignPanel.107")}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"es.gob.afirma.signers.odf.AOODFSigner", SimpleAfirmaMessages.getString("SignPanel.108")}, //$NON-NLS-1$ //$NON-NLS-2$
    	{"es.gob.afirma.signers.ooxml.AOOOXMLSigner", SimpleAfirmaMessages.getString("SignPanel.109")} //$NON-NLS-1$ //$NON-NLS-2$
    };

    private UpperPanel upperPanel;

    private LowerPanel lowerPanel;

    JPanel mainPluginsButtonsPanel = null;
    JPanel pluginButtonsPanel = null;

    private final JFrame window;

    private final JButton selectButton = new JButton();

    private final SimpleAfirma saf;

	private List<SignOperationConfig> signOperationConfigs;

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

    /** Crea la interfaz grafica del panel. */
	private void createUI() {

        if (!LookAndFeelManager.HIGH_CONTRAST) {
            setBackground(LookAndFeelManager.WINDOW_COLOR);
        }

		setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;

        // Panel superior con el mensaje de bienvenida y el boton de firma
        this.upperPanel = new UpperPanel(this);
        c.weightx = 1.0;
        c.gridy = 0;
        this.add(this.upperPanel, c);

        // Panel inferior con el panel al que arrastrar los ficheros y en el
        // que se muestra su informacion cuando se cargan
        this.lowerPanel = new LowerPanel(this);
        c.weighty = 1.0;
        c.gridy++;
        this.add(this.lowerPanel, c);

        // Panel adicional en el que se mostraran los botones de los plugins.
        // Este panel permanecera oculto si no hay botones
        this.mainPluginsButtonsPanel = buildMainPluginsButtonsPanel();
        c.weighty = 0.0;
        c.gridy++;
        this.add(this.mainPluginsButtonsPanel, c);

        refreshPluginButtonsContainer();

        setVisible(true);
    }

    JFrame getWindow() {
    	return this.window;
    }

    SimpleAfirma getSimpleAfirma() {
    	return this.saf;
    }

    JButton getSelectButton() {
    	return this.selectButton;
    }

    List<SignOperationConfig> getSignOperationConfigs() {
		return this.signOperationConfigs;
	}


	private CommonWaitDialog signWaitDialog = null;

    /** Firma el fichero actualmente cargado. */
    public void sign() {

    	// Mostramos el dialogo de espera
    	this.signWaitDialog = new CommonWaitDialog(
			getSimpleAfirma().getMainFrame(),
			SimpleAfirmaMessages.getString("SignPanel.48"), //$NON-NLS-1$
			SimpleAfirmaMessages.getString("SignPanel.50") //$NON-NLS-1$
		);

    	// Comprobamos primero si se esta firmando un unico PDF con firma visible,
    	// en cuyo caso calculamos las propiedades para la firma visible
    	if (this.signOperationConfigs.size() == 1 &&
    			this.signOperationConfigs.get(0).getSigner() instanceof AOPDFSigner &&
    			this.lowerPanel.getFilePanel() instanceof SignPanelFilePanel &&
    			((SignPanelFilePanel)this.lowerPanel.getFilePanel()).isVisibleSignature()) {

    		this.signWaitDialog.setMessage(SimpleAfirmaMessages.getString("SignPanelSignTask.0")); //$NON-NLS-1$

    		try {
    			VisiblePdfSignatureManager.getVisibleSignatureParams(
    					this.signOperationConfigs.get(0),
    					this,
    					getWindow());
    		}
    		catch (final AOCancelledOperationException e) {
    			this.signWaitDialog.dispose();
			}
    		catch (final Exception e) {
    			LOGGER.warning("No se pudo crear la firma visible PDF, se creara una firma invisible"); //$NON-NLS-1$
    			initSignTask(this.signOperationConfigs);
			}
    	}
    	else {
    		initSignTask(this.signOperationConfigs);
    	}
    }

    /**
     * Inicia el proceso de firma.
     * @param signConfigs Operaciones de firma a ejecutar.
     */
    @Override
    public void initSignTask(List<SignOperationConfig> signConfigs) {

    	setCursor(new Cursor(Cursor.WAIT_CURSOR));

    	new SignPanelSignTask(
        		this,
        		signConfigs,
        		this.saf.getAOKeyStoreManager(),
        		getCertFilters(),
        		this.signWaitDialog,
        		this,
        		this.saf
    		).execute();
        	this.signWaitDialog.setVisible(true);
    }

    @Override
    public void finishTask() {
    	this.signWaitDialog.dispose();
    	setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
    }

    /** M&eacute;todo para indicar a la clase que el <code>AOKeyStoreManager</code> est&aacute; listo para usarse. */
    public void notifyStoreReady() {
        if (this.signOperationConfigs != null && !this.signOperationConfigs.isEmpty()) {
        	 this.saf.setSignMenuCommandEnabled(true);
            this.lowerPanel.updateSignButtonState(true);
        }
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

	@Override
	public void loadFiles(File[] files) {

     	setCursor(new Cursor(Cursor.WAIT_CURSOR));

     	final File[] dataFiles = filterFiles(files);
     	if (dataFiles == null || dataFiles.length == 0) {
     		LOGGER.warning("No se ha cargado ningun fichero valido"); //$NON-NLS-1$
     	}
     	else {
     		final List<SignOperationConfig> configs = new ArrayList<>();
     		for (final File dataFile : dataFiles) {
     			try {
     				configs.add(prepareSignConfig(dataFile));
     			}
     			catch (final Exception e) {
     				LOGGER.warning(String.format("Error cargar el fichero %s, se salta al siguiente", dataFile.getAbsolutePath())); //$NON-NLS-1$
     			}
     		}
     		this.lowerPanel.loadDataInfo(configs);
     		this.lowerPanel.updateSignButtonState(this.saf.isKeyStoreReady());

     		this.signOperationConfigs = configs;
     	}

      	setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	}

	/**
	 * Filtra los ficheros para obtener un conjunto de ficheros v&aacute;lidos
	 * para la operaci&oacute;n.
	 * @param fileList Listado de ficheros seleccionados.
	 * @return Listado de ficheros a firmar o {@code null} si no se indicaron ficheros.
	 */
	 private static File[] filterFiles(File[] fileList) {

		 if (fileList == null || fileList.length == 0) {
			 return null;
		 }

		 final List<File> resultFiles = new ArrayList<>();

		 final List<File> tempFiles = new ArrayList<>();
		 for (final File file : fileList) {
			 tempFiles.add(file);
		 }

		 for (int i = 0; i < tempFiles.size(); i++) {
			 final File file = tempFiles.get(i);
			 if (file.exists() && file.canRead()) {
				 if (file.isFile()) {
					 resultFiles.add(file);
				 }
				 else {
					 for (final File subFile : file.listFiles()) {
						 tempFiles.add(subFile);
					 }
				 }
			 }
		 }
		 return resultFiles.toArray(new File[resultFiles.size()]);
	 }

	 private static SignOperationConfig prepareSignConfig(File dataFile) throws IOException {

		 final byte[] data;
		 try (final InputStream fis = new FileInputStream(dataFile)) {
			 data = AOUtil.getDataFromInputStream(fis);
		 }
		 catch(final OutOfMemoryError e) {
			 throw new IOException("No hay memoria suficiente para leer el fichero", e); //$NON-NLS-1$
		 }
		 catch(final Exception e) {
			 throw new IOException("No se ha podido leer el fichero", e); //$NON-NLS-1$
		 }

		 final SignOperationConfig config = new SignOperationConfig();
		 config.setDataFile(dataFile);
		 configureDataSigner(config, data);

		 return config;
	 }

	 private static void configureDataSigner(SignOperationConfig config, byte[] data) throws IOException {
		 // Comprobamos si es un fichero PDF
		 if (DataAnalizerUtil.isPDF(data)) {
			 config.setFileType(FileType.PDF);
			 config.setSigner(AOSignerFactory.getSigner(
					 PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_PDF))
					 );
		 }
		 // Comprobamos si es una factura electronica
		 else if (DataAnalizerUtil.isFacturae(data)) {
			 config.setFileType(FileType.FACTURAE);
			 config.setSigner(AOSignerFactory.getSigner(
					 PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_FACTURAE))
					 );
			 // No se pueden agregar firmas a una factura electronica ya firmada
			 if (config.getSigner() instanceof AOFacturaESigner &&
					 config.getSigner().isSign(data)) {
				 throw new IOException("No se puede firmar con formato FacturaE la factura ya firmada"); //$NON-NLS-1$
			 }
		 }
		 // Comprobamos si es un OOXML
		 else if (DataAnalizerUtil.isOOXML(data)) {
			 config.setFileType(FileType.OOXML);
			 config.setSigner(AOSignerFactory.getSigner(
					 PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_OOXML))
					 );
		 }
		 // Comprobamos si es un ODF
		 else if (DataAnalizerUtil.isODF(data)) {
			 config.setFileType(FileType.ODF);
			 config.setSigner(AOSignerFactory.getSigner(
					 PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_ODF))
					 );
		 }
		 // Comprobamos si es un fichero de firma CAdES o XAdES (los PDF, facturas, OOXML y ODF pasaran por las condiciones anteriores)
		 else {
			 config.setSigner(AOSignerFactory.getSigner(data));
			 if (config.getSigner() != null) {
				 if (config.getSigner() instanceof AOXAdESSigner) {
					 config.setFileType(FileType.SIGN_XADES);
				 }
				 else {
					 config.setFileType(FileType.SIGN_CADES);
				 }
				 config.setCryptoOperation(CryptoOperation.COSIGN);
			 }
			 // Comprobamos si es un fichero XML
			 else if (DataAnalizerUtil.isXML(data)) {
				 config.setFileType(FileType.XML);
				 config.setSigner(AOSignerFactory.getSigner(
						 PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_XML))
						 );
			 }
			 // Cualquier otro tipo de fichero
			 else {
				 config.setFileType(FileType.BINARY);
				 config.setSigner(AOSignerFactory.getSigner(
						 PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_BIN))
						 );
			 }
		 }
		 config.setSignatureFormatName(getSignatureName(config.getSigner()));
		 config.setExtraParams(ExtraParamsHelper.loadExtraParamsForSigner(config.getSigner()));
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

	/**
	 * Construye el panel en el que se mostraran los botones propios de los plugins instalados.
	 * @return Panel para los botones de los plugins.
	 */
	private JPanel buildMainPluginsButtonsPanel() {
		final JPanel mainPanel = new JPanel(new GridBagLayout());

		this.pluginButtonsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
    	c.weightx = 1.0;
    	c.gridy = 0;
    	mainPanel.add(new JSeparator(SwingConstants.HORIZONTAL), c);
    	c.gridy++;
    	mainPanel.add(this.pluginButtonsPanel, c);

    	if (!LookAndFeelManager.HIGH_CONTRAST) {
    		mainPanel.setBackground(LookAndFeelManager.WINDOW_COLOR);
    		this.pluginButtonsPanel.setBackground(LookAndFeelManager.WINDOW_COLOR);
    	}

    	return mainPanel;
	}

	@Override
    public void refreshPluginButtonsContainer() {

    	final List<PluginGraphicButton> pluginsButtons = PluginsUiComponentsBuilder.getPluginsButtons(
    			PluginIntegrationWindow.INPUT_DATA);

    	for (final PluginGraphicButton button : pluginsButtons) {
    		button.getGraphicButton().addActionListener(
    				new ActionListener() {
    					@Override
    					public void actionPerformed(ActionEvent e) {
    						try {
    							final List<InputData> inputDatas = new ArrayList<>();
    							final List<SignOperationConfig> configs = getSignOperationConfigs();
    							if (configs != null) {
    								for (final SignOperationConfig config : configs) {
    									final InputData data = new InputData();
    									data.setDataFile(config.getDataFile());
    									data.setSignatureFormat(config.getSignatureFormatName());
    									inputDatas.add(data);
    								}
    							}
    							new Thread(new Runnable() {
    								@Override
    								public void run() {
    									((DataProcessAction) button.getButton().getAction()).processData(
    											inputDatas.toArray(new InputData[inputDatas.size()]),
    											SwingUtilities.getWindowAncestor(SignPanel.this));
    								}
    							}).start();
    						}
    						catch (final Exception ex) {
    							LOGGER.log(Level.SEVERE, "La accion del boton devolvio un error", ex); //$NON-NLS-1$
    						}
    					}
    				});
    	}

    	EventQueue.invokeLater(new Runnable() {
			@Override
			public void run() {
		        if (pluginsButtons.isEmpty()) {
		        	SignPanel.this.mainPluginsButtonsPanel.setVisible(false);
		        }
		        else {
		        	SignPanel.this.mainPluginsButtonsPanel.setVisible(false);
		        	SignPanel.this.pluginButtonsPanel.removeAll();
		        	for (final PluginGraphicButton button : pluginsButtons) {
		        		SignPanel.this.pluginButtonsPanel.add(button.getGraphicButton());
		        	}
		        	SignPanel.this.mainPluginsButtonsPanel.setVisible(true);
		        }
			}
		});
    }

	private final class UpperPanel extends JPanel {

	    private static final long serialVersionUID = 533243192995645135L;

	    private final LoadDataFileListener loadDataListener;

	    UpperPanel(final LoadDataFileListener loadDataListener) {
	        super(true);
	        this.loadDataListener = loadDataListener;
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
				final File[] files;
				try {
			        files = AOUIFactory.getLoadFiles(
			    		SimpleAfirmaMessages.getString("SignPanel.35"), //$NON-NLS-1$
			    		null,
			    		null,
			    		null,
			    		null,
			    		false,
			    		true,
			    		AutoFirmaUtil.getDefaultDialogsIcon(),
			    		UpperPanel.this
					);
				}
				catch(final AOCancelledOperationException e1) {
					return;
				}

		    	this.loadDataListener.loadFiles(files);
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

	    private final LoadDataFileListener loadDataListener;

	    private JPanel filePanel;

	    private final JButton signButton;

	    private DropTarget dropTarget = null;

	    LowerPanel(final LoadDataFileListener loadDataListener) {
	        super(true);
	        this.loadDataListener = loadDataListener;
	        this.signButton = new JButton();
	        SwingUtilities.invokeLater(() -> createUI());
	    }

	    void createUI() {
	        setLayout(new BorderLayout(5, 5));
	        setBorder(BorderFactory.createEmptyBorder(15, 15, 15, 15));

	        this.filePanel = new ResizingTextPanel(SimpleAfirmaMessages.getString("SignPanel.41")); //$NON-NLS-1$
	        this.filePanel.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("SignPanel.42")); //$NON-NLS-1$
	        this.filePanel.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("SignPanel.43")); //$NON-NLS-1$
	        this.filePanel.setToolTipText(SimpleAfirmaMessages.getString("SignPanel.42")); //$NON-NLS-1$
	        this.filePanel.setFocusable(false);

	       this.dropTarget = new DropTarget(
	        		this.filePanel,
	        		DnDConstants.ACTION_COPY,
	        		new DropDataFileListener(this.loadDataListener),
	        		true);

	        this.filePanel.setDropTarget(this.dropTarget);

	        this.add(this.filePanel, BorderLayout.CENTER);

	        final JPanel buttonPanel = new JPanel(true);
	        this.signButton.setPreferredSize(new Dimension(160, 27));
           	this.signButton.setText(SimpleAfirmaMessages.getString("SignPanel.45")); //$NON-NLS-1$

	        this.signButton.setMnemonic('F');
	        this.signButton.setEnabled(false);
	        buttonPanel.add(this.signButton);
	        this.signButton.addActionListener(
	    		ae -> sign()
			);

	        // Establecemos la configuracion de color
	        if (!LookAndFeelManager.HIGH_CONTRAST) {
	            setBackground(LookAndFeelManager.WINDOW_COLOR);
	            this.filePanel.setBackground(Color.DARK_GRAY);
	            this.filePanel.setForeground(Color.LIGHT_GRAY);
	            buttonPanel.setBackground(LookAndFeelManager.WINDOW_COLOR);
	        }

	        this.add(buttonPanel, BorderLayout.PAGE_END);
	    }

	    public void loadDataInfo(List<SignOperationConfig> configs) {

	         remove(this.filePanel);

	         if (configs.size() == 1) {
	        	 this.filePanel = new SignPanelFilePanel(
	        			 configs.get(0),
	        			 this.dropTarget);
	         }
	         else if (configs.size() > 1) {
	        	 this.filePanel = new SignPanelMultiFilePanel(
	        			 configs,
	        			 this.dropTarget);
	         }


	         add(this.filePanel);
	         revalidate();
	    }

	    public void updateSignButtonState(boolean enable) {

	        if (enable) {
	            this.signButton.setIcon(null);
	            // En el caso de la cofirma de un documento, mostramos un texto distinto
                this.signButton.setText(SimpleAfirmaMessages.getString("SignPanel.45")); //$NON-NLS-1$
	            this.signButton.setToolTipText(null);
	            this.signButton.requestFocusInWindow();
	        }
	        else {
	        	this.signButton.setText(""); //$NON-NLS-1$
	        	this.signButton.setIcon(new ImageIcon(this.getClass().getResource("/resources/progress.gif"))); //$NON-NLS-1$
	        	this.signButton.setToolTipText(SimpleAfirmaMessages.getString("SignPanel.13")); //$NON-NLS-1$
	        }
	        this.signButton.setEnabled(enable);
	    }

	    JPanel getFilePanel() {
	    	return this.filePanel;
	    }
	}

     /**
      * Filtro de ficheros que s&oacute;lo admite ficheros (no directorios)
      * con permisos de lectura.
      */
     class OnlyFileFilter implements FileFilter {
 		@Override
 		public boolean accept(File pathname) {
 			return pathname.isFile() && pathname.canRead();
 		}

     }
}
