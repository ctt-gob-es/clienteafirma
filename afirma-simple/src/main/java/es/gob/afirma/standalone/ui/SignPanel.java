/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_BIN;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_FACTURAE;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_ODF;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_OOXML;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_PDF;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_XML;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTarget;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.prefs.BackingStoreException;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.RuntimeConfigNeededException;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.CertificateFilter;
import es.gob.afirma.keystores.MultipleCertificateFilter;
import es.gob.afirma.keystores.filters.PseudonymFilter;
import es.gob.afirma.keystores.filters.SkipAuthDNIeFilter;
import es.gob.afirma.keystores.filters.rfc.KeyUsageFilter;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.pades.common.PdfExtraParams;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.signers.xades.XAdESExtraParams;
import es.gob.afirma.signvalidation.SignValider;
import es.gob.afirma.signvalidation.SignValiderFactory;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signvalidation.SignValidity.VALIDITY_ERROR;
import es.gob.afirma.standalone.DataAnalizerUtil;
import es.gob.afirma.standalone.DesktopUtil;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;
import es.gob.afirma.standalone.plugins.DataProcessAction;
import es.gob.afirma.standalone.plugins.InputData;
import es.gob.afirma.standalone.ui.SignOperationConfig.CryptoOperation;
import es.gob.afirma.standalone.ui.pdf.VisiblePdfSignatureManager;

/** Panel de selecci&oacute;n y firma del fichero objetivo.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class SignPanel extends JPanel implements LoadDataFileListener, SignatureExecutor, PluginButtonsContainer {

    private static final long serialVersionUID = -4828575650695534417L;

    static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** Anchura m&iacute;nima que deber&aacute; tener el panel. */
	private static final int MINIMUM_PANEL_WIDTH = 600;
	/** Altura m&iacute;nima que deber&aacute; tener el panel. */
	private static final int MINIMUM_PANEL_HEIGHT = 520;

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

	/**
	 * Valor bandera usado para determinar si esta corriendo una tarea en segunda plano y hay que mostrar un
	 * di&aacute;logo de espera.
	 */
    private boolean taskIsRunning = false;

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

        if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
            setBackground(LookAndFeelManager.DEFAULT_COLOR);
        }

		setLayout(new GridBagLayout());

        // Establecemos el que deberia ser el tamano minimo del panel antes de que se
        // muestren las barras de desplazamiento
        setPreferredSize(new Dimension(MINIMUM_PANEL_WIDTH, MINIMUM_PANEL_HEIGHT));

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

    	// Si se trata de firmar un documento que sabemos que es una firma invalida y no se
    	// se permite esto, se lanza un error
    	if (this.signOperationConfigs.size() == 1 &&
    			this.signOperationConfigs.get(0).getSignValidity() != null &&
    			this.signOperationConfigs.get(0).getSignValidity().get(0).getValidity() == SIGN_DETAIL_TYPE.KO &&
    			!PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_ALLOW_INVALID_SIGNATURES)) {

    		AOUIFactory.showErrorMessage(
    				SimpleAfirmaMessages.getString("SimpleAfirma.9"), //$NON-NLS-1$,
    				SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
    				JOptionPane.ERROR_MESSAGE,
    				null
    			);
    		return;
    	}

    	// Mostramos el dialogo de espera
    	this.signWaitDialog = new CommonWaitDialog(
			getSimpleAfirma().getMainFrame(),
			SimpleAfirmaMessages.getString("SignPanel.48"), //$NON-NLS-1$
			SimpleAfirmaMessages.getString("SignPanel.50") //$NON-NLS-1$
		);

		// Si procede, solicitamos confirmacion para firmar
		if (PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_CONFIRMTOSIGN)) {
			final ConfirmSignatureDialog dialog = new ConfirmSignatureDialog(
					this, this.signOperationConfigs.size());
			dialog.setVisible(true);

			// Si se cancela el dialogo, se interrumpe la operacion
			if (dialog.getResult() == null) {
				return;
			}

			// Si se solicita recordar la decision (solo en caso de permitir la firma)
			// guardamos la preferencia
			if (dialog.getResult().booleanValue()) {
				PreferencesManager.putBoolean(
						PreferencesManager.PREFERENCE_GENERAL_CONFIRMTOSIGN,
						false);
				try {
					PreferencesManager.flush();
				} catch (final BackingStoreException e) {
					LOGGER.warning("No se pudo guardar la preferencia del usuario: " + //$NON-NLS-1$
							PreferencesManager.PREFERENCE_GENERAL_CONFIRMTOSIGN);
				}
			}
		}

		// Establecemos la configuracion para las multifirmas que pueda haber
		setMultisignOpFromPreferences(this.signOperationConfigs);

    	// Comprobamos si se va a realizar alguna firma de PDF y establecemos si deben ser
		// firmas visibles o no
    	if (checkSignPDFOperationSign(this.signOperationConfigs)) {

    		this.signWaitDialog.setMessage(SimpleAfirmaMessages.getString("SignPanelSignTask.0")); //$NON-NLS-1$

    		boolean visibleSignature;
    		boolean visibleStamp;
    		String certificationLevel = null;

    		// Si se muestra el panel de configuracion de la firma PDF (caso de las firmas
    		// simples), se tomara la configuracion del panel
    		if (this.lowerPanel.getFilePanel() instanceof SignPanelFilePanel) {
    			visibleSignature = ((SignPanelFilePanel)this.lowerPanel.getFilePanel()).isVisibleSignature();
    			visibleStamp = ((SignPanelFilePanel)this.lowerPanel.getFilePanel()).isVisibleStamp();
    			certificationLevel = ((SignPanelFilePanel)this.lowerPanel.getFilePanel()).getCertificationLevel();
    		}
    		// Si no, se tomara de las preferencias
    		else {
    			visibleSignature = PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_PADES_VISIBLE);
    			visibleStamp = false;
    			final boolean allowCertifiedPdf = PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_PADES_VISIBLE);
    			if (allowCertifiedPdf) {
    				certificationLevel = PreferencesManager.get(PreferencesManager.PREFERENCE_PADES_DEFAULT_CERTIFICATION_LEVEL);
    			}
    		}

    		if (certificationLevel != null && !certificationLevel.equals(PdfExtraParams.CERTIFICATION_LEVEL_VALUE_TYPE_0)) {
    			for (final SignOperationConfig config : this.signOperationConfigs) {
    				config.addExtraParam(PdfExtraParams.CERTIFICATION_LEVEL, certificationLevel);
    			}
    		}

    		// Si corresponde, se muestran los dialogos necesarios para que el usuario seleccione la
    		// configuracion de firma visible PDF. Hacemos una copia de los datos para que, si se
    		// cancela, no quede establecida esa configuracion

    		final List<SignOperationConfig> configs = new ArrayList<>(this.signOperationConfigs.size());
    		for (final SignOperationConfig config : this.signOperationConfigs) {
    			configs.add(config.clone());
    		}

    		try {
    			VisiblePdfSignatureManager.getVisibleSignatureParams(
    					configs,
    					this,
    					visibleSignature,
    					visibleStamp,
    					getWindow());
    		}
    		catch (final AOCancelledOperationException e) {
    			this.signWaitDialog.dispose();
			}
    		catch (final Exception e) {
    			LOGGER.log(Level.WARNING, "No se pudo crear la firma visible PDF, se creara una firma invisible", e); //$NON-NLS-1$
    			initSignTask(configs);
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
    public void initSignTask(final List<SignOperationConfig> signConfigs) {

    	setCursor(new Cursor(Cursor.WAIT_CURSOR));

    	this.taskIsRunning = true;

    	new SignPanelSignTask(
        		this,
        		signConfigs,
        		this.saf.getAOKeyStoreManager(),
        		getCertFilters(),
        		this.signWaitDialog,
        		this,
        		this.saf
    		).execute();

    	// Cuidamos de no mostrar el dialogo cuando la tarea ya haya terminado
   		if (this.taskIsRunning) {
   			this.signWaitDialog.setVisible(true);
   		}
    }

    @Override
    public void relaunchTask(final PrivateKeyEntry pke, final List<SignOperationConfig> signConfigs) {

    	new SignPanelSignTask(
        		this,
        		signConfigs,
        		pke,
        		this.signWaitDialog,
        		this,
        		this.saf
    		).execute();
    }

    @Override
    public void finishTask() {

    	// Marcamos la tarea como termiada para evitar mostrar el dialogo espera despues de su fin
		this.taskIsRunning = false;
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
    	if (PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_KEYSTORE_SKIP_AUTH_CERT_DNIE)) {
    		filters.add(new SkipAuthDNIeFilter());
    	}
    	if (PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_KEYSTORE_ALIAS_ONLY_CERTS)) {
    		filters.add(new PseudonymFilter(PseudonymFilter.VALUE_ONLY));
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
	public void loadFiles(final File[] files, final SignOperationConfig generalSignConfig) {

     	setCursor(new Cursor(Cursor.WAIT_CURSOR));

     	final File[] dataFiles = filterFiles(files);
     	if (dataFiles == null || dataFiles.length == 0) {
     		LOGGER.warning("No se ha cargado ningun fichero valido"); //$NON-NLS-1$
    		AOUIFactory.showErrorMessage(
    				SimpleAfirmaMessages.getString("SimpleAfirma.12"), //$NON-NLS-1$,
    				SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
    				JOptionPane.ERROR_MESSAGE,
    				null
    			);
     	}
     	else {
     		final List<SignOperationConfig> configs = new ArrayList<>();
     		for (final File dataFile : dataFiles) {
     			try {
     				if (generalSignConfig != null) {
     					configs.add(prepareSignConfig(dataFile, generalSignConfig));
     				}
     				else {
     					configs.add(prepareSignConfig(dataFile));
     				}
     			}
     			catch (final Exception e) {
     				LOGGER.log(Level.WARNING,
     						String.format("Error al cargar el fichero %s, se salta al siguiente", LoggerUtil.getCleanUserHomePath(dataFile.getAbsolutePath())), //$NON-NLS-1$
     						e);
     			}
     		}
     		this.lowerPanel.loadDataInfo(configs);
     		this.lowerPanel.updateSignButtonState(this.saf.isKeyStoreReady());
     		this.lowerPanel.requestFocusInWindow();

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
	 private static File[] filterFiles(final File[] fileList) {

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

	 private static SignOperationConfig prepareSignConfig(final File dataFile) throws IOException {

		 final byte[] data;
		 try (final InputStream fis = new FileInputStream(dataFile)) {
			 data = Files.readAllBytes(dataFile.toPath());
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

	 private static SignOperationConfig prepareSignConfig(final File dataFile, final SignOperationConfig signConfig) throws IOException {

		 final byte[] data;
		 try (final InputStream fis = new FileInputStream(dataFile)) {
			 data = Files.readAllBytes(dataFile.toPath());
		 }
		 catch(final OutOfMemoryError e) {
			 throw new IOException("No hay memoria suficiente para leer el fichero", e); //$NON-NLS-1$
		 }
		 catch(final Exception e) {
			 throw new IOException("No se ha podido leer el fichero", e); //$NON-NLS-1$
		 }

		 final SignOperationConfig config = new SignOperationConfig();

		 config.setDataFile(dataFile);
		 configureDataSigner(config, signConfig, data);

		 return config;
	 }

	 private static void configureDataSigner(final SignOperationConfig config, final byte[] data) throws IOException {

		 // Comprobamos si es un fichero PDF
		 if (DataAnalizerUtil.isPDF(data)) {
			 config.setFileType(FileType.PDF);
			 config.setSigner(AOSignerFactory.getSigner(
					 PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_PDF))
					 );
			 // Se comprueba si ya esta firmada para deshabilitar la opcion de marca visible
			 if (config.getSigner() instanceof AOPDFSigner &&
					 config.getSigner().isSign(data)) {
				 config.setCryptoOperation(CryptoOperation.COSIGN);
			 }
		 }
		 // Comprobamos si es una factura electronica
		 else if (DataAnalizerUtil.isFacturae(data)) {
			 config.setFileType(FileType.FACTURAE);
			 config.setSigner(AOSignerFactory.getSigner(
					 PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_FACTURAE))
					 );
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

		 // Si los datos seleccionados son considerados firma por el firmador con el que se vaya
		 // a utilizar, se validan con el
		 if (config.getSigner().isSign(data, config.getExtraParams())) {
			 setValidationInfo(data, config);
		 }

		 config.setSignatureFormatName(getSignatureName(config.getSigner()));
		 config.setExtraParams(ExtraParamsHelper.loadExtraParamsForSigner(config.getSigner()));
		 config.getExtraParams().put(XAdESExtraParams.CONFIRM_DIFFERENT_PROFILE, Boolean.TRUE);
	 }

	 private static void configureDataSigner(final SignOperationConfig newConfig, final SignOperationConfig config, final byte[] data) throws IOException {

		 String defaultSignFormat;

		 // Comprobamos si es un fichero PDF
		 if (DataAnalizerUtil.isPDF(data)) {
			 newConfig.setFileType(FileType.PDF);
			 defaultSignFormat = PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_PDF);
		 }
		 // Comprobamos si es una factura electronica
		 else if (DataAnalizerUtil.isFacturae(data)) {
			 newConfig.setFileType(FileType.FACTURAE);
			 defaultSignFormat = PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_FACTURAE);
		 }
		 // Comprobamos si es un OOXML
		 else if (DataAnalizerUtil.isOOXML(data)) {
			 newConfig.setFileType(FileType.OOXML);
			 defaultSignFormat = PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_OOXML);
		 }
		 // Comprobamos si es un ODF
		 else if (DataAnalizerUtil.isODF(data)) {
			 newConfig.setFileType(FileType.ODF);
			 defaultSignFormat = PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_ODF);
		 }
		// Comprobamos si es un fichero XML
		 else if (DataAnalizerUtil.isXML(data)) {
			 newConfig.setFileType(FileType.XML);
			 defaultSignFormat = PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_XML);
		 }
		 // Cualquier otro tipo de fichero
		 else {
			 newConfig.setFileType(FileType.BINARY);
			 defaultSignFormat = PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_BIN);
		 }

		 // Si se establecio desde fuera el formato de firma, se utiliza ese. Si no, el por defecto
		 // segun el tipo de archivo
		 if (config.getSigner() != null) {
			 newConfig.setSigner(config.getSigner());
		 }
		 else {
			 newConfig.setSigner(AOSignerFactory.getSigner(defaultSignFormat));
		 }

		 // Si los datos seleccionados son considerados firma por el firmador con el que se vaya
		 // a utilizar, se validan con el
		 if (newConfig.getSigner().isSign(data, config.getExtraParams())) {

			 // En caso de ser firma CAdES o XAdES, reajustamos el tipo
			 if (newConfig.getSigner() instanceof AOCAdESSigner) {
				 newConfig.setFileType(FileType.SIGN_CADES);
			 }
			 else if (newConfig.getSigner() instanceof AOXAdESSigner) {
				 newConfig.setFileType(FileType.SIGN_XADES);
			 }

			 setValidationInfo(data, newConfig);
		 }

		 newConfig.setSignatureFormatName(getSignatureName(newConfig.getSigner()));

		 newConfig.setCryptoOperation(config.getCryptoOperation());
		 newConfig.setDigestAlgorithm(config.getDigestAlgorithm());
		 final Properties extraParams = newConfig.getExtraParams() != null ? (Properties) newConfig.getExtraParams().clone() : new Properties();

		 extraParams.put(XAdESExtraParams.CONFIRM_DIFFERENT_PROFILE, Boolean.TRUE);

		 newConfig.setExtraParams(extraParams);
	 }

	 private static void setValidationInfo(final byte[] data, final SignOperationConfig newConfig) {
		 final SignValider validator = SignValiderFactory.getSignValider(newConfig.getSigner());
		 if (validator != null) {
			 SignValidity validity = null;
			 final Properties validationParams = new Properties();

			 final boolean needCheckPsa = PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_PADES_CHECK_SHADOW_ATTACK);
			 if (!needCheckPsa) {
				 validationParams.put(PdfExtraParams.ALLOW_SHADOW_ATTACK, Boolean.TRUE.toString());
			 }

			 validationParams.put(PdfExtraParams.CHECK_CERTIFICATES, Boolean.TRUE.toString());
			 validationParams.put(PdfExtraParams.PAGES_TO_CHECK_PSA, PdfExtraParams.PAGES_TO_CHECK_PSA_VALUE_ALL);

			 String errorText = null;
			 List<SignValidity> validityList = new ArrayList<>();
			 try {
				validityList = validator.validate(data, validationParams);
				validity = validityList.get(0);
				if (validity.getValidity() == SignValidity.SIGN_DETAIL_TYPE.KO
						|| validity.getValidity() == SignValidity.SIGN_DETAIL_TYPE.UNKNOWN) {
					errorText = buildErrorText(validity.getValidity(), validity.getError());
				}
			} catch (final RuntimeConfigNeededException e) {
				validityList.add(new SignValidity(SIGN_DETAIL_TYPE.PENDING_CONFIRM_BY_USER, VALIDITY_ERROR.SUSPECTED_SIGNATURE , e));
				errorText = e.getMessage();
			} catch (final Exception e) {
				final SignValidity signValidity = new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.CORRUPTED_SIGN, e);
				validityList.add(signValidity);
				errorText = buildErrorText(signValidity.getValidity(), signValidity.getError());
			}

			newConfig.setSignValidity(validityList);
			if (errorText != null) {
				newConfig.setInvalidSignatureText(errorText);
			}
		 }
	}

	private static String buildErrorText(final SIGN_DETAIL_TYPE result, final VALIDITY_ERROR error) {

		 final String errorMsg;
		 if (result == SIGN_DETAIL_TYPE.UNKNOWN) {
			 errorMsg = SimpleAfirmaMessages.getString("SignPanel.141"); //$NON-NLS-1$
		 } else {
			 errorMsg = SimpleAfirmaMessages.getString("SignPanel.140"); //$NON-NLS-1$
		 }
		 switch (error) {
			case NO_DATA:
				return errorMsg + ": " + SimpleAfirmaMessages.getString("SignPanel.125"); //$NON-NLS-1$ //$NON-NLS-2$
			case CORRUPTED_SIGN:
				return errorMsg + ": " + SimpleAfirmaMessages.getString("SignPanel.126"); //$NON-NLS-1$ //$NON-NLS-2$
			case NO_MATCH_DATA:
				return errorMsg + ": " + SimpleAfirmaMessages.getString("SignPanel.127"); //$NON-NLS-1$ //$NON-NLS-2$
			case NO_SIGN:
				return errorMsg + ": " + SimpleAfirmaMessages.getString("SignPanel.128"); //$NON-NLS-1$ //$NON-NLS-2$
			case CERTIFICATE_PROBLEM:
				return errorMsg + ": " + SimpleAfirmaMessages.getString("SignPanel.129"); //$NON-NLS-1$ //$NON-NLS-2$
			case CERTIFICATE_EXPIRED:
				return errorMsg + ": " + SimpleAfirmaMessages.getString("SignPanel.130"); //$NON-NLS-1$ //$NON-NLS-2$
			case CERTIFICATE_NOT_VALID_YET:
				return errorMsg + ": " + SimpleAfirmaMessages.getString("SignPanel.131"); //$NON-NLS-1$ //$NON-NLS-2$
			case ALGORITHM_NOT_SUPPORTED:
				return errorMsg + ": " + SimpleAfirmaMessages.getString("SignPanel.132"); //$NON-NLS-1$ //$NON-NLS-2$
			case CA_NOT_SUPPORTED:
				return errorMsg + ": " + SimpleAfirmaMessages.getString("SignPanel.133"); //$NON-NLS-1$ //$NON-NLS-2$
			case CRL_PROBLEM:
				return errorMsg + ": " + SimpleAfirmaMessages.getString("SignPanel.134"); //$NON-NLS-1$ //$NON-NLS-2$
			case PDF_UNKOWN_VALIDITY:
				return errorMsg + ": " + SimpleAfirmaMessages.getString("SignPanel.135"); //$NON-NLS-1$ //$NON-NLS-2$
			case OOXML_UNKOWN_VALIDITY:
				return errorMsg + ": " + SimpleAfirmaMessages.getString("SignPanel.136"); //$NON-NLS-1$ //$NON-NLS-2$
			case ODF_UNKOWN_VALIDITY:
				return errorMsg + ": " + SimpleAfirmaMessages.getString("SignPanel.137"); //$NON-NLS-1$ //$NON-NLS-2$
			case UNKOWN_ERROR:
				return errorMsg + ": " + SimpleAfirmaMessages.getString("SignPanel.138"); //$NON-NLS-1$ //$NON-NLS-2$
			case UNKOWN_SIGNATURE_FORMAT:
				return errorMsg + ": " + SimpleAfirmaMessages.getString("SignPanel.139"); //$NON-NLS-1$ //$NON-NLS-2$
			case MODIFIED_DOCUMENT:
				return SimpleAfirmaMessages.getString("SignPanel.151"); //$NON-NLS-1$
			case OVERLAPPING_SIGNATURE:
				return SimpleAfirmaMessages.getString("SignPanel.152"); //$NON-NLS-1$
			case CERTIFIED_SIGN_REVISION:
				return errorMsg + ": " + SimpleAfirmaMessages.getString("SignPanel.159"); //$NON-NLS-1$ //$NON-NLS-2$
			default:
				return errorMsg;
		}
	}

	/** Identifica el tipo de firma asociado a un firmador. Si no identifica el firmador,
	  * devuelte el tipo por defecto.
	  * @param signer Firmador.
	  * @return Tipo de firma que se debe ejecutar. */
	 public static String getSignatureName(final AOSigner signer) {
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

    	if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
    		mainPanel.setBackground(LookAndFeelManager.DEFAULT_COLOR);
    		this.pluginButtonsPanel.setBackground(LookAndFeelManager.DEFAULT_COLOR);
    	}

    	return mainPanel;
	}

	@Override
    public void refreshPluginButtonsContainer() {

    	final List<PluginGraphicButton> pluginsButtons = PluginsUiComponentsBuilder.getPluginsButtons(
    			PluginIntegrationWindow.INPUT_DATA);

    	for (final PluginGraphicButton button : pluginsButtons) {
    		button.getGraphicButton().addActionListener(
    				e -> {
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

							new Thread(() -> {
								if (button.getButton().getAction() instanceof DataProcessAction) {
								((DataProcessAction) button.getButton().getAction()).processData(
									inputDatas.toArray(new InputData[inputDatas.size()]),
									SwingUtilities.getWindowAncestor(SignPanel.this));
								}
								else {
									button.getButton().getAction().start(
											SwingUtilities.getWindowAncestor(SignPanel.this));
								}
							}).start();
						}
						catch (final Exception ex) {
							LOGGER.log(Level.SEVERE, "La accion del boton devolvio un error", ex); //$NON-NLS-1$
						}
					});
    	}

    	EventQueue.invokeLater(() -> {
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
		});
    }

	/**
	 * Comprueba si en las configuraciones de firma se encuentra alguna de tipo PDF.
	 * @param signConfigs Lista con las configuraciones de las firmas a realizar.
	 * @return Devuelve {@code true} en caso de que alguna de las configuraciones sea de firma PDF,
	 * {@code false} en caso contrario.
	 */
	private static boolean checkSignPDFOperationSign(final List<SignOperationConfig> signConfigs) {
		for (final SignOperationConfig signConfig : signConfigs) {
			if (signConfig.getSigner() instanceof AOPDFSigner) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Comprueba si hay casos de multifirma y les aplica la configuraci&oacute;n seleccionada en las preferencias.
	 * @param signConfigs Lista con las configuraciones de las firmas a realizar.
	 */
	private static void setMultisignOpFromPreferences(final List<SignOperationConfig> signConfigs) {

		// Establecemos la configuracion para cada tipo de firma
		CryptoOperation cadesCryptoOp = null;
		CryptoOperation xadesCryptoOp = null;
		for (final SignOperationConfig signConfig : signConfigs) {
			if (signConfig.getFileType() == FileType.SIGN_CADES) {
				if (cadesCryptoOp == null) {
					cadesCryptoOp = getCryptoOperation(PreferencesManager.PREFERENCE_CADES_MULTISIGN);
				}
				signConfig.setCryptoOperation(cadesCryptoOp);
			}
			else if (signConfig.getFileType() == FileType.SIGN_XADES) {
				if (xadesCryptoOp == null) {
					xadesCryptoOp = getCryptoOperation(PreferencesManager.PREFERENCE_XADES_MULTISIGN);
				}
				signConfig.setCryptoOperation(xadesCryptoOp);
			}
		}
	}

	/**
	 * Obtiene la operaci&oacute;n de multifirma que se debe realizar.
	 * @param pref Preferencia que indica la operacion a realizar.
	 * @return Operaci&oacute;n criptogr&aacute;fica (cofirma, contrafirma de hoja o contrafirma
	 * de todo el &aacute;rbol).
	 */
	private static CryptoOperation getCryptoOperation(final String pref) {
		final String multiSign = PreferencesManager.get(pref);
		return PreferencesManager.VALUE_MULTISIGN_COSIGN.equals(multiSign)
				? CryptoOperation.COSIGN
				: PreferencesManager.VALUE_MULTISIGN_COUNTERSIGN_LEAFS.equals(multiSign)
					? CryptoOperation.COUNTERSIGN_LEAFS
					: CryptoOperation.COUNTERSIGN_TREE;
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
			    		DesktopUtil.getDefaultDialogsIcon(),
			    		UpperPanel.this
					);
				}
				catch(final AOCancelledOperationException e1) {
					return;
				}

		    	this.loadDataListener.loadFiles(files, null);
			});

	        final JLabel welcomeLabel = new JLabel(SimpleAfirmaMessages.getString("SignPanel.14")); //$NON-NLS-1$
	        welcomeLabel.setFocusable(false);
	        welcomeLabel.setFont(welcomeLabel.getFont().deriveFont(Font.PLAIN, 26));
	        welcomeLabel.setLabelFor(getSelectButton());
	        this.add(welcomeLabel, BorderLayout.PAGE_START);

	        final String intro = SimpleAfirmaMessages.getString("SignPanel.40"); //$NON-NLS-1$

	        final JLabel introText = new JLabel(intro);
	        introText.setLabelFor(getSelectButton());
	        introText.setFocusable(false);
	        getAccessibleContext().setAccessibleDescription(intro);

	        final JPanel introPanel = new JPanel(new BorderLayout());
	        introPanel.add(introText, BorderLayout.PAGE_START);
	        this.add(introPanel, BorderLayout.CENTER);

	        final JPanel selectPanel = new JPanel(new FlowLayout(FlowLayout.LEFT), true);
	        selectPanel.add(getSelectButton());
	        this.add(selectPanel, BorderLayout.PAGE_END);

	        // Configuramos el color
	        if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
	            setBackground(LookAndFeelManager.DEFAULT_COLOR);
	            introPanel.setBackground(LookAndFeelManager.DEFAULT_COLOR);
	            selectPanel.setBackground(LookAndFeelManager.DEFAULT_COLOR);
	            welcomeLabel.setForeground(new Color(3399));
	        }

	    }
	}

	private final class LowerPanel extends JPanel {

	    private static final long serialVersionUID = 533243192995645135L;

	    private final LoadDataFileListener loadDataListener;

	    private JScrollPane filePanel;

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

	        // Identificamos el color de fondo
	        Color bgColor = Color.WHITE;
	        if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST && !Platform.OS.MACOSX.equals(Platform.getOS())) {
	        	bgColor = LookAndFeelManager.DEFAULT_COLOR;
	        }

	        final JPanel panel = new ResizingTextPanel(SimpleAfirmaMessages.getString("SignPanel.41")); //$NON-NLS-1$
	        panel.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("SignPanel.42")); //$NON-NLS-1$
	        panel.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("SignPanel.43")); //$NON-NLS-1$
	        panel.setToolTipText(SimpleAfirmaMessages.getString("SignPanel.42")); //$NON-NLS-1$
	        panel.setFocusable(false);

	       this.dropTarget = new DropTarget(
	        		panel,
	        		DnDConstants.ACTION_COPY,
	        		new DropDataFileListener(this.loadDataListener),
	        		true);

	        this.filePanel = new JScrollPane();
	        this.filePanel.setBackground(bgColor);
	        this.filePanel.getViewport().setBackground(bgColor);

	        final Color lineBorderColor = LookAndFeelManager.WINDOWS_HIGH_CONTRAST ? Color.WHITE : Color.GRAY;
	        this.filePanel.setBorder(BorderFactory.createLineBorder(lineBorderColor));

	        this.filePanel.getHorizontalScrollBar().setUnitIncrement(30);
	        this.filePanel.getVerticalScrollBar().setUnitIncrement(30);


	        // En Apple siempre hay barras, y es el SO el que las pinta o no depende de si hacen falta
	        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
	        	this.filePanel.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
	        	this.filePanel.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
	        }
	        else {
	        	this.filePanel.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
	        	this.filePanel.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
	        }
	        this.filePanel.getVerticalScrollBar().setUnitIncrement(16);
        	this.filePanel.getHorizontalScrollBar().setUnitIncrement(16);

	        //this.filePanel.setDropTarget(this.dropTarget);
	        this.filePanel.getViewport().setDropTarget(this.dropTarget);

	        this.filePanel.setViewportView(panel);

	        this.add(this.filePanel, BorderLayout.CENTER);

	        final JPanel buttonPanel = new JPanel(true);
	        this.signButton.setMargin(new Insets(0, 30, 0, 30));
           	this.signButton.setText(SimpleAfirmaMessages.getString("SignPanel.45")); //$NON-NLS-1$

	        this.signButton.setMnemonic('F');
	        this.signButton.setEnabled(false);
	        buttonPanel.add(this.signButton);
	        this.signButton.addActionListener(
	    		ae -> {
					sign();
				}
			);

	        // Establecemos la configuracion de color
	        if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
	            setBackground(LookAndFeelManager.DEFAULT_COLOR);
	            panel.setBackground(Color.DARK_GRAY);
	            panel.setForeground(Color.LIGHT_GRAY);
	            buttonPanel.setBackground(LookAndFeelManager.DEFAULT_COLOR);
	        }

	        this.add(buttonPanel, BorderLayout.PAGE_END);
	    }

	    public void loadDataInfo(final List<SignOperationConfig> configs) {

	         if (configs.size() == 1) {
	        	 this.filePanel.setViewportView(new SignPanelFilePanel(
	        			 configs.get(0)));
	         }
	         else if (configs.size() > 1) {
	        	 this.filePanel.setViewportView(new SignPanelMultiFilePanel(
	        			 configs));
	         }

	         add(this.filePanel);
	         revalidate();
	    }

	    public void updateSignButtonState(final boolean enable) {

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
	    	return (JPanel) this.filePanel.getViewport().getView();
	    }
	}

     /**
      * Filtro de ficheros que s&oacute;lo admite ficheros (no directorios)
      * con permisos de lectura.
      */
     static class OnlyFileFilter implements FileFilter {
 		@Override
 		public boolean accept(final File pathname) {
 			return pathname.isFile() && pathname.canRead();
 		}

     }
}
