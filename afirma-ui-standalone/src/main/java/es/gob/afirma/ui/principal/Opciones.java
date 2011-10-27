/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.principal;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GraphicsEnvironment;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.Properties;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.WindowConstants;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.ui.utils.Constants;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.JAccessibilityDialog;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;

/**
 * Clase que muestra el panel de opciones.
 */
public class Opciones extends JAccessibilityDialog {

	private static final long serialVersionUID = 1L;

	/** &Iacute;ndice de la pesta&ntilde;a de opciones generales. */
    private static final int GENERAL_OPTIONS_IDX = 0;
    
    /** &Iacute;ndice de la pesta&ntilde;a de opciones de accesibilidad. */
    private static final int ACCESIBILITY_OPTIONS_IDX = 2;
    
    /** &Iacute;ndice de la pesta&ntilde;a de opciones del formato de firma PDF. */
    private static final int PDF_OPTIONS_IDX = 1;
	
    /** Pantalla principal de la aplicaci&oacute;n. */
    private PrincipalGUI mainGui;
    
    /** Panel con la configuraci&oacute;n general del aplicativo. */
    private MainOptionsPane mainOptions;
    
    /** Panel con la configurac&oacute;n de las firmas PDF del aplicativo. */
    private ContextOptionsPane contextOptions;
    
    /** Panel con la configurac&oacute;n de las firmas PDF del aplicativo. */
    private AccessibilityOptionsPane accessibilityOptions;
    
    /** Indica si alguna accion del usuario necesita de un refresco de pantalla. */
    private static Boolean update = false;
    
    /** Panel con las pesta&ntilde;as de opciones. */
    private JTabbedPane mainPanel;
    
    /** Botón para maximizar la ventana */
    private JButton maximizar = new JButton();
    
    /** Botón para restaurar la ventana una vez maximizada */
    private JButton restaurar = new JButton();
    
    public Opciones(PrincipalGUI mainGUI) {
    	this.mainGui = mainGUI;
        initComponents();
    }

	public static void setUpdate(Boolean update) {
		Opciones.update = update;
	}

	@Override
	public int getMinimumRelation(){
		return 9;
	}
	
    /**
	 * Posición X inicial de la ventana dependiendo de la resolución de pantalla.
	 * @return int Posición X
	 */
    public int getInitialX() {
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); //329
		return (screenSize.width - 426) / 2 ;
	}
    
    /**
	 * Posición Y inicial de la ventana dependiendo del sistema operativo y de la
	 * resolución de pantalla.
	 * @return int Posición Y
	 */
	public int getInitialY() {
        Dimension screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
        if (Platform.getOS().equals(Platform.OS.MACOSX)){
        	return (screenSize.height - 485) / 2;
        } else {
        	return (screenSize.height - 456) / 2;
        }
	}
	
    /**
     * Inicializacion de componentes
     */
    private void initComponents() {
    	// Dimensiones de la ventana en Windows y Linux

    	if (GeneralConfig.isMaximized()){
    		//Se obtienen las dimensiones totales disponibles para mostrar una ventana
    		Rectangle rect =  GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();
 
    		//Se obtienen las dimensiones de maximizado
    		int maxWidth = (int)rect.getWidth();
    		int maxHeight = (int)rect.getHeight();
    		
    		//Se maximiza dependiendo del so
    		if (!Platform.getOS().equals(Platform.OS.LINUX)){
    			this.setBounds(0,0, maxWidth, maxHeight);
    		} else {
    			this.setBounds(0,0, maxWidth, maxHeight- Constants.maximizeVerticalMarginLinux);
    		}
			

			this.maximizar.setEnabled (false);
			this.restaurar.setEnabled (true);

			if (GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
    			if (Platform.getOS().equals(Platform.OS.LINUX)){
    				setMinimumSize(new Dimension(Constants.OPTION_FONT_INITIAL_WIDTH_LINUX, Constants.OPTION_FONT_INITIAL_HEIGHT_LINUX));
    			} else {
    				setMinimumSize(new Dimension(Constants.OPTION_FONT_INITIAL_WIDTH, Constants.OPTION_FONT_INITIAL_HEIGHT));
    			}
    		} else {
    			setMinimumSize(new Dimension(Constants.OPTION_INITIAL_WIDTH, Constants.OPTION_INITIAL_HEIGHT));
    		}
    	} else {
    		
    		this.maximizar.setEnabled (true);
			this.restaurar.setEnabled (false);

    		if (PrincipalGUI.optionActualPositionX != -1){
	    		if (GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
	    			setBounds(PrincipalGUI.optionActualPositionX, PrincipalGUI.optionActualPositionY, PrincipalGUI.optionActualWidth, PrincipalGUI.optionActualHeight);
	    			if (Platform.getOS().equals(Platform.OS.LINUX)){
	    				setMinimumSize(new Dimension(Constants.OPTION_FONT_INITIAL_WIDTH_LINUX, Constants.OPTION_FONT_INITIAL_HEIGHT_LINUX));
	    			} else {
	    				setMinimumSize(new Dimension(Constants.OPTION_FONT_INITIAL_WIDTH, Constants.OPTION_FONT_INITIAL_HEIGHT));
	    			}
	    		} else {
	    			if (AccessibilityOptionsPane.continueBigStyle){
	    				if (Platform.getOS().equals(Platform.OS.LINUX)){
		    				if (PrincipalGUI.optionActualWidth==Constants.OPTION_FONT_INITIAL_WIDTH_LINUX && PrincipalGUI.optionActualHeight==Constants.OPTION_FONT_INITIAL_HEIGHT_LINUX){
		    					setBounds(PrincipalGUI.optionActualPositionX, PrincipalGUI.optionActualPositionY, Constants.OPTION_INITIAL_WIDTH, Constants.OPTION_INITIAL_HEIGHT);
			    			} else {
			    				setBounds(PrincipalGUI.optionActualPositionX, PrincipalGUI.optionActualPositionY, PrincipalGUI.optionActualWidth, PrincipalGUI.optionActualHeight);
			    			}
		    			} else {
		    				if (PrincipalGUI.optionActualWidth==Constants.OPTION_FONT_INITIAL_WIDTH && PrincipalGUI.optionActualHeight==Constants.OPTION_FONT_INITIAL_HEIGHT){
		    					setBounds(PrincipalGUI.optionActualPositionX, PrincipalGUI.optionActualPositionY, Constants.OPTION_INITIAL_WIDTH, Constants.OPTION_INITIAL_HEIGHT);
			    			} else {
			    				setBounds(PrincipalGUI.optionActualPositionX, PrincipalGUI.optionActualPositionY, PrincipalGUI.optionActualWidth, PrincipalGUI.optionActualHeight);
			    			}
		    			}
	    			} else {
	    				setBounds(PrincipalGUI.optionActualPositionX, PrincipalGUI.optionActualPositionY, PrincipalGUI.optionActualWidth, PrincipalGUI.optionActualHeight);
	    			}
	    			setMinimumSize(new Dimension(Constants.OPTION_INITIAL_WIDTH, Constants.OPTION_INITIAL_HEIGHT));
	    		}
    		} else {
	    		if (GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
	    			if (Platform.getOS().equals(Platform.OS.LINUX)){
	    				setBounds(this.getInitialX(), this.getInitialY(), Constants.OPTION_FONT_INITIAL_WIDTH_LINUX, Constants.OPTION_FONT_INITIAL_HEIGHT_LINUX);
	    				setMinimumSize(new Dimension(getSize().width, getSize().height));
	    			} else {
	    				setBounds(this.getInitialX(), this.getInitialY(), Constants.OPTION_FONT_INITIAL_WIDTH, Constants.OPTION_FONT_INITIAL_HEIGHT);
	    				setMinimumSize(new Dimension(getSize().width, getSize().height));
	    			}
	    		} else {
	    			setBounds(this.getInitialX(), this.getInitialY(), Constants.OPTION_INITIAL_WIDTH, Constants.OPTION_INITIAL_HEIGHT);
	    			setMinimumSize(new Dimension(getSize().width, getSize().height));
	    		}
    		}
    	}

    	
    	// Configuracion de la ventana
    	setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setTitle(Messages.getString("Opciones.opciones")); // NOI18N
        setResizable(true);
        getContentPane().setLayout(new GridBagLayout());
        
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.weighty = 1.0;
//        c.insets = new Insets(13, 13, 0, 13);
        c.gridy = 0;
                
        // Panel superior con las opciones de configuracion
        mainPanel = new JTabbedPane();
        
        mainOptions =  new MainOptionsPane();
        mainPanel.addTab(Messages.getString("Opciones.general"),
        		null,
        		mainOptions.getConfigurationPanel(),
        		Messages.getString("Opciones.general"));
        mainOptions.loadConfig(GeneralConfig.getConfig());
        
        contextOptions =  new ContextOptionsPane();
        
        mainPanel.addTab(Messages.getString("Opciones.contextoFirma"),
        		null,
        		contextOptions.getConfigurationPanel(),
        		Messages.getString("Opciones.contexto"));
        
        contextOptions.loadConfig(GeneralConfig.getConfig());
        
        //Opciones de accesibilidad
        accessibilityOptions =  new AccessibilityOptionsPane();
        
        mainPanel.addTab(Messages.getString("Opciones.accesibilidad"),
        		null,
        		accessibilityOptions.getConfigurationPanel(),
        		Messages.getString("Opciones.accesibilidadTip"));
        
        accessibilityOptions.loadConfig(GeneralConfig.getConfig());
        Utils.setContrastColor(mainPanel);
        Utils.setFontBold(mainPanel); //Control letra en negrita
        
        // Definicion de mnemonicos.
        int tabNum = 0;
        mainPanel.setMnemonicAt(tabNum, KeyEvent.VK_G); //atajo para la primera pestaï¿½a
        mainPanel.setMnemonicAt(tabNum+1, KeyEvent.VK_X); //atajo para la segunda pestaï¿½a
        mainPanel.setMnemonicAt(tabNum+2, KeyEvent.VK_S); //atajo para la tercera pestaï¿½a

        c.weighty = 0.1;
        c.fill = GridBagConstraints.BOTH;
        getContentPane().add(mainPanel, c);
        
        c.weighty = 0.0;
        c.gridy = 1;
        getContentPane().add(createButtonsPanel(), c);
    }

    private Component createButtonsPanel() {
    	// Panel inferior
        JPanel bottomPanel = new JPanel(new GridBagLayout());
        
		GridBagConstraints cons = new GridBagConstraints();
		cons.anchor = GridBagConstraints.FIRST_LINE_START; //control de la orientación de componentes al redimensionar
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.ipadx = 0;
		cons.gridx = 0;
		cons.insets = new Insets(11, 0, 13, 0);
		
		// Etiqueta para rellenar a la izquierda
		JLabel label = new JLabel();
		bottomPanel.add(label, cons);
        
		JPanel panelMaximizar = new JPanel(new GridLayout(1, 1));
		//Boton maximizar ventana
		maximizar.setText(Messages.getString("Wizard.maximizar"));
	    maximizar.setName("maximizar");
	    maximizar.setMnemonic(KeyEvent.VK_M);
	    maximizar.addActionListener(new ActionListener() {
	    	public void actionPerformed(ActionEvent e) {
	    		maximizarActionPerformed();
			}
		});
	    Utils.remarcar(maximizar);
        Utils.setContrastColor(maximizar);
	    Utils.setFontBold(maximizar);
	    panelMaximizar.add(maximizar);
		
	    JPanel panelRestaurar = new JPanel(new GridLayout(1, 1));
	    // Boton restaurar
	    restaurar.setText(Messages.getString("Wizard.restaurar"));
	    restaurar.setName("restaurar");
	    restaurar.setMnemonic(KeyEvent.VK_R);
	    restaurar.addActionListener(new ActionListener() {
	    	public void actionPerformed(ActionEvent e) {
	    		restaurarActionPerformed();
			}
		});
	    Utils.remarcar(restaurar);
        Utils.setContrastColor(restaurar);
	    Utils.setFontBold(restaurar);
	    panelRestaurar.add(restaurar);
	    
	    //Espacio entre botones
		JPanel panelVacio = new JPanel();
		panelVacio.setPreferredSize(new Dimension(15, 10));
	    
		JPanel panelAceptar = new JPanel(new GridLayout(1, 1));
		// Boton aceptar
        JButton aceptar = new JButton();
        aceptar.setText(Messages.getString("PrincipalGUI.aceptar")); // NOI18N
        aceptar.setMnemonic(KeyEvent.VK_A); //Se asigna un atajo al botï¿½n aceptar
        aceptar.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
            	
            	Properties config = new Properties();
            	config.putAll(mainOptions.getConfig());
            	config.putAll(contextOptions.getConfig());
            	config.putAll(accessibilityOptions.getConfig());
            	
            	Properties signatureConfig = new Properties();
            	signatureConfig.putAll(mainOptions.getSignatureConfig());
            	signatureConfig.putAll(contextOptions.getSignatureConfig());
            	
                aceptarActionPerformed(config, signatureConfig);
            }
        });
        aceptar.getAccessibleContext().setAccessibleDescription(Messages.getString("PrincipalGUI.aceptar")); // NOI18N
        Utils.remarcar(aceptar);
        Utils.setContrastColor(aceptar);
        Utils.setFontBold(aceptar);
        panelAceptar.add(aceptar);
        
        JPanel panelCancelar = new JPanel(new GridLayout(1, 1));
        // Boton cancelar
        JButton	cancelar = new JButton();
        cancelar.setText(Messages.getString("PrincipalGUI.cancelar")); // NOI18N
        cancelar.setMnemonic(KeyEvent.VK_C); //Se asigna un atajo al botï¿½n cancelar
        cancelar.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                cancelarActionPerformed();
            }
        });
        cancelar.getAccessibleContext().setAccessibleDescription(Messages.getString("PrincipalGUI.cancelar")); // NOI18N
        Utils.remarcar(cancelar);
        Utils.setContrastColor(cancelar);
        Utils.setFontBold(cancelar);
        panelCancelar.add(cancelar);
        
        // Panel en donde se insertan los botones maximizar, aceptar y cancelar
        JPanel buttonPanel = new JPanel();
        buttonPanel.add(panelMaximizar, BorderLayout.CENTER);
        buttonPanel.add(panelRestaurar, BorderLayout.CENTER);
        buttonPanel.add(panelVacio, BorderLayout.CENTER);
		buttonPanel.add(panelAceptar, BorderLayout.CENTER);
		buttonPanel.add(panelCancelar, BorderLayout.CENTER);
		
        cons.ipadx = 0;
		cons.weightx = 1.0;
		cons.gridx = 1;
		
		bottomPanel.add(buttonPanel, cons);
        
		JPanel panelAyuda = new JPanel();
        // Boton ayuda
		JButton botonAyuda = HelpUtils.helpButton("opciones.configuracion");
		botonAyuda.setName("helpButton");
		
		// Sustituimos el listener por defecto por otro que abrir la ventana de ayuda
		// correspondiente a la pestana seleccionada
		for (ActionListener listener : botonAyuda.getActionListeners()) {
		    botonAyuda.removeActionListener(listener);
		}
		botonAyuda.addActionListener(new OpenHelpActionListener(this.mainPanel));
		
        cons.ipadx = 0;
		cons.weightx = 0.02;
		cons.gridx = 2;
		
		panelAyuda.add(botonAyuda);        
        bottomPanel.add(panelAyuda, cons);
        
        return bottomPanel;
    }
    
//    private Component createMainOptionsPanel() {
//    	
//        GridBagConstraints c = new GridBagConstraints();
//        c.fill = GridBagConstraints.HORIZONTAL;
//        c.weightx = 1.0;
//        c.insets = new Insets(13, 13, 0, 13);
//        c.gridy = 0;
//    	
//        JPanel mainPanel = new JPanel(new GridBagLayout());
//        
//    	// Panel general
//    	JPanel generalPanel = new JPanel(new GridBagLayout());
//    	generalPanel.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.general"))); // NOI18N
//
//    	GridBagConstraints c2 = new GridBagConstraints();
//        c2.fill = GridBagConstraints.HORIZONTAL;
//        c2.insets = new Insets(0, 13, 0, 13);
//        c2.weightx = 1.0;
//        c2.gridy = 0;
//        
//        // Checkbox para habilitar las opciones de configuracion avanzada
//        checkHabilitar = new JCheckBox();
//        checkHabilitar.setText(Messages.getString("Opciones.general.habilitar")); // NOI18N
//        checkHabilitar.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.general.habilitar")); // NOI18N
//        checkHabilitar.setSelected(GeneralConfig.isAvanzados()); 
//        checkHabilitar.setBounds(12, 20, 340, 23);
//        generalPanel.add(checkHabilitar, c2);
//        
//        mainPanel.add(generalPanel, c);
//
//        c.gridy = 1;
//        
//        // Panel criptografï¿½a
//        JPanel criptografiaPanel = new JPanel(new GridBagLayout());
//        criptografiaPanel.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.criptografia"))); // NOI18N
//
//        c2.fill = GridBagConstraints.HORIZONTAL;
//        c2.insets = new Insets(4, 13, 0, 13);
//        c2.weightx = 1.0;
//        c2.gridy = 0;
//        
//        // Etiqueta algoritmo de huella digital
//        JLabel etiquetaAlgoritmo = new JLabel(Messages.getString("Opciones.criptografia.algoritmo.parte"));
//        criptografiaPanel.add(etiquetaAlgoritmo, c2);
//        
//        c2.insets = new Insets(5, 13, 0, 13);
//        c2.gridy = 1;
//        
//        // Combo con los algoritmos de huella digital
//        comboAlgoritmo = new JComboBox();
//        comboAlgoritmo.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.criptografia.algoritmo.parte")); // NOI18N
//        comboAlgoritmo.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.criptografia.algoritmo.parte")); // NOI18N
//        comboAlgoritmo.setModel(new DefaultComboBoxModel(Arrays.asList("SHA-1","SHA-512","SHA-384","SHA-256").toArray()));
//        for (int i = 0; i < algoritmoV.size(); i++){
//            if (algoritmoV.get(i).equals(GeneralConfig.getSignConfig().getSignAlgorithm())){
//            	comboAlgoritmo.setSelectedIndex(i);
//            	break;
//            }
//        }
//        criptografiaPanel.add(comboAlgoritmo, c2);
//        
//        c2.insets = new Insets(5, 13, 0, 13);
//        c2.gridy = 2;
//        
//        // Checkbox para utilizar XML
//        checkXML = new JCheckBox();
//        checkXML.setSelected(GeneralConfig.getSignConfig().isUseAlgorithmInternally());
//        checkXML.setText(Messages.getString("Opciones.criptografia.utilizar")); // NOI18N
//        checkXML.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.criptografia.utilizar")); // NOI18N
//        criptografiaPanel.add(checkXML, c2);
//        
//        mainPanel.add(criptografiaPanel, c);
//        
//        c.gridy = 2;
//        
//        // Panel firmas de documentos PDF
//        JPanel panelFirmaPDF = new JPanel(new GridBagLayout());
//        panelFirmaPDF.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.firmas"))); // NOI18N
//        
//        c2.fill = GridBagConstraints.HORIZONTAL;
//        c2.insets = new Insets(4, 13, 0, 13);
//        c2.weightx = 1.0;
//        c2.gridy = 0;
//        
//        // Etiqueta motivo / razon de la firma
//        JLabel etiquetaMotivo = new JLabel();
//        etiquetaMotivo.setText(Messages.getString("Opciones.firmas.motivo")); // NOI18N
//        panelFirmaPDF.add(etiquetaMotivo, c2);
//        
//        c2.insets = new Insets(5, 13, 0, 13);
//        c2.gridy = 1;
//        
//        // Caja de texto para el motivo de la firma
//        campoMotivo = new JTextField();
//        campoMotivo.setText(GeneralConfig.getSignConfig().getSignReason());
//        campoMotivo.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.firmas.motivo")); // NOI18N
//        campoMotivo.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.firmas.motivo")); // NOI18N
//        panelFirmaPDF.add(campoMotivo, c2);
//        
//        c2.insets = new Insets(13, 13, 0, 13);
//        c2.gridy = 2;
//        
//        // Etiqueta lugar donde se realiza la firma
//        JLabel etiquetaLugar = new JLabel();
//        etiquetaLugar.setText(Messages.getString("Opciones.firmas.lugar")); // NOI18N
//        panelFirmaPDF.add(etiquetaLugar, c2);
//        
//        c2.insets = new Insets(5, 13, 0, 13);
//        c2.gridy = 3;
//        
//        // Caja de texto para el lugar donde se realiza la firma
//        campoLugar = new JTextField();
//        campoLugar.setText(GeneralConfig.getSignConfig().getSignatureProductionPlace());
//        campoLugar.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.firmas.lugar")); // NOI18N
//        campoLugar.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.firmas.lugar")); // NOI18N
//        panelFirmaPDF.add(campoLugar, c2);
//        
//        c2.insets = new Insets(13, 13, 0, 13);
//        c2.gridy = 4;
//        
//        // Etiqueta de los datos de contacto
//        JLabel etiquetaDatos = new JLabel();
//        etiquetaDatos.setText(Messages.getString("Opciones.firmas.datos")); // NOI18N
//        panelFirmaPDF.add(etiquetaDatos, c2);
//
//        c2.insets = new Insets(5, 13, 5, 13);
//        c2.gridy = 5;
//        
//        // Caja de texto para los datos de contacto
//        campoDatos = new JTextField();
//        campoDatos.setText(GeneralConfig.getSignConfig().getSignContact());
//        campoDatos.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.firmas.datos")); // NOI18N
//        campoDatos.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.firmas.datos")); // NOI18N
//        panelFirmaPDF.add(campoDatos, c2);
//
//        mainPanel.add(panelFirmaPDF, c);
//        
//		// Accesos rapidos al menu de ayuda
//        HelpUtils.enableHelpKey(checkHabilitar, "opciones.general");
//        HelpUtils.enableHelpKey(comboAlgoritmo, "opciones.algoritmo");
//        HelpUtils.enableHelpKey(checkXML, "opciones.referenciasInternas");
//        HelpUtils.enableHelpKey(campoMotivo, "opciones.pdf.motivo");
//        HelpUtils.enableHelpKey(campoLugar, "opciones.pdf.lugar");
//        HelpUtils.enableHelpKey(campoDatos, "opciones.pdf.datos");
//    	
//    	return mainPanel;
//    }
    
	/**
	 * Cierra la ventana y aplica todas las opciones seleccionadas
	 * @param config Configuraci&oacute;n actualmente establecida en la ventana de opciones.
	 */
    private void aceptarActionPerformed(Properties config, Properties signatureConfig) {

    	// Guardamos la posición y tamaño actual de la ventana sólo en caso de no estar maximizada por configuración
    	if (!GeneralConfig.isMaximized()){
	    	PrincipalGUI.optionActualPositionX = this.getX();
	    	PrincipalGUI.optionActualPositionY = this.getY();
	    	PrincipalGUI.optionActualWidth = this.getWidth();
	    	PrincipalGUI.optionActualHeight = this.getHeight();
    	}
    	
    	// Si se ha cambiado de vista (simple <-> avanzada) o se ha indicado que se desean todas las ventanas maximizadas o se ha indicado que se desean los cursores de texto grandes o se ha indicado que se desea remarcar los elementos con foco o se ha activado la opcion de alto contraste o se ha activado la opcion de tamaño de fuente grande o se ha activado la opcion de fuente en negrita, actualizamos la ventana principal
    	Boolean needUpdateGUI = ((GeneralConfig.isAvanzados() != Boolean.parseBoolean(config.getProperty(MainOptionsPane.MAIN_ADVANCED_VIEW)))|| (GeneralConfig.isMaximized() != Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_WINDOWS_SIZE))) || (GeneralConfig.isBigCaret() != Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_CURSOR_SIZE)))|| (GeneralConfig.isRemarked() != Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_FOCUS_VISIBLE)))|| (GeneralConfig.isHighContrast() != Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_HIGHT_CONTRAST)))|| (GeneralConfig.isBigFontSize() != Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_FONT_SIZE)))|| (GeneralConfig.isFontBold() != Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_FONT_STYLE)))|| update);
    	    	
    	// Guardamos el estado actual de la configuracion de la herramienta
    	GeneralConfig.loadConfig(config);
    	
    	// Guardamos la configuracion de firma establecida
    	GeneralConfig.loadSignatureConfig(signatureConfig);
    	
    	// Si se ha cambiado la vista de simple a avanzada o viceversa reconstruimos la interfaz
    	if (needUpdateGUI && mainGui != null) {
    		update = false;
    		mainGui.crearPaneles();
    		mainGui.generarMenuHerramientas();
    		mainGui.generarMenuAyuda();
    	}
    	
    	// Cerramos la pantalla    	
    	dispose();
    }

	/**
     * Cierra la ventana
     */
    private void cancelarActionPerformed() {
    	// Guardamos la posición y tamaño actual de la ventana sólo en caso de no estar maximizada por configuración
    	if (!GeneralConfig.isMaximized()){
	    	PrincipalGUI.optionActualPositionX = this.getX();
	    	PrincipalGUI.optionActualPositionY = this.getY();
	    	PrincipalGUI.optionActualWidth = this.getWidth();
	    	PrincipalGUI.optionActualHeight = this.getHeight();
    	}
    	dispose();
    }
    
    /**
	 * Cambia el tamaño de la ventana al tamaño máximo de pantalla menos el tamaño de la barra de tareas de windows
	 */
	public void maximizarActionPerformed(){
		actualPositionX = this.getX();
		actualPositionY = this.getY();
		actualWidth = this.getWidth();
		actualHeight = this.getHeight();
		
		//Se obtienen las dimensiones totales disponibles para mostrar una ventana
		Rectangle rect =  GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();
		
		//Se obtienen las dimensiones de maximizado
		int maxWidth = (int)rect.getWidth();
		int maxHeight = (int)rect.getHeight();
		
		this.maximizar.setEnabled (false);
		this.restaurar.setEnabled (true);
		
		//Se hace el resize dependiendo del so
		if (!Platform.getOS().equals(Platform.OS.LINUX)){
			this.setBounds(0,0, maxWidth, maxHeight);
		} else {
			this.setBounds(0,0, maxWidth, maxHeight- Constants.maximizeVerticalMarginLinux);
		}
	}
	
	/**
	 * Restaura el tamaño de la ventana a la posicion anterior al maximizado
	 */
	public void restaurarActionPerformed(){
		if (actualPositionX != -1 && actualPositionY != -1 && actualWidth != -1 && actualHeight != -1){
			this.setBounds(actualPositionX, actualPositionY, actualWidth, actualHeight);
		} else {
			if (GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
    			if (Platform.getOS().equals(Platform.OS.LINUX)){
    				setBounds(this.getInitialX(), this.getInitialY(), Constants.OPTION_FONT_INITIAL_WIDTH_LINUX, Constants.OPTION_FONT_INITIAL_HEIGHT_LINUX);
    				setMinimumSize(new Dimension(getSize().width, getSize().height));
    			} else {
    				setBounds(this.getInitialX(), this.getInitialY(), Constants.OPTION_FONT_INITIAL_WIDTH, Constants.OPTION_FONT_INITIAL_HEIGHT);
    				setMinimumSize(new Dimension(getSize().width, getSize().height));
    			}
    		} else {
    			setBounds(this.getInitialX(), this.getInitialY(), Constants.OPTION_INITIAL_WIDTH, Constants.OPTION_INITIAL_HEIGHT);
    			setMinimumSize(new Dimension(getSize().width, getSize().height));
    		}
		}
		this.maximizar.setEnabled (true);
		this.restaurar.setEnabled (false);
	}
	
	private class OpenHelpActionListener implements ActionListener {

	    private JTabbedPane tabbedPane;
	    
	    public OpenHelpActionListener(JTabbedPane tabbedpane) {
	        this.tabbedPane = tabbedpane;
        }
	    
        @Override
        public void actionPerformed(ActionEvent e) {
            
            switch (this.tabbedPane.getSelectedIndex()) {
            case ACCESIBILITY_OPTIONS_IDX:
                HelpUtils.showHelp("opciones.accesibilidad");
                break;
            case PDF_OPTIONS_IDX:
                HelpUtils.showHelp("");
                break;
            case GENERAL_OPTIONS_IDX:
            default:
                HelpUtils.showHelp("opciones.configuracion");
                break;
            }
        }
	    
	}
}