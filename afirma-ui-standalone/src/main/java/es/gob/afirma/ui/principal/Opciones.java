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
import es.gob.afirma.ui.utils.ProfileManager;
import es.gob.afirma.ui.utils.RequestFocusListener;
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
    
    /** Panel con las opciones de gestion de perfiles de usuario. */
    private ProfilesOptionsPane profilesOptions;
    
    /** Indica si alguna accion del usuario necesita de un refresco de pantalla. */
    private static boolean update = false;
    
    /** Panel con las pesta&ntilde;as de opciones. */
    private JTabbedPane mainPanel;
    
    /** Bot&oacute;n para maximizar la ventana */
    private JButton maximizar = new JButton();
    
    /** Bot&oacute;n para restaurar la ventana una vez maximizada */
    private JButton restaurar = new JButton();
    
    private boolean aplicar = false;
    
    private JButton aceptar = new JButton();
    
    public MainOptionsPane getMainOptions(){
    	return this.mainOptions;
    }
    
    public ContextOptionsPane getContextOptions(){
    	return this.contextOptions;
    }
    
    public void setAplicar(boolean aplicar){
    	this.aplicar = aplicar;
    }
    
    public JButton getAceptar(){
    	return this.aceptar;
    }
    
    public boolean isAplicar(){
    	return this.aplicar;
    }    
    
 // Panel inferior
    JPanel bottomPanel = new JPanel(new GridBagLayout());
    
    /**
     * Constructor.
     * @param mainGUI ventana padre
     */
    public Opciones(PrincipalGUI mainGUI, boolean aplicar) {
    	super(mainGUI);
    	this.mainGui = mainGUI;
    	this.aplicar = aplicar;
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
	 * Posici&oacute;n X inicial de la ventana dependiendo de la resoluci&oacute;n de pantalla.
	 * @return int Posici&oacute;n X
	 */
    public int getInitialX() {
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); //329
		return (screenSize.width - 426) / 2 ;
	}
    
    /**
	 * Posici&oacute;n Y inicial de la ventana dependiendo del sistema operativo y de la
	 * resoluci&oacute;n de pantalla.
	 * @return int Posici&oacute;n Y
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
    public void initComponents() {
    	
    	//Se obtienen las dimensiones totales disponibles para mostrar una ventana
		Rectangle rect =  GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();

		//Se obtienen las dimensiones de maximizado
		int maxWidth = (int)rect.getWidth();
		int maxHeight = (int)rect.getHeight();
		
		// Dimensiones de la ventana en Windows y Linux
    	if (GeneralConfig.isMaximized()){
    		//Se maximiza dependiendo del so
    		if (!Platform.getOS().equals(Platform.OS.LINUX)){
    			this.setBounds(0,0, maxWidth, maxHeight);
    		} else {
    			this.setBounds(0,0, maxWidth, maxHeight- Constants.maximizeVerticalMarginLinux);
    		}
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
		    					setMinimumSize(new Dimension(Constants.OPTION_INITIAL_WIDTH, Constants.OPTION_INITIAL_HEIGHT));
		    					setBounds(PrincipalGUI.optionActualPositionX, PrincipalGUI.optionActualPositionY, Constants.OPTION_INITIAL_WIDTH, Constants.OPTION_INITIAL_HEIGHT);
			    			} else {
			    				setBounds(PrincipalGUI.optionActualPositionX, PrincipalGUI.optionActualPositionY, PrincipalGUI.optionActualWidth, PrincipalGUI.optionActualHeight);
			    			}
		    			} else {
		    				if (PrincipalGUI.optionActualWidth==Constants.OPTION_FONT_INITIAL_WIDTH && PrincipalGUI.optionActualHeight==Constants.OPTION_FONT_INITIAL_HEIGHT){
		    					setMinimumSize(new Dimension(Constants.OPTION_INITIAL_WIDTH, Constants.OPTION_INITIAL_HEIGHT));
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
    	
    	//Se comprueba el estado de los botones de maximizado y restauracion
		if (!Platform.getOS().equals(Platform.OS.LINUX)){
			if (this.getSize().equals(new Dimension(maxWidth,maxHeight))){
				this.maximizar.setEnabled (false);
	    		this.restaurar.setEnabled (true);
			} else {
				this.maximizar.setEnabled (true);
	    		this.restaurar.setEnabled (false);
			}
		} else {
			
			if (this.getSize().equals(new Dimension(maxWidth,maxHeight - Constants.maximizeVerticalMarginLinux))){
				this.maximizar.setEnabled (false);
	    		this.restaurar.setEnabled (true);
			} else {
				this.maximizar.setEnabled (true);
	    		this.restaurar.setEnabled (false);
			}
		}
    	
    	// Configuracion de la ventana
    	setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setTitle(Messages.getString("Opciones.opciones")); // NOI18N
        //setResizable(false);
        getContentPane().removeAll();
        getContentPane().setLayout(new GridBagLayout());
        
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.weighty = 1.0;
//        c.insets = new Insets(13, 13, 0, 13);
        c.gridy = 0;
        c.gridx = 0;
                
        // Panel superior con las opciones de configuracion
        this.mainPanel = new JTabbedPane();
        
        this.mainOptions =  new MainOptionsPane();
        this.mainPanel.addTab(Messages.getString("Opciones.general"),
        		null,
        		this.mainOptions.getConfigurationPanel(),
        		Messages.getString("Opciones.general"));
        this.mainOptions.loadConfig(GeneralConfig.getConfig());
        
        this.contextOptions =  new ContextOptionsPane();
        
        this.mainPanel.addTab(Messages.getString("Opciones.contextoFirma"),
        		null,
        		this.contextOptions.getConfigurationPanel(),
        		Messages.getString("Opciones.contexto"));
        
        this.contextOptions.loadConfig(GeneralConfig.getConfig());
        
        //Opciones de accesibilidad
        this.accessibilityOptions =  new AccessibilityOptionsPane(this, mainGui);
        
        this.mainPanel.addTab(Messages.getString("Opciones.accesibilidad"),
        		null,
        		this.accessibilityOptions.getConfigurationPanel(),
        		Messages.getString("Opciones.accesibilidadTip"));
        
        this.accessibilityOptions.loadConfig(GeneralConfig.getConfig());

        // Opciones de gestion de perfiles
        this.profilesOptions =  new ProfilesOptionsPane(this);
        
        this.mainPanel.addTab("Perfiles de usuario",
        		null,
        		this.profilesOptions.getConfigurationPanel(),
        		"Gesti\u00F3n de los perfiles de usuario");
        
        Utils.setContrastColor(this.mainPanel);
        Utils.setFontBold(this.mainPanel); //Control letra en negrita
        
        // Definicion de mnemonicos.
        int tabNum = 0;
        this.mainPanel.setMnemonicAt(tabNum, KeyEvent.VK_G); //atajo para la primera pestana
        this.mainPanel.setMnemonicAt(tabNum+1, KeyEvent.VK_X); //atajo para la segunda pestana
        this.mainPanel.setMnemonicAt(tabNum+2, KeyEvent.VK_S); //atajo para la tercera pestana
        this.mainPanel.setMnemonicAt(tabNum+3, KeyEvent.VK_P); //atajo para la cuarta pestana
        
        if (aplicar){
        	this.mainPanel.setSelectedIndex(2);
        	this.accessibilityOptions.aplicar.addAncestorListener(new RequestFocusListener(false));        	
        	aplicar = false;
        }

        c.weighty = 0.1;
        c.fill = GridBagConstraints.BOTH;
        getContentPane().add(this.mainPanel, c);
        
        c.weighty = 0.0;
        c.gridy = 1;
        getContentPane().add(createButtonsPanel(), c);
    }

    private Component createButtonsPanel() {
    	
        bottomPanel.removeAll();
		GridBagConstraints cons = new GridBagConstraints();
		cons.anchor = GridBagConstraints.FIRST_LINE_START; //control de la orientacion de componentes al redimensionar
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.ipadx = 0;
		cons.gridx = 0;
		cons.gridy = 0;
		cons.insets = new Insets(11, 0, 13, 0);
		
		// Etiqueta para rellenar a la izquierda
		JLabel label = new JLabel();
		bottomPanel.add(label, cons);
        
		JPanel panelMaximizar = new JPanel(new GridLayout(1, 1));
		//Boton maximizar ventana
		this.maximizar.setText(Messages.getString("Wizard.maximizar"));
	    this.maximizar.setName("maximizar");
	    this.maximizar.getAccessibleContext().setAccessibleName(Messages.getString("Wizard.maximizar") + ". " + Messages.getString("Wizard.maximizar.description"));
	    this.maximizar.setMnemonic(KeyEvent.VK_M);
	    this.maximizar.addActionListener(new ActionListener() {
	    	public void actionPerformed(ActionEvent e) {
	    		maximizarActionPerformed();
			}
		});
	    Utils.remarcar(this.maximizar);
        Utils.setContrastColor(this.maximizar);
	    Utils.setFontBold(this.maximizar);
	    panelMaximizar.add(this.maximizar);
		
	    JPanel panelRestaurar = new JPanel(new GridLayout(1, 1));
	    // Boton restaurar
	    this.restaurar.setText(Messages.getString("Wizard.restaurar"));
	    this.restaurar.setName("restaurar");
	    this.restaurar.getAccessibleContext().setAccessibleName(Messages.getString("Wizard.restaurar") + ". " + Messages.getString("Wizard.restaurar.description"));
	    this.restaurar.setMnemonic(KeyEvent.VK_R);
	    this.restaurar.addActionListener(new ActionListener() {
	    	public void actionPerformed(ActionEvent e) {
	    		restaurarActionPerformed();
			}
		});
	    Utils.remarcar(this.restaurar);
        Utils.setContrastColor(this.restaurar);
	    Utils.setFontBold(this.restaurar);
	    panelRestaurar.add(this.restaurar);
	    
	    //Espacio entre botones
		JPanel panelVacio = new JPanel();
		panelVacio.setPreferredSize(new Dimension(15, 10));
	    
		JPanel panelAceptar = new JPanel(new GridLayout(1, 1));
		// Boton aceptar
        
        aceptar.setText(Messages.getString("PrincipalGUI.aceptar")); // NOI18N
        aceptar.setMnemonic(KeyEvent.VK_A); //Se asigna un atajo al boton aceptar
        this.getRootPane().setDefaultButton(aceptar); //Se asigna el botón por defecto para la ventana
        aceptar.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
            	
            	Properties config = new Properties();
            	config.putAll(Opciones.this.mainOptions.getConfig());
            	config.putAll(Opciones.this.contextOptions.getConfig());
            	config.putAll(Opciones.this.accessibilityOptions.getConfig());
            	
                aceptarActionPerformed(config, Opciones.this.profilesOptions.getProfiles());
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
        cancelar.setMnemonic(KeyEvent.VK_C); //Se asigna un atajo al boton cancelar
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
        cons.weightx = 0.5;
		cons.gridx = 2;
		
		panelAyuda.add(botonAyuda);        
        bottomPanel.add(panelAyuda, cons);
        
        return bottomPanel;
    }
    
	/**
	 * Cierra la ventana y aplica todas las opciones seleccionadas
	 * @param config Configuraci&oacute;n actualmente establecida en la ventana de opciones.
	 * @param remainderProfilesNames Listado de nombres que deben permanecer registrados. 
	 */
    public void aceptarActionPerformed(Properties config, String[] remainderProfilesNames) {

    	// Guardamos la posicion y tamano actual de la ventana solo en caso de no estar maximizada por configuracion
    	if (!GeneralConfig.isMaximized()){
	    	PrincipalGUI.optionActualPositionX = this.getX();
	    	PrincipalGUI.optionActualPositionY = this.getY();
	    	PrincipalGUI.optionActualWidth = this.getWidth();
	    	PrincipalGUI.optionActualHeight = this.getHeight();
    	}
    	
    	// Si se ha cambiado de vista (simple <-> avanzada) o se ha indicado que se desean todas las ventanas maximizadas o se ha indicado que se desean los cursores de texto grandes o se ha indicado que se desea remarcar los elementos con foco o se ha activado la opcion de alto contraste o se ha activado la opcion de tamano de fuente grande o se ha activado la opcion de fuente en negrita, actualizamos la ventana principal
    	boolean needUpdateGUI = (
    			(GeneralConfig.isAvanzados() != Boolean.parseBoolean(config.getProperty(MainOptionsPane.MAIN_ADVANCED_VIEW))) ||
    			(GeneralConfig.isMaximized() != Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_WINDOWS_SIZE))) ||
    			(GeneralConfig.isBigCaret() != Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_CURSOR_SIZE))) ||
    			(GeneralConfig.isRemarked() != Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_FOCUS_VISIBLE))) ||
    			(GeneralConfig.isHighContrast() != Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_HIGHT_CONTRAST))) ||
    			(GeneralConfig.isBigFontSize() != Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_FONT_SIZE))) ||
    			(GeneralConfig.isFontBold() != Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_FONT_STYLE))) ||
    			update);
    	    	
    	// Guardamos el estado actual de la configuracion de la herramienta
    	GeneralConfig.loadConfig(config);
    	
    	// Eliminamos los perfiles que el usuario haya borrado de la lista
    	this.removeDeletedProfiles(remainderProfilesNames);
    	
    	// Si se ha cambiado la vista de simple a avanzada o viceversa reconstruimos la interfaz
    	if (needUpdateGUI && this.mainGui != null) {
    		update = false;
    		this.mainGui.crearPaneles();
    		this.mainGui.generarMenuHerramientas();
    		this.mainGui.generarMenuAyuda();
    	}
    	
    	// Cerramos la pantalla    	
    	dispose();
    }

	/**
     * Cierra la ventana
     */
    private void cancelarActionPerformed() {
    	// Guardamos la posicion y tamano actual de la ventana solo en caso de no estar maximizada por configuracion
    	if (!GeneralConfig.isMaximized()){
	    	PrincipalGUI.optionActualPositionX = this.getX();
	    	PrincipalGUI.optionActualPositionY = this.getY();
	    	PrincipalGUI.optionActualWidth = this.getWidth();
	    	PrincipalGUI.optionActualHeight = this.getHeight();
    	}
    	dispose();
    }
    
    /**
	 * Cambia el tama&ntilde;o de la ventana al tama&ntilde;o m&aacute;ximo de pantalla menos el tama&ntilde;o de la barra de tareas de windows
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
			this.setBounds(0,0, maxWidth, maxHeight - Constants.maximizeVerticalMarginLinux);
		}
	}
	
	/**
	 * Restaura el tama&ntilde;o de la ventana a la posicion anterior al maximizado
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
	
	/**
	 * Elimina de la lista de perfiles dados de alta en la aplicaci&oacute;n, todos aquellos
	 * cuyo nombre no aparezca en este listado.
	 * @param remainderProfiles Nombres de los listados que no deben borrarse.
	 */
	private void removeDeletedProfiles(String[] remainderProfiles) {
		
		boolean remain;
		for (String name : ProfileManager.getProfilesNames()) {
			remain = false;
			for (String remainderProfileName : remainderProfiles) {
				if (name.equals(remainderProfileName)) {
					remain = true;
					break;
				}
			}
			if (!remain) {
				ProfileManager.removeConfiguration(
						ProfileManager.getProfileIdByName(name));
			}
		}
	}
	
	/**
	 * Recupera la configuraci&oacute;n global establecida en los paneles de opciones.
	 * @return Propiedades de configuraci&oacute;n.
	 */
	public Properties getConfiguration() {
		Properties config = new Properties();
		config.putAll(this.mainOptions.getConfig());
		config.putAll(this.contextOptions.getConfig());
		config.putAll(this.accessibilityOptions.getConfig());
		
		return config;
	}
	
	public Properties getSignConfig() {
		Properties config = new Properties();
		config.putAll(this.mainOptions.getSignatureConfig());
		config.putAll(this.contextOptions.getSignatureConfig());
		
		return config;
	}
}