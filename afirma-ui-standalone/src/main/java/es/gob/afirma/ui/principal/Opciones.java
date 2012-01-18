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
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Properties;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JWindow;
import javax.swing.WindowConstants;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.ui.utils.Constants;
import es.gob.afirma.ui.utils.CustomDialog;
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
class Opciones extends JAccessibilityDialog {

	private static final long serialVersionUID = 1L;

	/** &Iacute;ndice de la pesta&ntilde;a de opciones generales. */
    private static final int GENERAL_OPTIONS_IDX = 0;
    
    /** &Iacute;ndice de la pesta&ntilde;a de opciones de accesibilidad. */
    private static final int ACCESIBILITY_OPTIONS_IDX = 2;
    
    /** &Iacute;ndice de la pesta&ntilde;a de opciones de perfiles de usuario. */
    private static final int PROFILE_OPTIONS_IDX = 3;
    
    /** &Iacute;ndice de la pesta&ntilde;a de opciones del formato de firma PDF. */
    private static final int PDF_OPTIONS_IDX = 1;
	
    /** Pantalla principal de la aplicaci&oacute;n. */
    private PrincipalGUI mainGui;
    
    /** Panel con la configuraci&oacute;n general del aplicativo. */
    MainOptionsPane mainOptions;
    
    /** Panel con la configurac&oacute;n de las firmas PDF del aplicativo. */
    ContextOptionsPane contextOptions;
    
    /** Panel con la configurac&oacute;n de las firmas PDF del aplicativo. */
    AccessibilityOptionsPane accessibilityOptions;
    
    /** Panel con las opciones de gestion de perfiles de usuario. */
    ProfilesOptionsPane profilesOptions;
    
    /** Indica si alguna accion del usuario necesita de un refresco de pantalla. */
    private static boolean update = false;
    
    /** Panel con las pesta&ntilde;as de opciones. */
    private JTabbedPane mainPanel;
    
//    /** Bot&oacute;n para maximizar la ventana */
//    private JButton maximizar = new JButton();
//    
//    /** Bot&oacute;n para restaurar la ventana una vez maximizada */
//    private JButton restaurar = new JButton();
    
    private boolean aplicar = false;
    
    private boolean accesibilidad = false;
    
    private JButton aceptar = new JButton();
    
    /**
	 * Panel de botones relacionados con la accesibilidad.
	 */
	private JPanel accessibilityButtonsPanel = null;
	
	/**
	 * Boton de restaurar.
	 */
	JButton restoreButton = null;
	
	/**
	 * Boton de maximizar.
	 */
	JButton maximizeButton = null;
    
    MainOptionsPane getMainOptions(){
    	return this.mainOptions;
    }
    
    ContextOptionsPane getContextOptions(){
    	return this.contextOptions;
    }
    
    void setAplicar(boolean aplicar){
    	this.aplicar = aplicar;
    }
    
    JButton getAceptar(){
    	return this.aceptar;
    }
    
    boolean isAplicar(){
    	return this.aplicar;
    }    
    
 // Panel inferior
    JPanel bottomPanel = new JPanel(new GridBagLayout());
    
    /**
     * Constructor.
     * @param mainGUI ventana padre
     */
    Opciones(PrincipalGUI mainGUI, boolean aplicar, boolean accesibilidad) {
    	super(mainGUI);
    	this.mainGui = mainGUI;
    	this.aplicar = aplicar;
    	this.accesibilidad = accesibilidad;
        initComponents();
    }

	static void setUpdate(boolean update) {
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
    public static int getInitialX() {
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); //329
		return (screenSize.width - 426) / 2 ;
	}
    
    /**
	 * Posici&oacute;n Y inicial de la ventana dependiendo del sistema operativo y de la
	 * resoluci&oacute;n de pantalla.
	 * @return int Posici&oacute;n Y
	 */
	static int getInitialY() {
        Dimension screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
        if (Platform.getOS().equals(Platform.OS.MACOSX)){
        	return (screenSize.height - 485) / 2;
        } 
        return (screenSize.height - 456) / 2;
	}
	
    /**
     * Inicializacion de componentes
     */
    void initComponents() {
    	
    	createAccessibilityButtonsPanel();
    	
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
	    			if (AccessibilityOptionsPane.isContinueBigStyle()){
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
	    				setBounds(Opciones.getInitialX(), Opciones.getInitialY(), Constants.OPTION_FONT_INITIAL_WIDTH_LINUX, Constants.OPTION_FONT_INITIAL_HEIGHT_LINUX);
	    				setMinimumSize(new Dimension(getSize().width, getSize().height));
	    			} else {
	    				setBounds(Opciones.getInitialX(), Opciones.getInitialY(), Constants.OPTION_FONT_INITIAL_WIDTH, Constants.OPTION_FONT_INITIAL_HEIGHT);
	    				setMinimumSize(new Dimension(getSize().width, getSize().height));
	    			}
	    		} else {
	    			setBounds(Opciones.getInitialX(), Opciones.getInitialY(), Constants.OPTION_INITIAL_WIDTH, Constants.OPTION_INITIAL_HEIGHT);
	    			setMinimumSize(new Dimension(getSize().width, getSize().height));
	    		}
    		}
    	}
    	
    	//Se comprueba el estado de los botones de maximizado y restauracion
		if (!Platform.getOS().equals(Platform.OS.LINUX)){
			if (this.getSize().equals(new Dimension(maxWidth,maxHeight))){
				this.maximizeButton.setEnabled (false);
	    		this.restoreButton.setEnabled (true);
			} else {
				this.maximizeButton.setEnabled (true);
	    		this.restoreButton.setEnabled (false);
			}
		} else {
			
			if (this.getSize().equals(new Dimension(maxWidth,maxHeight - Constants.maximizeVerticalMarginLinux))){
				this.maximizeButton.setEnabled (false);
	    		this.restoreButton.setEnabled (true);
			} else {
				this.maximizeButton.setEnabled (true);
	    		this.restoreButton.setEnabled (false);
			}
		}
    	
    	// Configuracion de la ventana
    	setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setTitle(Messages.getString("Opciones.opciones")); // NOI18N //$NON-NLS-1$
        //setResizable(false);
        getContentPane().removeAll();
        getContentPane().setLayout(new GridBagLayout());
        
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 0;
        c.weighty = 0;
        c.insets = new Insets(0, 13, 0, 13);
        c.gridy = 0;
        c.gridx = 0;
        c.gridwidth = 2;
        c.gridheight = 1;
        
        getContentPane().add(this.accessibilityButtonsPanel,c);
        
        c.weightx = 1.0;
        c.weighty = 1.0;
        c.insets = new Insets(0, 13, 0, 13);
        c.gridy = c.gridy + 1;
        c.gridx = 0;
        c.gridwidth = 1;
        c.gridheight = 1;
                
        // Panel superior con las opciones de configuracion
        this.mainPanel = new JTabbedPane();
        
        this.mainOptions =  new MainOptionsPane();
        this.mainPanel.addTab(Messages.getString("Opciones.general"), //$NON-NLS-1$
        		null,
        		this.mainOptions.getConfigurationPanel(),
        		Messages.getString("Opciones.general")); //$NON-NLS-1$
        this.mainOptions.loadConfig(GeneralConfig.getConfig());

        this.contextOptions =  new ContextOptionsPane();
        
        this.mainPanel.addTab (Messages.getString("Opciones.contextoFirma"), //$NON-NLS-1$
        		null,
        		this.contextOptions.getConfigurationPanel(),
        		Messages.getString("Opciones.contexto")); //$NON-NLS-1$
        
        this.contextOptions.loadConfig(GeneralConfig.getConfig());
        
        //Opciones de accesibilidad
        this.accessibilityOptions =  new AccessibilityOptionsPane(this, this.mainGui);
        
        this.mainPanel.addTab(Messages.getString("Opciones.accesibilidad"), //$NON-NLS-1$
        		null,
        		this.accessibilityOptions.getConfigurationPanel(),
        		Messages.getString("Opciones.accesibilidadTip")); //$NON-NLS-1$
        
        this.accessibilityOptions.loadConfig(GeneralConfig.getConfig());

        // Opciones de gestion de perfiles
        this.profilesOptions =  new ProfilesOptionsPane(this);
        
        this.mainPanel.addTab(
                "Perfiles de usuario",
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
        
        //Foco a la pestana seleccionada
        this.mainPanel.addAncestorListener(new RequestFocusListener(false));
        
        if (this.aplicar){
        	this.mainPanel.setSelectedIndex(2);
        	if (!this.accesibilidad){
        		this.accessibilityOptions.getAplicar().addAncestorListener(new RequestFocusListener(false));
        	}
        	HelpUtils.visualize("opciones.accesibilidad"); //$NON-NLS-1$
        	this.aplicar = false;
        	this.accesibilidad = false;
        }

        this.mainPanel.addMouseListener(new MouseListener() {
			
			@Override
			public void mouseReleased(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void mousePressed(final MouseEvent e) {
				// TODO Auto-generated method stub
				int index = ((JTabbedPane)e.getSource()).getSelectedIndex();
				switch (index) {
				case 0:
					HelpUtils.visualize("opciones.configuracion"); //$NON-NLS-1$
					break;
				case 1:
					//HelpUtils.visualize("");
					break;
				case 2:
					HelpUtils.visualize("opciones.accesibilidad"); //$NON-NLS-1$
					break;
				case 3:
					//HelpUtils.visualize("");
					break;
				default:
					break;
				}
			}
			
			@Override
			public void mouseExited(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void mouseEntered(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void mouseClicked(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}
		});
        
        this.mainPanel.addKeyListener(new KeyListener() {
			
			@Override
			public void keyTyped(KeyEvent e) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void keyReleased(KeyEvent e) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void keyPressed(KeyEvent e) {
				// TODO Auto-generated method stub
				if (e.getKeyCode()==KeyEvent.VK_RIGHT){
					int index = ((JTabbedPane)e.getSource()).getSelectedIndex();
					index++;
					if (index==4){
						index=0;
					}
					switch (index) {
					case 0:
						HelpUtils.visualize("opciones.configuracion"); //$NON-NLS-1$
						break;
					case 1:
						//HelpUtils.visualize("");
						break;
					case 2:
						HelpUtils.visualize("opciones.accesibilidad"); //$NON-NLS-1$
						break;
					case 3:
						//HelpUtils.visualize("");
						break;
					default:
						break;
					}
				} else if(e.getKeyCode()==KeyEvent.VK_LEFT){
					int index = ((JTabbedPane)e.getSource()).getSelectedIndex();
					index--;
					if (index==-1){
						index=3;
					}
					switch (index) {
					case 0:
						HelpUtils.visualize("opciones.configuracion"); //$NON-NLS-1$
						break;
					case 1:
						//HelpUtils.visualize("");
						break;
					case 2:
						HelpUtils.visualize("opciones.accesibilidad"); //$NON-NLS-1$
						break;
					case 3:
						//HelpUtils.visualize("");
						break;
					default:
						break;
					}
				}
			}
		});
        c.weighty = 0.1;
        c.fill = GridBagConstraints.BOTH;
        getContentPane().add(this.mainPanel, c);
        
        c.weighty = 0.0;
        c.gridy = c.gridy + 1;
        getContentPane().add(createButtonsPanel(), c);
    }
    
    private Component createButtonsPanel() {
    	
        this.bottomPanel.removeAll();

		JPanel panelAceptar = new JPanel(new GridLayout(1, 1));
		
		// Boton aceptar
        this.aceptar.setText(Messages.getString("PrincipalGUI.aceptar")); // NOI18N //$NON-NLS-1$
        this.aceptar.setMnemonic(KeyEvent.VK_A); //Se asigna un atajo al boton aceptar
        this.getRootPane().setDefaultButton(this.aceptar); //Se asigna el boton por defecto para la ventana
        this.aceptar.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent evt) {
            	
            	Properties config = new Properties();
            	config.putAll(Opciones.this.mainOptions.getConfig());
            	config.putAll(Opciones.this.contextOptions.getConfig());
            	config.putAll(Opciones.this.accessibilityOptions.getConfig());
            	
                aceptarActionPerformed(config, Opciones.this.profilesOptions.getProfiles());
            }
        });
        this.aceptar.getAccessibleContext().setAccessibleDescription(Messages.getString("PrincipalGUI.aceptar")); // NOI18N //$NON-NLS-1$
        Utils.remarcar(this.aceptar);
        Utils.setContrastColor(this.aceptar);
        Utils.setFontBold(this.aceptar);
        panelAceptar.add(this.aceptar);
        
        JPanel panelCancelar = new JPanel(new GridLayout(1, 1));
        
        // Boton cancelar
        JButton	cancelar = new JButton();
        cancelar.setText(Messages.getString("PrincipalGUI.cancelar")); // NOI18N //$NON-NLS-1$
        cancelar.setMnemonic(KeyEvent.VK_C); //Se asigna un atajo al boton cancelar
        cancelar.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent evt) {
                cancelarActionPerformed();
            }
        });
        cancelar.getAccessibleContext().setAccessibleDescription(Messages.getString("PrincipalGUI.cancelar")); // NOI18N //$NON-NLS-1$
        Utils.remarcar(cancelar);
        Utils.setContrastColor(cancelar);
        Utils.setFontBold(cancelar);
        panelCancelar.add(cancelar);
        
        
        // Panel en donde se insertan los botones aceptar y cancelar
        JPanel buttonPanel = new JPanel(new GridBagLayout());
        
        
        GridBagConstraints c = new GridBagConstraints();
        
        c.anchor = GridBagConstraints.CENTER; //control de la orientacion de componentes
        c.insets = new Insets(0, 0, 0, 20);
       
		buttonPanel.add(panelAceptar, c);
		 c.insets = new Insets(0, 0, 0, 0);
		//buttonPanel.add(panelVacio, c);
		buttonPanel.add(panelCancelar,c);
		
        
		JPanel panelAyuda = new JPanel();
        // Boton ayuda
		JButton botonAyuda = HelpUtils.helpButton("opciones.configuracion"); //$NON-NLS-1$
		botonAyuda.setName("helpButton");
		
		// Sustituimos el listener por defecto por otro que abrir la ventana de ayuda
		// correspondiente a la pestana seleccionada
		for (ActionListener listener : botonAyuda.getActionListeners()) {
		    botonAyuda.removeActionListener(listener);
		}
		botonAyuda.addActionListener(new OpenHelpActionListener(this.mainPanel));
		
		GridBagConstraints cons = new GridBagConstraints();
		//cons.anchor = GridBagConstraints.FIRST_LINE_START; //control de la orientacion de componentes
		cons.fill = GridBagConstraints.BOTH;
		//cons.ipadx = 0;
		cons.gridx = 0;
		cons.gridy = 0;
		
		//cons.ipadx = 0;
        cons.weighty = 1.0;
		cons.weightx = 1.0;
		cons.gridx = 0;
		cons.insets = new Insets(0, 40, 0, 0);
		
		this.bottomPanel.add(buttonPanel, cons);

        cons.weightx = 0.0;
        cons.weighty = 1.0;
		cons.gridx = 1;
		cons.insets = new Insets(0, 0, 0, 10);
		
		panelAyuda.add(botonAyuda);        
        this.bottomPanel.add(panelAyuda, cons);
        
        return this.bottomPanel;
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
    	Opciones.removeDeletedProfiles(remainderProfilesNames);
    	
    	// Si se ha cambiado la vista de simple a avanzada o viceversa reconstruimos la interfaz
    	if (needUpdateGUI && this.mainGui != null) {
    		update = false;
    		this.mainGui.crearPaneles();
    		this.mainGui.generarMenuHerramientas();
    		this.mainGui.generarMenuAccesibilidad();
    		this.mainGui.generarMenuAyuda();
    	}
    	
    	// Cerramos la pantalla    	
    	dispose();
    	HelpUtils.visualize(Main.helpIndex);
    }

	/**
     * Cierra la ventana
     */
    void cancelarActionPerformed() {
    	// Guardamos la posicion y tamano actual de la ventana solo en caso de no estar maximizada por configuracion
    	if (!GeneralConfig.isMaximized()){
	    	PrincipalGUI.optionActualPositionX = this.getX();
	    	PrincipalGUI.optionActualPositionY = this.getY();
	    	PrincipalGUI.optionActualWidth = this.getWidth();
	    	PrincipalGUI.optionActualHeight = this.getHeight();
    	}
    	dispose();
    	HelpUtils.visualize(Main.helpIndex);
    }
    
    /**
	 * Cambia el tama&ntilde;o de la ventana al tama&ntilde;o m&aacute;ximo de pantalla menos el tama&ntilde;o de la barra de tareas de windows
	 */
	public void maximizarActionPerformed(){
		setActualPositionX(this.getX());
		setActualPositionY(this.getY());
		setActualWidth(this.getWidth());
		setActualHeight(this.getHeight());
		
		//Se obtienen las dimensiones totales disponibles para mostrar una ventana
		Rectangle rect =  GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();
		
		//Se obtienen las dimensiones de maximizado
		int maxWidth = (int)rect.getWidth();
		int maxHeight = (int)rect.getHeight();
		
//		this.maximizar.setEnabled (false);
//		this.restaurar.setEnabled (true);
		this.maximizeButton.setEnabled (false);
		this.restoreButton.setEnabled (true);
		
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
		
		if (getActualPositionX() != -1 && getActualPositionY() != -1 && getActualWidth() != -1 && getActualHeight() != -1){
			this.setBounds(getActualPositionX(), getActualPositionY(), getActualWidth(), getActualHeight());
		} else {
			if (GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
    			if (Platform.getOS().equals(Platform.OS.LINUX)){
    				setBounds(Opciones.getInitialX(), Opciones.getInitialY(), Constants.OPTION_FONT_INITIAL_WIDTH_LINUX, Constants.OPTION_FONT_INITIAL_HEIGHT_LINUX);
    				setMinimumSize(new Dimension(getSize().width, getSize().height));
    			} else {
    				setBounds(Opciones.getInitialX(), Opciones.getInitialY(), Constants.OPTION_FONT_INITIAL_WIDTH, Constants.OPTION_FONT_INITIAL_HEIGHT);
    				setMinimumSize(new Dimension(getSize().width, getSize().height));
    			}
    		} else {
    			setBounds(Opciones.getInitialX(), Opciones.getInitialY(), Constants.OPTION_INITIAL_WIDTH, Constants.OPTION_INITIAL_HEIGHT);
    			setMinimumSize(new Dimension(getSize().width, getSize().height));
    		}
		}
//		this.maximizar.setEnabled (true);
//		this.restaurar.setEnabled (false);
		this.maximizeButton.setEnabled (true);
		this.restoreButton.setEnabled (false);
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
                HelpUtils.showHelp("opciones.accesibilidad"); //$NON-NLS-1$
                break;
            case PDF_OPTIONS_IDX:
                HelpUtils.showHelp("");
                break;
            case PROFILE_OPTIONS_IDX:
            	HelpUtils.showHelp("opciones.perfil"); //$NON-NLS-1$
            	break;
            case GENERAL_OPTIONS_IDX:
            default:
                HelpUtils.showHelp("opciones.configuracion"); //$NON-NLS-1$
                break;
            }
        }
	}
	
	/**
	 * Elimina de la lista de perfiles dados de alta en la aplicaci&oacute;n, todos aquellos
	 * cuyo nombre no aparezca en este listado.
	 * @param remainderProfiles Nombres de los listados que no deben borrarse.
	 */
	private static void removeDeletedProfiles(String[] remainderProfiles) {
		
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
	
	/**
	 * Se crea el panel de botones de accesibilidad.
	 */
	private void createAccessibilityButtonsPanel() {
		this.accessibilityButtonsPanel = new JPanel(new GridBagLayout());
		
		//Para el tooltip
		final JWindow tip = new JWindow();
		final JLabel tipText = new JLabel();
		
		//Panel que va a contener los botones de accesibilidad
		JPanel panel = new JPanel(new GridBagLayout());
		
		//panel.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
		//panel.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
		//panel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.RAISED));
		//panel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
		//panel.setBorder(BorderFactory.createLineBorder(Color.BLACK));
		//panel.setBorder(BorderFactory.createCompoundBorder());
		//panel.setBorder(BorderFactory.createMatteBorder(1, 1, 1, 1, Color.BLACK));
		
		//Restricciones para los botones
		GridBagConstraints consButtons = new GridBagConstraints();
		consButtons.fill = GridBagConstraints.BOTH;
		consButtons.gridx = 0;
		consButtons.gridy = 0;
		consButtons.weightx = 1.0;
		consButtons.weighty = 1.0;
		consButtons.insets = new Insets(0,0,0,0);  //right padding
		//consButtons.anchor=GridBagConstraints.EAST;
		
		//Restore button
		JPanel restorePanel = new JPanel();
		//this.restoreButton = getButton("r", KeyEvent.VK_R );
		ImageIcon imageIconRestore= new ImageIcon(CustomDialog.class.getResource("/resources/images/restore.png")); //$NON-NLS-1$
		this.restoreButton = new JButton(imageIconRestore);
		this.restoreButton.setMnemonic(KeyEvent.VK_R );
		this.restoreButton.setToolTipText(Messages.getString("Wizard.restaurar.description")); //$NON-NLS-1$
		this.restoreButton.getAccessibleContext().setAccessibleName(this.restoreButton.getToolTipText());
		
		this.restoreButton.addFocusListener(new FocusListener() {
			
			@Override
			public void focusLost(FocusEvent e) {
				Utils.showToolTip(false, tip, Opciones.this.restoreButton, tipText);
			}
			
			@Override
			public void focusGained(FocusEvent e) {
				Utils.showToolTip(true, tip, Opciones.this.restoreButton, tipText);
			}
		});
		Dimension dimension = new Dimension(20,20);
		this.restoreButton.setPreferredSize(dimension);
		
		//this.restoreButton.setBorder(null); //Eliminar Borde, ayuda a centrar el iconod el boton
		//this.restoreButton.setContentAreaFilled(false); //area del boton invisible
		this.restoreButton.setName("restaurar");
		Utils.remarcar(this.restoreButton);
		restorePanel.add(this.restoreButton);
		this.restoreButton.addActionListener(new ActionListener() {
	    	@Override
            public void actionPerformed(ActionEvent e) {
	    		restaurarActionPerformed();
			}
		});
		
		
		panel.add(restorePanel, consButtons);
		
		
		consButtons.gridx = 1;
		//consButtons.weightx = 0.5;
		consButtons.insets = new Insets(0,0,0,0);  //right padding
		
		//Maximize button
		JPanel maximizePanel = new JPanel();

		ImageIcon imageIconMaximize= new ImageIcon(CustomDialog.class.getResource("/resources/images/maximize.png"));
		this.maximizeButton = new JButton(imageIconMaximize);
		this.maximizeButton.setMnemonic(KeyEvent.VK_M );
		this.maximizeButton.setToolTipText(Messages.getString("Wizard.maximizar.description"));
		this.maximizeButton.getAccessibleContext().setAccessibleName(this.maximizeButton.getToolTipText());

		//this.maximizeButton.setBorder(null); //Eliminar Borde, ayuda a centrar el iconod el boton
		//this.maximizeButton.setContentAreaFilled(false); //area del boton invisible
		
		this.maximizeButton.setName("maximizar");
		//Se asigna una dimension por defecto
		this.maximizeButton.setPreferredSize(dimension);
				
		Utils.remarcar(this.maximizeButton);
		//maximizePanel.add(this.maximizeButton, consMaximizePanel);
		maximizePanel.add(this.maximizeButton);	
		
		this.maximizeButton.addFocusListener(new FocusListener() {
			
			@Override
			public void focusLost(FocusEvent e) {
				Utils.showToolTip(false, tip, Opciones.this.maximizeButton, tipText);
			}
			
			@Override
			public void focusGained(FocusEvent e) {
				Utils.showToolTip(true, tip, Opciones.this.maximizeButton, tipText);
			}
		});
		
		this.maximizeButton.addActionListener(new ActionListener() {
		    	@Override
                public void actionPerformed(ActionEvent e) {
		    		maximizarActionPerformed();
				}
			});

		
		panel.add(maximizePanel, consButtons);

		//Se anade al panel general
		//Restricciones para el panel de botones
		GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.NONE;
		c.gridx = 0;
		c.gridy = 0;
		c.weightx = 1.0;
		c.weighty = 1.0;
		//c.insets = new Insets(3,3,0,3);
		c.insets = new Insets(0,0,0,0); 
		c.anchor=GridBagConstraints.EAST;
		this.accessibilityButtonsPanel.add(panel, c);
		
		
		// Habilitado/Deshabilitado de botones restaurar/maximizar
    	if (GeneralConfig.isMaximized()){
    		//Se deshabilita el boton de maximizado
    		this.maximizeButton.setEnabled(false);
    		//Se habilita el boton de restaurar
    		this.restoreButton.setEnabled(true);
    	} else {
    		//Se habilita el boton de maximizado
    		this.maximizeButton.setEnabled(true);
    		//Se deshabilita el boton de restaurar
    		this.restoreButton.setEnabled(false);
    	}
		
	}
}