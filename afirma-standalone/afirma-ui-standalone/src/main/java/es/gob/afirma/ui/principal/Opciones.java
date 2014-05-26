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
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
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
final class Opciones extends JAccessibilityDialog {

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
    private final PrincipalGUI mainGui;

    /** Panel con la configuraci&oacute;n general del aplicativo. */
    private MainOptionsPane mainOptions;
    MainOptionsPane getMainOptionsPane() {
    	return this.mainOptions;
    }

    /** Panel con la configurac&oacute;n de las firmas PDF del aplicativo. */
    private ContextOptionsPane contextOptions;

    /** Panel con la configurac&oacute;n de las firmas PDF del aplicativo. */
    private AccessibilityOptionsPane accessibilityOptions;
    AccessibilityOptionsPane getAccessibilityOptions() {
    	return this.accessibilityOptions;
    }

    /** Panel con las opciones de gestion de perfiles de usuario. */
    private ProfilesOptionsPane profilesOptions;
    ProfilesOptionsPane getProfilesOptions() {
    	return this.profilesOptions;
    }

    /** Indica si alguna accion del usuario necesita de un refresco de pantalla. */
    private static boolean update = false;

    /** Panel con las pesta&ntilde;as de opciones. */
    private JTabbedPane mainPanel;

    private boolean aplicar = false;

    private boolean accesibilidad = false;

    private final JButton aceptar = new JButton();

    /**
	 * Panel de botones relacionados con la accesibilidad.
	 */
	private JPanel accessibilityButtonsPanel = null;

	/**
	 * Boton de restaurar.
	 */
	private JButton restoreButton = null;
	JButton getRestoreButton() {
		return this.restoreButton;
	}

	/**
	 * Boton de maximizar.
	 */
	private JButton maximizeButton = null;
	JButton getMaximizeButton() {
		return this.maximizeButton;
	}

    MainOptionsPane getMainOptions(){
    	return this.mainOptions;
    }

    ContextOptionsPane getContextOptions() {
    	return this.contextOptions;
    }

    void setAplicar(final boolean aplicar) {
    	this.aplicar = aplicar;
    }

    JButton getAceptar(){
    	return this.aceptar;
    }

    boolean isAplicar(){
    	return this.aplicar;
    }

 // Panel inferior
    private final JPanel bottomPanel = new JPanel(new GridBagLayout());

    /**
     * Constructor.
     * @param mainGUI ventana padre
     */
    Opciones(final PrincipalGUI mainGUI, final boolean aplicar, final boolean accesibilidad) {
    	super(mainGUI);
    	this.mainGui = mainGUI;
    	this.aplicar = aplicar;
    	this.accesibilidad = accesibilidad;
        initComponents();
    }

	static void setUpdate(final boolean update) {
		Opciones.update = update;
	}


	/** {@inheritDoc} */
	@Override
	public int getMinimumRelation(){
		return 9;
	}

    /** Posici&oacute;n X inicial de la ventana dependiendo de la resoluci&oacute;n de pantalla.
	 * @return int Posici&oacute;n X */
    private static int getInitialX() {
		final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); //329
		return (screenSize.width - 426) / 2 ;
	}

    /**
	 * Posici&oacute;n Y inicial de la ventana dependiendo del sistema operativo y de la
	 * resoluci&oacute;n de pantalla.
	 * @return int Posici&oacute;n Y
	 */
	private static int getInitialY() {
        final Dimension screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
        if (Platform.getOS().equals(Platform.OS.MACOSX)){
        	return (screenSize.height - 485) / 2;
        }
        return (screenSize.height - 456) / 2;
	}

    /** Inicializaci&oacute;n de componentes. */
    void initComponents() {

    	createAccessibilityButtonsPanel();

    	//Se obtienen las dimensiones totales disponibles para mostrar una ventana
		final Rectangle rect =  GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();

		//Se obtienen las dimensiones de maximizado
		final int maxWidth = (int)rect.getWidth();
		final int maxHeight = (int)rect.getHeight();

		// Dimensiones de la ventana en Windows y Linux
    	if (GeneralConfig.isMaximized()) {
    		//Se maximiza dependiendo del so
    		if (!Platform.getOS().equals(Platform.OS.LINUX)){
    			this.setBounds(0,0, maxWidth, maxHeight);
    		}
    		else {
    			this.setBounds(0,0, maxWidth, maxHeight- Constants.MAXIMIZE_VERTICAL_MARGIN_LINUX);
    		}
			if (GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
    			if (Platform.getOS().equals(Platform.OS.LINUX)){
    				setMinimumSize(new Dimension(Constants.OPTION_FONT_INITIAL_WIDTH_LINUX, Constants.OPTION_FONT_INITIAL_HEIGHT_LINUX));
    			}
    			else {
    				setMinimumSize(new Dimension(Constants.OPTION_FONT_INITIAL_WIDTH, Constants.OPTION_FONT_INITIAL_HEIGHT));
    			}
    		}
			else {
    			setMinimumSize(new Dimension(Constants.OPTION_INITIAL_WIDTH, Constants.OPTION_INITIAL_HEIGHT));
    		}
    	}
    	else {
    		if (PrincipalGUI.getOptionActualPositionX() != -1){
	    		if (GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
	    			setBounds(
    			          PrincipalGUI.getOptionActualPositionX(),
    			          PrincipalGUI.getOptionActualPositionY(),
    			          PrincipalGUI.getOptionActualWidth(),
    			          PrincipalGUI.getOptionActualHeight()
			        );
	    			if (Platform.getOS().equals(Platform.OS.LINUX)){
	    				setMinimumSize(new Dimension(Constants.OPTION_FONT_INITIAL_WIDTH_LINUX, Constants.OPTION_FONT_INITIAL_HEIGHT_LINUX));
	    			}
	    			else {
	    				setMinimumSize(new Dimension(Constants.OPTION_FONT_INITIAL_WIDTH, Constants.OPTION_FONT_INITIAL_HEIGHT));
	    			}
	    		}
	    		else {
	    			if (AccessibilityOptionsPane.isContinueBigStyle()){
	    				if (Platform.getOS().equals(Platform.OS.LINUX)){
		    				if (PrincipalGUI.getOptionActualWidth()==Constants.OPTION_FONT_INITIAL_WIDTH_LINUX && PrincipalGUI.getOptionActualHeight()==Constants.OPTION_FONT_INITIAL_HEIGHT_LINUX){
		    					setMinimumSize(new Dimension(Constants.OPTION_INITIAL_WIDTH, Constants.OPTION_INITIAL_HEIGHT));
		    					setBounds(PrincipalGUI.getOptionActualPositionX(), PrincipalGUI.getOptionActualPositionY(), Constants.OPTION_INITIAL_WIDTH, Constants.OPTION_INITIAL_HEIGHT);
			    			}
		    				else {
			    				setBounds(PrincipalGUI.getOptionActualPositionX(), PrincipalGUI.getOptionActualPositionY(), PrincipalGUI.getOptionActualWidth(), PrincipalGUI.getOptionActualHeight());
			    			}
		    			}
	    				else {
		    				if (PrincipalGUI.getOptionActualWidth()==Constants.OPTION_FONT_INITIAL_WIDTH && PrincipalGUI.getOptionActualHeight()==Constants.OPTION_FONT_INITIAL_HEIGHT){
		    					setMinimumSize(new Dimension(Constants.OPTION_INITIAL_WIDTH, Constants.OPTION_INITIAL_HEIGHT));
		    					setBounds(PrincipalGUI.getOptionActualPositionX(), PrincipalGUI.getOptionActualPositionY(), Constants.OPTION_INITIAL_WIDTH, Constants.OPTION_INITIAL_HEIGHT);
			    			}
		    				else {
			    				setBounds(PrincipalGUI.getOptionActualPositionX(), PrincipalGUI.getOptionActualPositionY(), PrincipalGUI.getOptionActualWidth(), PrincipalGUI.getOptionActualHeight());
			    			}
		    			}
	    			}
	    			else {
	    				setBounds(PrincipalGUI.getOptionActualPositionX(), PrincipalGUI.getOptionActualPositionY(), PrincipalGUI.getOptionActualWidth(), PrincipalGUI.getOptionActualHeight());
	    			}
	    			setMinimumSize(new Dimension(Constants.OPTION_INITIAL_WIDTH, Constants.OPTION_INITIAL_HEIGHT));
	    		}
    		}
    		else {
	    		if (GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
	    			if (Platform.getOS().equals(Platform.OS.LINUX)){
	    				setBounds(
    				          Opciones.getInitialX(),
    				          Opciones.getInitialY(),
    				          Constants.OPTION_FONT_INITIAL_WIDTH_LINUX,
    				          Constants.OPTION_FONT_INITIAL_HEIGHT_LINUX
				        );
	    				setMinimumSize(new Dimension(getSize().width, getSize().height));
	    			}
	    			else {
	    				setBounds(Opciones.getInitialX(), Opciones.getInitialY(), Constants.OPTION_FONT_INITIAL_WIDTH, Constants.OPTION_FONT_INITIAL_HEIGHT);
	    				setMinimumSize(new Dimension(getSize().width, getSize().height));
	    			}
	    		}
	    		// Dimensiones normales
	    		else {
	    			setBounds(
			           Opciones.getInitialX(),
			           Opciones.getInitialY() - (Platform.OS.MACOSX.equals(Platform.getOS()) ? 70 : 0),
			           Constants.OPTION_INITIAL_WIDTH,
			           Constants.OPTION_INITIAL_HEIGHT + (Platform.OS.MACOSX.equals(Platform.getOS()) ? 50 : 0)
			        );
	    			setMinimumSize(new Dimension(getSize().width, getSize().height));
	    		}
    		}
    	}

    	//Se comprueba el estado de los botones de maximizado y restauracion
		if (!Platform.getOS().equals(Platform.OS.LINUX)){
			if (this.getSize().equals(new Dimension(maxWidth,maxHeight))){
				this.maximizeButton.setEnabled (false);
	    		this.restoreButton.setEnabled (true);
			}
			else {
				this.maximizeButton.setEnabled (true);
	    		this.restoreButton.setEnabled (false);
			}
		}
		else {

			if (this.getSize().equals(new Dimension(maxWidth,maxHeight - Constants.MAXIMIZE_VERTICAL_MARGIN_LINUX))){
				this.maximizeButton.setEnabled (false);
	    		this.restoreButton.setEnabled (true);
			}
			else {
				this.maximizeButton.setEnabled (true);
	    		this.restoreButton.setEnabled (false);
			}
		}

    	// Configuracion de la ventana
    	setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setTitle(Messages.getString("Opciones.opciones")); // NOI18N //$NON-NLS-1$
        getContentPane().removeAll();
        getContentPane().setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
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
                Messages.getString("Opciones.perfiles.pestana"), //$NON-NLS-1$
        		null,
        		this.profilesOptions.getConfigurationPanel(),
        		Messages.getString("Opciones.perfiles.descripcion")); //$NON-NLS-1$

        Utils.setContrastColor(this.mainPanel);
        Utils.setFontBold(this.mainPanel); //Control letra en negrita

        // Definicion de mnemonicos.
        final int tabNum = 0;
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

        this.mainPanel.addMouseListener(new MouseAdapter() {

        	/** {@inheritDoc} */
			@Override
			public void mousePressed(final MouseEvent e) {
				final int index = ((JTabbedPane)e.getSource()).getSelectedIndex();
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

		});

        this.mainPanel.addKeyListener(new KeyAdapter() {

        	/** {@inheritDoc} */
			@Override
			public void keyPressed(final KeyEvent e) {
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
				}
				else if(e.getKeyCode()==KeyEvent.VK_LEFT){
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
						break;
					case 2:
						HelpUtils.visualize("opciones.accesibilidad"); //$NON-NLS-1$
						break;
					case 3:
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

		final JPanel panelAceptar = new JPanel(new GridLayout(1, 1));

		// Boton aceptar
        this.aceptar.setText(Messages.getString("PrincipalGUI.aceptar")); // NOI18N //$NON-NLS-1$
        this.aceptar.setMnemonic(KeyEvent.VK_A); //Se asigna un atajo al boton aceptar
        this.getRootPane().setDefaultButton(this.aceptar); //Se asigna el boton por defecto para la ventana
        this.aceptar.addActionListener(new ActionListener() {
        	/** {@inheritDoc} */
            @Override
			public void actionPerformed(final ActionEvent evt) {

            	final Properties config = new Properties();
            	config.putAll(Opciones.this.getMainOptionsPane().getConfig());
            	config.putAll(Opciones.this.getContextOptions().getConfig());
            	config.putAll(Opciones.this.getAccessibilityOptions().getConfig());

                aceptarActionPerformed(config, Opciones.this.getProfilesOptions().getProfiles());
            }
        });
        this.aceptar.getAccessibleContext().setAccessibleDescription(Messages.getString("PrincipalGUI.aceptar")); // NOI18N //$NON-NLS-1$
        Utils.remarcar(this.aceptar);
        Utils.setContrastColor(this.aceptar);
        Utils.setFontBold(this.aceptar);
        panelAceptar.add(this.aceptar);

        final JPanel panelCancelar = new JPanel(new GridLayout(1, 1));

        // Boton cancelar
        final JButton	cancelar = new JButton();
        cancelar.setText(Messages.getString("PrincipalGUI.cancelar")); // NOI18N //$NON-NLS-1$
        cancelar.setMnemonic(KeyEvent.VK_C); //Se asigna un atajo al boton cancelar
        cancelar.addActionListener(new ActionListener() {
        	/** {@inheritDoc} */
            @Override
			public void actionPerformed(final ActionEvent evt) {
                cancelarActionPerformed();
            }
        });
        cancelar.getAccessibleContext().setAccessibleDescription(Messages.getString("PrincipalGUI.cancelar")); // NOI18N //$NON-NLS-1$
        Utils.remarcar(cancelar);
        Utils.setContrastColor(cancelar);
        Utils.setFontBold(cancelar);
        panelCancelar.add(cancelar);


        // Panel en donde se insertan los botones aceptar y cancelar
        final JPanel buttonPanel = new JPanel(new GridBagLayout());


        final GridBagConstraints c = new GridBagConstraints();

        c.anchor = GridBagConstraints.CENTER; //control de la orientacion de componentes
        c.insets = new Insets(0, 0, 0, 20);

		buttonPanel.add(panelAceptar, c);
		c.insets = new Insets(0, 0, 0, 0);
		buttonPanel.add(panelCancelar,c);


		final JPanel panelAyuda = new JPanel();
        // Boton ayuda
		final JButton botonAyuda = HelpUtils.helpButton("opciones.configuracion"); //$NON-NLS-1$
		botonAyuda.setName("helpButton"); //$NON-NLS-1$

		// Sustituimos el listener por defecto por otro que abrir la ventana de ayuda
		// correspondiente a la pestana seleccionada
		for (final ActionListener listener : botonAyuda.getActionListeners()) {
		    botonAyuda.removeActionListener(listener);
		}
		botonAyuda.addActionListener(new OpenHelpActionListener(this.mainPanel));

		final GridBagConstraints cons = new GridBagConstraints();
		cons.fill = GridBagConstraints.BOTH;
		cons.gridx = 0;
		cons.gridy = 0;

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


	/** Cierra la ventana y aplica todas las opciones seleccionadas
	 * @param config Configuraci&oacute;n actualmente establecida en la ventana de opciones.
	 * @param remainderProfilesNames Listado de nombres que deben permanecer registrados. */
    void aceptarActionPerformed(final Properties config, final String[] remainderProfilesNames) {

    	// Comprobamos que la politica de firma sea correcta
    	if (this.mainOptions != null && Boolean.parseBoolean(config.getProperty(MainOptionsPane.MAIN_POLICY_ESTABLISHED, "false")) && //$NON-NLS-1$
    			(!this.mainOptions.checkAboutBadPolicyId() ||
    			!this.mainOptions.checkSignaturePolicyQualifier() ||
    			!this.mainOptions.checkSha1MessageDigestLength())) {
			return;
    	}

    	// Guardamos la posicion y tamano actual de la ventana solo en caso de no estar maximizada por configuracion
    	if (!GeneralConfig.isMaximized()){
	    	PrincipalGUI.setOptionActualPositionX(this.getX());
	    	PrincipalGUI.setOptionActualPositionY(this.getY());
	    	PrincipalGUI.setOptionActualWidth(this.getWidth());
	    	PrincipalGUI.setOptionActualHeight(this.getHeight());
    	}

    	// Si se ha cambiado de vista (simple <-> avanzada) o se ha indicado que se desean todas las ventanas maximizadas o
    	// se ha indicado que se desean los cursores de texto grandes o se ha indicado que se desea remarcar los elementos
    	// con foco o se ha activado la opcion de alto contraste o se ha activado la opcion de tamano de fuente grande o se
    	// ha activado la opcion de fuente en negrita, actualizamos la ventana principal
    	final boolean needUpdateGUI = GeneralConfig.isAvanzados() != Boolean.parseBoolean(config.getProperty(MainOptionsPane.MAIN_ADVANCED_VIEW)) ||
		GeneralConfig.isMaximized() != Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_WINDOWS_SIZE)) ||
		GeneralConfig.isBigCaret() != Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_CURSOR_SIZE)) ||
		GeneralConfig.isRemarked() != Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_FOCUS_VISIBLE)) ||
		GeneralConfig.isHighContrast() != Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_HIGHT_CONTRAST)) ||
		GeneralConfig.isBigFontSize() != Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_FONT_SIZE)) ||
		GeneralConfig.isFontBold() != Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_FONT_STYLE)) ||
		update;

    	// Guardamos el estado actual de la configuracion de la herramienta
    	GeneralConfig.loadConfig(config);

    	// Si estamos en el perfil por defecto, guardamos todos los cambios en las preferencias
    	// para que persistan
    	if (ProfilesOptionsPane.getCurrentProfileId() == null ||
    			ProfileManager.DEFAULT_PROFILE_NAME.equals(
    			ProfileManager.getProfileName(ProfilesOptionsPane.getCurrentProfileId()))) {
    		ProfileManager.savePreferencesOnDefaultProfile(config);	
    	}
    	
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

//    	// Guardamos la nueva configuracion
//    	saveProfile();

    	// Cerramos la pantalla
    	dispose();
    	HelpUtils.visualize(Main.getHelpIndex());
    }

	/**
     * Cierra la ventana
     */
    void cancelarActionPerformed() {
    	// Guardamos la posicion y tamano actual de la ventana solo en caso de no estar maximizada por configuracion
    	if (!GeneralConfig.isMaximized()){
	    	PrincipalGUI.setOptionActualPositionX(this.getX());
	    	PrincipalGUI.setOptionActualPositionY(this.getY());
	    	PrincipalGUI.setOptionActualWidth(this.getWidth());
	    	PrincipalGUI.setOptionActualHeight(this.getHeight());
    	}
    	dispose();
    	HelpUtils.visualize(Main.getHelpIndex());
    }

    /**
	 * Cambia el tama&ntilde;o de la ventana al tama&ntilde;o m&aacute;ximo de pantalla menos el tama&ntilde;o de la barra de tareas de windows
	 */
	void maximizarActionPerformed(){
		setActualPositionX(this.getX());
		setActualPositionY(this.getY());
		setActualWidth(this.getWidth());
		setActualHeight(this.getHeight());

		//Se obtienen las dimensiones totales disponibles para mostrar una ventana
		final Rectangle rect =  GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();

		//Se obtienen las dimensiones de maximizado
		final int maxWidth = (int)rect.getWidth();
		final int maxHeight = (int)rect.getHeight();

		this.maximizeButton.setEnabled (false);
		this.restoreButton.setEnabled (true);

		//Se hace el resize dependiendo del so
		if (!Platform.getOS().equals(Platform.OS.LINUX)){
			this.setBounds(0,0, maxWidth, maxHeight);
		}
		else {
			this.setBounds(0,0, maxWidth, maxHeight - Constants.MAXIMIZE_VERTICAL_MARGIN_LINUX);
		}
	}

	/**
	 * Restaura el tama&ntilde;o de la ventana a la posicion anterior al maximizado
	 */
	void restaurarActionPerformed(){

		if (getActualPositionX() != -1 && getActualPositionY() != -1 && getActualWidth() != -1 && getActualHeight() != -1){
			this.setBounds(getActualPositionX(), getActualPositionY(), getActualWidth(), getActualHeight());
		}
		else {
			if (GeneralConfig.isBigFontSize() || GeneralConfig.isFontBold()){
    			if (Platform.getOS().equals(Platform.OS.LINUX)){
    				setBounds(Opciones.getInitialX(), Opciones.getInitialY(), Constants.OPTION_FONT_INITIAL_WIDTH_LINUX, Constants.OPTION_FONT_INITIAL_HEIGHT_LINUX);
    				setMinimumSize(new Dimension(getSize().width, getSize().height));
    			}
    			else {
    				setBounds(Opciones.getInitialX(), Opciones.getInitialY(), Constants.OPTION_FONT_INITIAL_WIDTH, Constants.OPTION_FONT_INITIAL_HEIGHT);
    				setMinimumSize(new Dimension(getSize().width, getSize().height));
    			}
    		}
			else {
    			setBounds(Opciones.getInitialX(), Opciones.getInitialY(), Constants.OPTION_INITIAL_WIDTH, Constants.OPTION_INITIAL_HEIGHT);
    			setMinimumSize(new Dimension(getSize().width, getSize().height));
    		}
		}
		this.maximizeButton.setEnabled (true);
		this.restoreButton.setEnabled (false);
	}

	private static final class OpenHelpActionListener implements ActionListener {

	    private final JTabbedPane tabbedPane;

	    OpenHelpActionListener(final JTabbedPane tabbedpane) {
	        this.tabbedPane = tabbedpane;
        }

	    /** {@inheritDoc} */
        @Override
		public void actionPerformed(final ActionEvent e) {

            switch (this.tabbedPane.getSelectedIndex()) {
            case ACCESIBILITY_OPTIONS_IDX:
                HelpUtils.showHelp("opciones.accesibilidad"); //$NON-NLS-1$
                break;
            case PDF_OPTIONS_IDX:
                HelpUtils.showHelp(""); //$NON-NLS-1$
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
	private static void removeDeletedProfiles(final String[] remainderProfiles) {

		boolean remain;
		for (final String name : ProfileManager.getProfilesNames()) {
			remain = false;
			for (final String remainderProfileName : remainderProfiles) {
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

	/** Recupera la configuraci&oacute;n global establecida en los paneles de opciones.
	 * @return Propiedades de configuraci&oacute;n. */
	Properties getConfiguration() {
		final Properties config = new Properties();
		config.putAll(this.mainOptions.getConfig());
		config.putAll(this.contextOptions.getConfig());
		config.putAll(this.accessibilityOptions.getConfig());

		return config;
	}

	Properties getSignConfig() {
		final Properties config = new Properties();
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
		final JPanel panel = new JPanel(new GridBagLayout());

		//Restricciones para los botones
		final GridBagConstraints consButtons = new GridBagConstraints();
		consButtons.fill = GridBagConstraints.BOTH;
		consButtons.gridx = 0;
		consButtons.gridy = 0;
		consButtons.weightx = 1.0;
		consButtons.weighty = 1.0;
		consButtons.insets = new Insets(0,0,0,0);  //right padding

		//Restore button
		final JPanel restorePanel = new JPanel();
		final ImageIcon imageIconRestore= new ImageIcon(CustomDialog.class.getResource("/resources/images/restore.png")); //$NON-NLS-1$
		this.restoreButton = new JButton(imageIconRestore);
		this.restoreButton.setMnemonic(KeyEvent.VK_R );
		this.restoreButton.setToolTipText(Messages.getString("Wizard.restaurar.description")); //$NON-NLS-1$
		this.restoreButton.getAccessibleContext().setAccessibleName(this.restoreButton.getToolTipText());

		this.restoreButton.addFocusListener(new FocusListener() {

			/** {@inheritDoc} */
			@Override
			public void focusLost(final FocusEvent e) {
				Utils.showToolTip(false, tip, Opciones.this.getRestoreButton(), tipText);
			}

			/** {@inheritDoc} */
			@Override
			public void focusGained(final FocusEvent e) {
				Utils.showToolTip(true, tip, Opciones.this.getRestoreButton(), tipText);
			}
		});
		final Dimension dimension = new Dimension(20,20);
		this.restoreButton.setPreferredSize(dimension);

		this.restoreButton.setName(Messages.getString("Wizard.restaurar")); //$NON-NLS-1$
		Utils.remarcar(this.restoreButton);
		restorePanel.add(this.restoreButton);
		this.restoreButton.addActionListener(new ActionListener() {
			/** {@inheritDoc} */
            @Override
			public void actionPerformed(final ActionEvent e) {
	    		restaurarActionPerformed();
			}
		});

		panel.add(restorePanel, consButtons);

		consButtons.gridx = 1;
		consButtons.insets = new Insets(0,0,0,0);  //right padding

		//Maximize button
		final JPanel maximizePanel = new JPanel();

		final ImageIcon imageIconMaximize= new ImageIcon(CustomDialog.class.getResource("/resources/images/maximize.png")); //$NON-NLS-1$
		this.maximizeButton = new JButton(imageIconMaximize);
		this.maximizeButton.setMnemonic(KeyEvent.VK_M );
		this.maximizeButton.setToolTipText(Messages.getString("Wizard.maximizar.description")); //$NON-NLS-1$
		this.maximizeButton.getAccessibleContext().setAccessibleName(this.maximizeButton.getToolTipText());

		this.maximizeButton.setName(Messages.getString("Wizard.maximizar")); //$NON-NLS-1$
		//Se asigna una dimension por defecto
		this.maximizeButton.setPreferredSize(dimension);

		Utils.remarcar(this.maximizeButton);
		maximizePanel.add(this.maximizeButton);

		this.maximizeButton.addFocusListener(new FocusListener() {

			/** {@inheritDoc} */
			@Override
			public void focusLost(final FocusEvent e) {
				Utils.showToolTip(false, tip, Opciones.this.getMaximizeButton(), tipText);
			}

			/** {@inheritDoc} */
			@Override
			public void focusGained(final FocusEvent e) {
				Utils.showToolTip(true, tip, Opciones.this.getMaximizeButton(), tipText);
			}
		});

		this.maximizeButton.addActionListener(new ActionListener() {

                @Override
				public void actionPerformed(final ActionEvent e) {
		    		maximizarActionPerformed();
				}
			});


		panel.add(maximizePanel, consButtons);

		//Se anade al panel general
		//Restricciones para el panel de botones
		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.NONE;
		c.gridx = 0;
		c.gridy = 0;
		c.weightx = 1.0;
		c.weighty = 1.0;
		c.insets = new Insets(0,0,0,0);
		c.anchor=GridBagConstraints.EAST;
		this.accessibilityButtonsPanel.add(panel, c);


		// Habilitado/Deshabilitado de botones restaurar/maximizar
    	if (GeneralConfig.isMaximized()){
    		//Se deshabilita el boton de maximizado
    		this.maximizeButton.setEnabled(false);
    		//Se habilita el boton de restaurar
    		this.restoreButton.setEnabled(true);
    	}
    	else {
    		//Se habilita el boton de maximizado
    		this.maximizeButton.setEnabled(true);
    		//Se deshabilita el boton de restaurar
    		this.restoreButton.setEnabled(false);
    	}

	}

//	private void saveProfile() {
//		try {
//			ProfileManager.saveConfiguration(UserProfile.getCurrentProfileId(), ProfileManager.getProfileName(UserProfile.getCurrentProfileId()), this.getConfiguration());
//		}
//		catch (final IllegalArgumentException e) {
//			CustomDialog.showMessageDialog(
//					this,
//					true,
//					"Se ha insertado un nombre de fichero no v\u00E1lido. No se guardar\u00E1 el perfil.",
//					"Perfiles",
//					JOptionPane.ERROR_MESSAGE);
//		}
//	}

}