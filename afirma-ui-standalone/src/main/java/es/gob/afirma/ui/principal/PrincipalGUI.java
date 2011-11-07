/*
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.principal;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.net.URL;
import java.security.Provider;
import java.security.Security;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import javax.swing.ButtonGroup;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JSeparator;
import javax.swing.LookAndFeel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.WindowConstants;
import javax.swing.plaf.metal.MetalLookAndFeel;
import javax.swing.plaf.metal.MetalTheme;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.ui.listeners.ElementDescriptionFocusListener;
import es.gob.afirma.ui.listeners.ElementDescriptionMouseListener;
import es.gob.afirma.ui.utils.Constants;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.HighContrastTheme;
import es.gob.afirma.ui.utils.JAccessibilityFrame;
import es.gob.afirma.ui.utils.JStatusBar;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;

/**
 *
 * Ventana principal de la aplicacion. Desde aqui se invocan a todas los paneles
 * que contienen el resto de objetos: firma, validacion, cifrado, descifrado,
 * ensobrado y desensobrado.
 * 
 */
public class PrincipalGUI extends JAccessibilityFrame {

	private static final long serialVersionUID = 1L;

	private static final String DEFAULT_LOCALE = "es_ES"; //$NON-NLS-1$
	
    /** Ruta del JAR en donde se almacenan los iconos de la aplicaci&oacute;n. */
    private static final String ICON_DIR_PATH = "/resources/images/";  //$NON-NLS-1$
    
    /** Ruta del JAR en donde se almacenan las im&aacute;agnees de la aplicaci&oacute;n. */
    private static final String IMAGE_DIR_PATH = "/resources/images/";  //$NON-NLS-1$
    
	private int actualPositionX = -1;
	
	private int actualPositionY = -1;
	
	private int actualWidth = -1;
	
	private int actualHeight = -1;
	
	private double maximizedHight = 0;
	
	private double maximizedWidth = 0;
	
	public static int optionActualPositionX = -1;
	
	public static int optionActualPositionY = -1;
	
	public static int optionActualWidth = -1;
	
	public static int optionActualHeight = -1;
	
	public static int wizardActualPositionX = -1;
	
	public static int wizardActualPositionY = -1;
	
	public static int wizardActualWidth = -1;
	
	public static int wizardActualHeight = -1;
	
	public static int aboutActualPositionX = -1;
	
	public static int aboutActualPositionY = -1;
	
	public static int aboutActualWidth = -1;
	
	public static int aboutActualHeight = -1;
	
	public static int fileActualPositionX = -1;
	
	public static int fileActualPositionY = -1;
	
	public static int fileActualWidth = -1;
	
	public static int fileActualHeight = -1;
	
	private JMenuBar menu;
	
	public static JStatusBar bar = new JStatusBar();
	
	private static int linuxMargin = 35;
	
	/**
	 * Tema por defecto de tipo Metal.
	 */
	private MetalTheme defaultTheme = MetalLookAndFeel.getCurrentTheme();
	
	/**
	 * LookAndFeel por defecto.
	 */
	private LookAndFeel defaultLookAndFeel = UIManager.getLookAndFeel();
	
//	private JTabbedPane panelPest = null;
	
	private HorizontalTabbedPanel htPanel;
		
	@Override
	public int getMinimumRelation(){
		return 7;
	}
	
	/**
	 * Posicion X inicial de la ventana dependiendo de la resolucion de pantalla.
	 * @return int Posicion X
	 */
	public int getInitialX() {
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); //329
		return (screenSize.width - 500) / 2 ;
	}
	
	/**
	 * Posicion Y inicial de la ventana dependiendo del sistema operativo y de la
	 * resolucion de pantalla.
	 * @return int Posicion Y
	 */
	public int getInitialY() {
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); //329
		if (Platform.getOS().equals(Platform.OS.MACOSX)){
			return (screenSize.height - 340) / 2;
		}
		return (screenSize.height - 320) / 2;
	}
		
	public PrincipalGUI() {
		super();
		initComponents();
		iniciarProveedores();
		this.addComponentListener(new ComponentAdapter() {
            @Override
		    public void componentResized(ComponentEvent e)
		    {
		    	resized(e);
		    }
            @Override
		    public void componentMoved(ComponentEvent e)
		    {
		    	resized(e);
		    }
		});
		
	}

	/**
	 * Inicializacion de los componentes
	 */
	private void initComponents() {
		// Dimensiones de la ventana
		setBounds(this.getInitialX(), this.getInitialY(), Constants.WINDOW_INITIAL_WIDTH, Constants.WINDOW_INITIAL_HEIGHT);
		// Parametros ventana
		setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE); // NOI18N
		setTitle("Firma"); //$NON-NLS-1$
		getContentPane().setLayout(new BorderLayout(11, 7));
		setMinimumSize(getSize());

		// Icono de @firma
        setIconImage(this.loadIcon("afirma_ico.png").getImage()); //$NON-NLS-1$
		
		// Componentes principales
		this.htPanel = new HorizontalTabbedPanel();
		getContentPane().add(this.htPanel, BorderLayout.CENTER);
		
		// Menu
		menu = new JMenuBar();
		// Menu de herramientas
		generarMenuHerramientas();
		// Menu de ayuda
		generarMenuAyuda();	
		
		setJMenuBar(menu);
		
		// Panel superior
		JPanel arriba = new JPanel();
		arriba.setMinimumSize(new Dimension(1, 1));
		arriba.setPreferredSize(new Dimension(1, 1));
		getContentPane().add(arriba, BorderLayout.PAGE_START);

		// Panel derecho
		JPanel derecha = new JPanel();
		derecha.setMinimumSize(new Dimension(1, 1));
		derecha.setPreferredSize(new Dimension(1, 1));
		getContentPane().add(derecha, BorderLayout.LINE_END);

		// Panel izquierdo
		JPanel izquierda = new JPanel();
		izquierda.setMinimumSize(new Dimension(1, 1));
		izquierda.setPreferredSize(new Dimension(1, 1));
		getContentPane().add(izquierda, BorderLayout.LINE_START);

		// Barra de estado
		bar.setLabelWidth((int) Toolkit.getDefaultToolkit().getScreenSize().getWidth());
		bar.setStatus(""); //$NON-NLS-1$
		bar.setLeftMargin(3);
		
		getContentPane().add(bar, BorderLayout.SOUTH);

		crearPaneles();
	}

	/**
	 * Genera el menu Herramientas con los submenus Opciones, Idiomas, Salir.
	 * @return	Menu de herramientas
	 */
	public JMenu generarMenuHerramientas() {
		menu.removeAll();
		// Opcion del menu principal - Herramientas
		JMenu herramientas = new JMenu();
		herramientas.setMnemonic(KeyEvent.VK_S);
		herramientas.setText(Messages.getString("PrincipalGUI.herramientas.text")); //$NON-NLS-1$
        herramientas.setToolTipText(Messages.getString("PrincipalGUI.herramientas.text")); //$NON-NLS-1$
        Utils.setContrastColor(herramientas);
		Utils.setFontBold(herramientas);
		Utils.remarcar(herramientas);
		menu.add(herramientas);
		
		// Subopcion menu Herramientas - Opciones
		JMenuItem opciones = new JMenuItem();
		opciones.setText(Messages.getString("Opciones.opciones")); //$NON-NLS-1$
		opciones.setMnemonic(KeyEvent.VK_O); //Se asigna un atajo al menu
		opciones.addActionListener(new ActionListener() {
            @Override
			public void actionPerformed(ActionEvent evt) {
				opcionesActionPerformed();
			}
		});
		Utils.setContrastColor(opciones);
		Utils.setFontBold(opciones);
		herramientas.add(opciones);
		
		// Subopcion menu Herramientas - Idiomas
		JMenu menuIdioma = new JMenu();
		menuIdioma.setText(Messages.getString("Opciones.general.idioma")); //$NON-NLS-1$
		menuIdioma.setMnemonic(KeyEvent.VK_I); //Se asigna un atajo al menu
		
		// Obtenemos ruta donde se encuentra la aplicacion
		URL baseDirectory = getClass().getProtectionDomain().getCodeSource().getLocation();
		File languagesDirectory = null;

		// Obtenemos el contenido del directorio languages
		try {
			File fileDirectory =  new File(baseDirectory.toURI());
			if (fileDirectory.isFile())
				fileDirectory = fileDirectory.getParentFile();
			languagesDirectory  = new File (fileDirectory, "languages"); //$NON-NLS-1$
		} catch (Exception ex) {
			ex.printStackTrace();
		}        	

		// Inicialmente introducimos el espanol
		List<String> languages = new ArrayList<String>();
		languages.add(DEFAULT_LOCALE);
		
		// Parseamos los nombres de las librerias de idiomas para obtener los codigos
		// del idioma.
		if ((languagesDirectory != null) && (languagesDirectory.isDirectory())) {        	
			File[] listFiles = languagesDirectory.listFiles();

			for (int i=0; i<listFiles.length; i++) {
				if (listFiles[i] != null && listFiles[i].isFile() && listFiles[i].getName().startsWith("help")) {
					String locale = listFiles[i].getName().substring(5,listFiles[i].getName().indexOf(".jar")); 
					languages.add(locale);
				}
			}
		}
		
		//Lista de mnemonicos usados para los radio buttons de lenguajes
		List<Character> mnemonicList = new ArrayList<Character>();
		
		// Generamos las opciones del menu idiomas
		ButtonGroup grupo = new ButtonGroup();
		for (String language : languages)
			if (language != null) {
				final Locale locale = new Locale(language.substring(0, 2), language.substring(3));
				String languageName = locale.getDisplayLanguage(locale);
				JRadioButtonMenuItem opcionIdioma = new JRadioButtonMenuItem(languageName.substring(0, 1).toUpperCase() + languageName.substring(1));
				
				//Se asigna un mnemonico que no haya sido utilizado
				opcionIdioma.setMnemonic(Utils.getLanguageMnemonic(mnemonicList, languageName.toLowerCase()));
				
				Utils.setContrastColor(opcionIdioma);
				Utils.setFontBold(opcionIdioma);
				menuIdioma.add(opcionIdioma);
				grupo.add(opcionIdioma);

				if (Locale.getDefault().equals(locale)){
					opcionIdioma.setSelected(true);  
				}

				opcionIdioma.addItemListener(new ItemListener() {
					public void itemStateChanged(ItemEvent e) {
						if (e.getStateChange() == ItemEvent.SELECTED)
							cambiarIdioma(locale);
					}
				});
			}

		Utils.setContrastColor(menuIdioma);
		Utils.setFontBold(menuIdioma);
		herramientas.add(menuIdioma);

		
		// Separador
		JSeparator separador = new JSeparator();
		herramientas.add(separador);

		// Subopcion menu Herramientas - Salir
		JMenuItem salir = new JMenuItem();
		salir.setText(Messages.getString("PrincipalGUI.salir")); // NOI18N
		salir.setMnemonic(KeyEvent.VK_L); //Se asigna un atajo al menu
		salir.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				salirActionPerformed();
			}
		});
		Utils.setContrastColor(salir);
		Utils.setFontBold(salir);
		herramientas.add(salir);
		
		return herramientas;
	}
	
	/**
	 * Genera el menu de ayuda con las opciones Ayuda, Acerca de.
	 * @return	Menu de ayuda
	 */
	public JMenu generarMenuAyuda() {
		// Opcion del menu principal - Ayuda
		JMenu ayuda = new JMenu();
		ayuda.setMnemonic(KeyEvent.VK_Y);
		ayuda.setText(Messages.getString("PrincipalGUI.ayuda.text")); // NOI18N
		ayuda.setToolTipText(Messages.getString("PrincipalGUI.ayuda.text")); // NOI18N
		Utils.setContrastColor(ayuda);
		Utils.setFontBold(ayuda);
		Utils.remarcar(ayuda);
		menu.add(ayuda);

		// Subopcion menu Ayuda - Ayuda
		JMenuItem ayudaHTML = new JMenuItem();
		ayudaHTML.setText(Messages.getString("ayudaHTML.contenido")); // NOI18N
		ayudaHTML.setMnemonic(KeyEvent.VK_U); //Se asigna un atajo al menu
		ayudaHTML.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				ayudaHTMLActionPerformed();
			}
		});
		Utils.setContrastColor(ayudaHTML);
		Utils.setFontBold(ayudaHTML);
		ayuda.add(ayudaHTML);
		
		// Subopcion menu Ayuda - Acerca de
		JMenuItem acerca = new JMenuItem();
		acerca.setText(Messages.getString("ayuda.contenido")); // NOI18N
		acerca.setMnemonic(KeyEvent.VK_C); //Se asigna un atajo al menu
		acerca.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				acercaActionPerformed();
			}
		});

		Utils.setContrastColor(acerca);
		Utils.setFontBold(acerca);
		ayuda.add(acerca);
		
		this.callResize();
		return ayuda;
	}

	/**
	 * Seleccion menu opciones: Muestra la ventana modal con las opciones
	 */
	private void opcionesActionPerformed() {
		Opciones ventanaOpciones = new Opciones(PrincipalGUI.this);
		ventanaOpciones.setModal(true);
		ventanaOpciones.setVisible(true);
	}

	/**
	 * Seleccion idiomas: Cambia el idioma de la aplicaci�n
	 * @param locale	Nuevo Locale
	 */
	private void cambiarIdioma(Locale locale) {
		Locale.setDefault(locale);
		HelpUtils.change(locale.toString());
		this.getContentPane().removeAll();
		
		// Cambia el idioma de los mensajes
		Messages.changeLocale();
		initComponents();
		SwingUtilities.updateComponentTreeUI(this);
	}
	
	/**
	 * Seleccion menu salir: Cierra la aplicaci�n
	 */
	private void salirActionPerformed() {
		System.exit(0);
	}

	/**
	 * Seleccion menu acerca de: Muestra la ventana con la informacion de aFirma
	 */
	void acercaActionPerformed() {
		Acercade.main();
	}

	/**
	 * Seleccion menu ayuda: Muestra la ventana con el panel de ayuda
	 */
	private void ayudaHTMLActionPerformed() {
		HelpUtils.visualize(true);
	}
	
	/**
	 * Construye el panel principal de la aplicaci&oacute;n con las pesta&ntilde;as de
	 * las distintas funcionalidades.
	 */
	public void crearPaneles() {
	    
	    this.htPanel.reset();

	    // Comprobacion del estado de Ventanas Maximizadas para que se genere 
	    // la ventana principal con el tamaño adecuado
	    if (this.getExtendedState()==MAXIMIZED_BOTH){
	    	maximizedWidth = this.getSize().getWidth();
			maximizedHight = this.getSize().getHeight();
	    }
	    if (GeneralConfig.isMaximized()){
	    	if (Platform.getOS().equals(Platform.OS.LINUX)) {
	    		//Se obtienen las dimensiones totales disponibles para mostrar una ventana
	    		Rectangle rect =  GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();

	    		//Se obtienen las dimensiones de maximizado
	    		int maxWidth = (int)rect.getWidth();
	    		int maxHeight = (int)rect.getHeight();
	    		if (!Platform.getOS().equals(Platform.OS.LINUX)) {
	    			//Se hace el resize
	    			this.setBounds(0,0, maxWidth, maxHeight);
	    		} else {
	    			//Se hace el resize
	    			this.setBounds(0,0, maxWidth, maxHeight-linuxMargin);
	    		}
	    		
	    	} else {
	    		this.setExtendedState(MAXIMIZED_BOTH);
	    	}
		} else {
			if (actualPositionX != -1 && actualPositionY != -1 && actualWidth != -1 && actualHeight != -1){
				if (actualWidth == maximizedWidth && actualHeight == maximizedHight){
					this.setExtendedState(MAXIMIZED_BOTH);
				} else{
					this.setExtendedState(0);
					this.setBounds(this.actualPositionX, this.actualPositionY, this.actualWidth, this.actualHeight);
				}
			} else {
				this.setExtendedState(0);
				setBounds(this.getInitialX(), this.getInitialY(), Constants.WINDOW_INITIAL_WIDTH, Constants.WINDOW_INITIAL_HEIGHT);
			}
		}
	    	
	    // Comprobacion del estado del Alto Contraste para que se creen los paneles
	    // con la configuracion adecuada
	    if (GeneralConfig.isHighContrast()){
			setHighContrast(true);
		} else {
			setHighContrast(false);
		}
	    Utils.setContrastColor(bar);
	    
	    Icon baseIcon = this.loadIcon("boton_transparente.png"); //$NON-NLS-1$
	    
	    // Panel de firma
	    ToggleImageButton buttonFirma = this.createToggleButton(
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleFirma")); //$NON-NLS-1$
        buttonFirma.setSelectedImage(this.loadImage("boton_fondo.png")); //$NON-NLS-1$
        buttonFirma.setToggledIcon(this.loadIcon("boton_firma_ico.png"), baseIcon); //$NON-NLS-1$
        buttonFirma.setSelectedToggledIcon(this.loadIcon("boton_firma_sel_ico.png"), baseIcon); //$NON-NLS-1$
        
        buttonFirma.setMnemonic(KeyEvent.VK_F);
        buttonFirma.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Firma.botonpricipal.status"))); //$NON-NLS-1$
        buttonFirma.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Firma.botonpricipal.status"))); //$NON-NLS-1$
        buttonFirma.getAccessibleContext().setAccessibleName(
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleFirma") + " " + //$NON-NLS-1$ //$NON-NLS-2$
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleFirma.description")); //$NON-NLS-1$
        JPanel panelFirma = new Firma();
        this.htPanel.addTab(buttonFirma, panelFirma);
	    
     // Panel de multifirma
        ToggleImageButton buttonMultifirma = this.createToggleButton(
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleMultifirma")); //$NON-NLS-1$
        buttonMultifirma.setSelectedImage(this.loadImage("boton_fondo.png")); //$NON-NLS-1$
        buttonMultifirma.setToggledIcon(this.loadIcon("boton_multifirma_ico.png"), baseIcon); //$NON-NLS-1$
        buttonMultifirma.setSelectedToggledIcon(this.loadIcon("boton_multifirma_sel_ico.png"), baseIcon); //$NON-NLS-1$

        buttonMultifirma.setMnemonic(KeyEvent.VK_M);
        buttonMultifirma.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Multifirma.botonpricipal.status"))); //$NON-NLS-1$
        buttonMultifirma.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Multifirma.botonpricipal.status"))); //$NON-NLS-1$
        buttonMultifirma.getAccessibleContext().setAccessibleName(
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleMultifirma") + " " +  //$NON-NLS-1$ //$NON-NLS-2$
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleMultifirma.description")); //$NON-NLS-1$
        JPanel panelMultifirmaSimple = new MultifirmaSimple();
        this.htPanel.addTab(buttonMultifirma, panelMultifirmaSimple);
	    
        // Panel de multifirma masiva
        ToggleImageButton buttonMultifirmaMasiva = this.createToggleButton(
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleMultifirmaMasiva")); //$NON-NLS-1$
        buttonMultifirmaMasiva.setSelectedImage(this.loadImage("boton_fondo.png")); //$NON-NLS-1$
        buttonMultifirmaMasiva.setToggledIcon(this.loadIcon("boton_masiva_ico.png"), baseIcon); //$NON-NLS-1$
        buttonMultifirmaMasiva.setSelectedToggledIcon(this.loadIcon("boton_masiva_sel_ico.png"), baseIcon); //$NON-NLS-1$
        buttonMultifirmaMasiva.setDisabledToggledIcon(this.loadIcon("boton_masiva_dis_ico.png"), baseIcon); //$NON-NLS-1$

        buttonMultifirmaMasiva.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Masiva.botonpricipal.status"))); //$NON-NLS-1$
        buttonMultifirmaMasiva.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Masiva.botonpricipal.status"))); //$NON-NLS-1$
        buttonMultifirmaMasiva.getAccessibleContext().setAccessibleName(
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleMultifirmaMasiva") + " " +  //$NON-NLS-1$ //$NON-NLS-2$
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleMultifirma.description")); //$NON-NLS-1$
        
        buttonMultifirmaMasiva.setEnabled(GeneralConfig.isAvanzados());
        if (buttonMultifirmaMasiva.isEnabled()) {
            buttonMultifirmaMasiva.setMnemonic(KeyEvent.VK_I);
        }
        
        JPanel panelMultifirmaMasiva =  new MultifirmaMasiva();
        this.htPanel.addTab(buttonMultifirmaMasiva, panelMultifirmaMasiva);

        // Panel de validacion y extraccion de documentos
        ToggleImageButton buttonValidacion = this.createToggleButton(
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleValidacion")); //$NON-NLS-1$
        buttonValidacion.setSelectedImage(this.loadImage("boton_fondo.png")); //$NON-NLS-1$
        buttonValidacion.setToggledIcon(this.loadIcon("boton_validacion_ico.png"), baseIcon); //$NON-NLS-1$
        buttonValidacion.setSelectedToggledIcon(this.loadIcon("boton_validacion_sel_ico.png"), baseIcon); //$NON-NLS-1$

        buttonValidacion.setMnemonic(KeyEvent.VK_V);
        buttonValidacion.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Validacion.botonpricipal.status"))); //$NON-NLS-1$
        buttonValidacion.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Validacion.botonpricipal.status"))); //$NON-NLS-1$
        buttonValidacion.getAccessibleContext().setAccessibleName(
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleValidacion") + " " + //$NON-NLS-1$ //$NON-NLS-2$
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleValidacion.description")); //$NON-NLS-1$
        JPanel panelValidacion = new Validacion();
        this.htPanel.addTab(buttonValidacion, panelValidacion);

        // Panel de cifrado simetrico
        ToggleImageButton buttonCifrado = this.createToggleButton(
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleCifrado")); //$NON-NLS-1$
        buttonCifrado.setSelectedImage(this.loadImage("boton_fondo.png")); //$NON-NLS-1$
        buttonCifrado.setToggledIcon(this.loadIcon("boton_cifrado_ico.png"), baseIcon); //$NON-NLS-1$
        buttonCifrado.setSelectedToggledIcon(this.loadIcon("boton_cifrado_sel_ico.png"), baseIcon); //$NON-NLS-1$

        buttonCifrado.setMnemonic(KeyEvent.VK_C);
        buttonCifrado.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Cifrado.botonpricipal.status"))); //$NON-NLS-1$
        buttonCifrado.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Cifrado.botonpricipal.status"))); //$NON-NLS-1$
        buttonCifrado.getAccessibleContext().setAccessibleName(
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleCifrado") + " " +  //$NON-NLS-1$  //$NON-NLS-2$
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleCifrado.description")); //$NON-NLS-1$
        JPanel panelCifrado = new Cifrado();
        this.htPanel.addTab(buttonCifrado, panelCifrado);

        // Panel de Descifrado
        ToggleImageButton buttonDescifrado = this.createToggleButton(
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleDescifrado")); //$NON-NLS-1$
        buttonDescifrado.setSelectedImage(this.loadImage("boton_fondo.png")); //$NON-NLS-1$
        buttonDescifrado.setToggledIcon(this.loadIcon("boton_descifrado_ico.png"), baseIcon); //$NON-NLS-1$
        buttonDescifrado.setSelectedToggledIcon(this.loadIcon("boton_descifrado_sel_ico.png"), baseIcon); //$NON-NLS-1$

        buttonDescifrado.setMnemonic(KeyEvent.VK_D);
        buttonDescifrado.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Descifrado.botonpricipal.status"))); //$NON-NLS-1$
        buttonDescifrado.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Descifrado.botonpricipal.status"))); //$NON-NLS-1$
        buttonDescifrado.getAccessibleContext().setAccessibleName(
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleDescifrado") + " " +  //$NON-NLS-1$ //$NON-NLS-2$
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleDescifrado.description")); //$NON-NLS-1$
        JPanel panelDescifrado = new Descifrado();
        this.htPanel.addTab(buttonDescifrado, panelDescifrado);

        // Panel de Ensobrado
        ToggleImageButton buttonEnsobrado = this.createToggleButton(
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleEnsobrado")); //$NON-NLS-1$
        buttonEnsobrado.setSelectedImage(this.loadImage("boton_fondo.png")); //$NON-NLS-1$
        buttonEnsobrado.setToggledIcon(this.loadIcon("boton_ensobrado_ico.png"), baseIcon); //$NON-NLS-1$
        buttonEnsobrado.setSelectedToggledIcon(this.loadIcon("boton_ensobrado_sel_ico.png"), baseIcon); //$NON-NLS-1$
        buttonEnsobrado.setDisabledToggledIcon(this.loadIcon("boton_ensobrado_dis_ico.png"), baseIcon); //$NON-NLS-1$

        buttonEnsobrado.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Ensobrado.botonpricipal.status"))); //$NON-NLS-1$
        buttonEnsobrado.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Ensobrado.botonpricipal.status"))); //$NON-NLS-1$
        buttonEnsobrado.getAccessibleContext().setAccessibleName(
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleEnsobrado") + " " +  //$NON-NLS-1$ //$NON-NLS-2$
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleEnsobrado.description")); //$NON-NLS-1$
        
        buttonEnsobrado.setEnabled(GeneralConfig.isAvanzados());
        if (buttonEnsobrado.isEnabled()) {
            buttonEnsobrado.setMnemonic(KeyEvent.VK_B);    
        }
        
        JPanel panelEnsobrado = new Ensobrado();
        this.htPanel.addTab(buttonEnsobrado, panelEnsobrado);

        // Panel de Desensobrado
        ToggleImageButton buttonDesensobrado = this.createToggleButton(
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleDesensobrado")); //$NON-NLS-1$
        buttonDesensobrado.setSelectedImage(this.loadImage("boton_fondo.png")); //$NON-NLS-1$
        buttonDesensobrado.setToggledIcon(this.loadIcon("boton_desensobrado_ico.png"), baseIcon); //$NON-NLS-1$
        buttonDesensobrado.setSelectedToggledIcon(this.loadIcon("boton_desensobrado_sel_ico.png"), baseIcon); //$NON-NLS-1$
        buttonDesensobrado.setDisabledToggledIcon(this.loadIcon("boton_desensobrado_dis_ico.png"), baseIcon); //$NON-NLS-1$

        
        buttonDesensobrado.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Desensobrado.botonpricipal.status"))); //$NON-NLS-1$
        buttonDesensobrado.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Desensobrado.botonpricipal.status"))); //$NON-NLS-1$
        buttonDesensobrado.getAccessibleContext().setAccessibleName(
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleDesensobrado") + " " + //$NON-NLS-1$ //$NON-NLS-2$
                Messages.getString("PrincipalGUI.TabConstraints.tabTitleDesensobrado.description")); //$NON-NLS-1$
        
        buttonDesensobrado.setEnabled(GeneralConfig.isAvanzados());
        if (buttonDesensobrado.isEnabled()) {
            buttonDesensobrado.setMnemonic(KeyEvent.VK_N);    
        }
        
        JPanel panelDesensobrado = new Desensobrado();
        this.htPanel.addTab(buttonDesensobrado, panelDesensobrado);
        
		HelpUtils.enableHelpKey(panelFirma, "firma"); //$NON-NLS-1$
		HelpUtils.enableHelpKey(panelMultifirmaSimple, "multifirma"); //$NON-NLS-1$
		HelpUtils.enableHelpKey(panelMultifirmaMasiva, "firma.masiva"); //$NON-NLS-1$
		HelpUtils.enableHelpKey(panelValidacion, "validacion"); //$NON-NLS-1$
		HelpUtils.enableHelpKey(panelCifrado, "cifrado"); //$NON-NLS-1$
		HelpUtils.enableHelpKey(panelDescifrado, "descifrado"); //$NON-NLS-1$
		HelpUtils.enableHelpKey(panelEnsobrado, "ensobrado"); //$NON-NLS-1$
		HelpUtils.enableHelpKey(panelDesensobrado, "desensobrado"); //$NON-NLS-1$
		
		//Al repintar la pantalla principal para quitar o poner las opciones avanzadas hay que ajustar 
		//la fuente para que se mantenga tal y como la tenia el usuario antes de cambiar esta opcion
		this.callResize();
		
	}

	/**
     * Crea un bot&oacute;n que se queda pulsado tras hacer clic en &eacute;l. Al volver a
     * hacer clic sobre &eacute;l vuelve a su posici&oacute;n original.
     * @param text Texto del bot&oacute;n.
     * @return Bot&oacute;n creado.
     */
    private ToggleImageButton createToggleButton(final String text) {

        ToggleImageButton tButton = new ToggleImageButton();
        tButton.setHorizontalAlignment(SwingConstants.LEFT);
        tButton.setButtonText(text);
       
        Utils.remarcar(tButton);
        Utils.setContrastColor(tButton);
        Utils.setFontBold(tButton);
        return tButton;
    }
	
    /**
     * Carga un icono contenido en el directorio de iconos del proyecto.
     * @param filename Nombre del icono.
     * @return Icono.
     */
    private ImageIcon loadIcon(final String filename) {
        return new ImageIcon(this.getClass().getResource(ICON_DIR_PATH + filename));
    }
    
    /**
     * Carga una imagen contenido en el directorio de imagenes del proyecto.
     * @param filename Nombre del fichero de imagen.
     * @return Imagen.
     */
    private Image loadImage(final String filename) {
        return Toolkit.getDefaultToolkit().createImage(
                this.getClass().getResource(IMAGE_DIR_PATH + filename));
    }

	/**
	 * Inicia los proveedores
	 */
	private void iniciarProveedores(){
		if (Platform.getOS().equals(Platform.OS.WINDOWS)) {
			Security.removeProvider("SunMSCAPI"); //$NON-NLS-1$
			try {
				Security.addProvider((Provider) Class.forName("sun.security.mscapi.SunMSCAPI").newInstance()); //$NON-NLS-1$
			} catch (InstantiationException e) {
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				e.printStackTrace();
			} catch (ClassNotFoundException e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * Escribe el nuevo estado en la barra de estado.
	 * @param nuevoEstado Estado que hay que introducir en la barra de estado.
	 */
	public static void setNuevoEstado(String nuevoEstado){
		bar.setStatus(nuevoEstado);
	}

	/**
	 * Muestra la ventana de la aplicaci�n
	 */
	public void main() {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				setVisible(true);
			}
		});
	}
	
	/**
	 * Evento de redimensionado. Redimensiona el tamaño de la barra de estado 
	 * y de su contenido, tambien almacena los valores actuales de posicion y tama�o de
	 * la ventana.
	 * 
	 */
	public void resized(ComponentEvent e) {
		//Tamaño de la ventana
		Dimension screenSize = this.getSize();
	    bar.setPreferredSize(new Dimension((int) screenSize.getWidth()*10/100,(int) screenSize.getHeight()*5/100));
	    bar.setLabelSize((int) screenSize.getWidth(),(int) screenSize.getHeight()*4/100);
	    
	    //Se guarda la posición en el caso de que no se haya maximizado por configuración
		if (!GeneralConfig.isMaximized()){
			actualPositionX = this.getX();
			actualPositionY = this.getY();
			actualWidth = this.getWidth();
			actualHeight = this.getHeight();
		}
	}
	
	/**
	 * Activa y desactiva el visionado en alto contraste
	 * @param highContrast Boolean que indica el estado del Alto Contraste
	 */
	public void setHighContrast(boolean highContrast) {
		// TODO Alto contraste en ventanas de Cargar / Guardar fichero
//		UIDefaults d = UIManager.getDefaults();
//		Enumeration<Object> claves = d.keys();
//		while (claves.hasMoreElements())
//		   System.out.println(claves.nextElement());
		try {
			if (highContrast) {
				//Tema de alto contraste
				MetalTheme theme = new HighContrastTheme();
				// set the chosen theme
				MetalLookAndFeel.setCurrentTheme(theme);
				
				//set Metal look and feel
				UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");

				UIManager.put("OptionPane.messageForeground",Color.WHITE);
				UIManager.put("Button.foreground",Color.WHITE);
				UIManager.put("ToolTip.foreground",Color.WHITE);
				UIManager.put("ToolTip.background",Color.BLACK);
				UIManager.put("Label.foreground",Color.WHITE);
				UIManager.put("FileChooserUI", "com.sun.java.swing.plaf.windows.WindowsFileChooserUI");
				UIManager.put("TableHeader.foreground", Color.WHITE);
				

			} else {
				
				//Se comprueba si el lookAndFeel por defecto es el que se habia modificado para el modo
				//Alto contraste
				if (defaultLookAndFeel instanceof MetalLookAndFeel) {
					MetalLookAndFeel.setCurrentTheme(this.defaultTheme); //Se asigna el tema por defecto
				}
				
				//Se asigna el lookAndFeel que habia por defecto
				UIManager.setLookAndFeel(defaultLookAndFeel);
				
				UIManager.put("OptionPane.messageForeground",Color.BLACK);
				UIManager.put("Button.foreground",Color.BLACK);
				UIManager.put("ToolTip.foreground",Color.BLACK);
				UIManager.put("ToolTip.background",new Color(255,255,225));
				UIManager.put("Label.foreground",Color.BLACK);
				UIManager.put("FileChooserUI", "com.sun.java.swing.plaf.windows.WindowsFileChooserUI");
				UIManager.put("TableHeader.foreground", Color.BLACK);
			}
			
		} catch (ClassNotFoundException e1) {
			System.out.println(e1.getMessage());
		} catch (InstantiationException e1) {
			System.out.println(e1.getMessage());
		} catch (IllegalAccessException e1) {
			System.out.println(e1.getMessage());
		} catch (UnsupportedLookAndFeelException e1) {
			System.out.println(e1.getMessage());
		}
		SwingUtilities.updateComponentTreeUI(this);

		this.validate();
		this.repaint();
	}
}
