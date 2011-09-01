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
import java.awt.EventQueue;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.io.File;
import java.net.URL;
import java.security.Provider;
import java.security.Security;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JSeparator;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import es.gob.afirma.misc.Platform;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.JAccessibilityFrame;
import es.gob.afirma.ui.utils.JStatusBar;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardUtils.BotoneraInferior;


/**
 *
 * Ventana principal de la aplicacion. Desde aqui se invocan a todas los paneles
 * que contienen el resto de objetos: firma, validacion, cifrado, descifrado,
 * ensobrado y desensobrado.
 * 
 */
public class PrincipalGUI extends JAccessibilityFrame {

	private static final long serialVersionUID = 1L;

	private static final String DEFAULT_LOCALE = "es_ES";
	
	public static JStatusBar bar = new JStatusBar();
	private JTabbedPane panelPest = null;
	
	@Override
	public int getMinimumRelation(){
		return 7;
	}
	
	@Override
	public int getInitialHeight() {
		// Dimensiones de la ventana
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); //329
		if (Platform.getOS().equals(Platform.OS.MACOSX)){
			return (screenSize.height - 340) / 2;
		}else{
			return (screenSize.height - 320) / 2;
		}
	}
	@Override
	public int getInitialWidth() {
		// Dimensiones de la ventana
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); //329
		return (screenSize.width - 500) / 2 ;
	}
	
	public PrincipalGUI() {
		initComponents();
		iniciarProveedores();
		this.addComponentListener(new ComponentAdapter() {
		    public void componentResized(ComponentEvent e)
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
		setBounds(this.getInitialWidth(), this.getInitialHeight(), 550, 370);
		
		// Parametros ventana
		setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE); // NOI18N
		setTitle("Firma"); // NOI18N
		getContentPane().setLayout(new BorderLayout(11, 7));
		setMinimumSize(getSize());

		// Icono de @firma
		setIconImage(new ImageIcon(getClass().getResource("/resources/images/afirma_ico.png")).getImage());

		// Carga del contorno principal
		panelPest = new JTabbedPane();
		panelPest.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);
		panelPest.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent evt) {
				panelPestStateChanged();
			}
		});
		getContentPane().add(panelPest, BorderLayout.CENTER);

		// Menu
		JMenuBar menu = new JMenuBar();
		// Menu de herramientas
		JMenu herramientas = generarMenuHerramientas();
		menu.add(herramientas);
		// Menu de ayuda
		JMenu ayuda = generarMenuAyuda();
		menu.add(ayuda);
		
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
		bar.setStatus("");
		bar.setLeftMargin(3);
		getContentPane().add(bar, BorderLayout.SOUTH);

		crearPaneles();
	}

	/**
	 * Genera el menu Herramientas con los submenus Opciones, Idiomas, Salir.
	 * @return	Menu de herramientas
	 */
	private JMenu generarMenuHerramientas() {
		// Opcion del menu principal - Herramientas
		JMenu herramientas = new JMenu();
		herramientas.setMnemonic(KeyEvent.VK_S);
		herramientas.setText(Messages.getString("PrincipalGUI.herramientas.text")); // NOI18N
		herramientas.setToolTipText(Messages.getString("PrincipalGUI.herramientas.text")); // NOI18N

		// Subopcion menu Herramientas - Opciones
		JMenuItem opciones = new JMenuItem();
		opciones.setText(Messages.getString("Opciones.opciones")); // NOI18N
		opciones.setMnemonic(KeyEvent.VK_O); //Se asigna un atajo al menú
		opciones.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				opcionesActionPerformed();
			}
		});
		herramientas.add(opciones);

		// Subopcion menu Herramientas - Idiomas
		JMenu menuIdioma = new JMenu();
		menuIdioma.setText(Messages.getString("Opciones.general.idioma")); // NOI18N
		menuIdioma.setMnemonic(KeyEvent.VK_I); //Se asigna un atajo al menú
		
		// Obtenemos ruta donde se encuentra la aplicaciï¿½n
		URL baseDirectory = getClass().getProtectionDomain().getCodeSource().getLocation();
		File languagesDirectory = null;

		// Obtenemos el contenido del directorio languages
		try {
			File fileDirectory =  new File(baseDirectory.toURI());
			if (fileDirectory.isFile())
				fileDirectory = fileDirectory.getParentFile();
			languagesDirectory  = new File (fileDirectory, "languages");
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
		
		//Lista de mnemónicos usados para los radio buttons de lenguajes
		List<Character> mnemonicList = new ArrayList<Character>();
		
		// Generamos las opciones del menu idiomas
		ButtonGroup grupo = new ButtonGroup();
		for (String language : languages)
			if (language != null) {
				final Locale locale = new Locale(language.substring(0, 2), language.substring(3));
				String languageName = locale.getDisplayLanguage(locale);
				JRadioButtonMenuItem opcionIdioma = new JRadioButtonMenuItem(languageName.substring(0, 1).toUpperCase() + languageName.substring(1));
				
				//Se asigna un mnemónico que no haya sido utilizado
				opcionIdioma.setMnemonic(Utils.getLanguageMnemonic(mnemonicList, languageName.toLowerCase()));
				
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

		herramientas.add(menuIdioma);

		
		// Separador
		JSeparator separador = new JSeparator();
		herramientas.add(separador);

		// Subopcion menu Herramientas - Salir
		JMenuItem salir = new JMenuItem();
		salir.setText(Messages.getString("PrincipalGUI.salir")); // NOI18N
		salir.setMnemonic(KeyEvent.VK_L); //Se asigna un atajo al menú
		salir.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				salirActionPerformed();
			}
		});
		herramientas.add(salir);
		
		return herramientas;
	}
	
	/**
	 * Genera el menu de ayuda con las opciones Ayuda, Acerca de.
	 * @return	Menu de ayuda
	 */
	private JMenu generarMenuAyuda() {
		// Opcion del menu principal - Ayuda
		JMenu ayuda = new JMenu();
		ayuda.setMnemonic(KeyEvent.VK_Y);
		ayuda.setText(Messages.getString("PrincipalGUI.ayuda.text")); // NOI18N
		ayuda.setToolTipText(Messages.getString("PrincipalGUI.ayuda.text")); // NOI18N

		// Subopcion menu Ayuda - Ayuda
		JMenuItem ayudaHTML = new JMenuItem();
		ayudaHTML.setText(Messages.getString("ayudaHTML.contenido")); // NOI18N
		ayudaHTML.setMnemonic(KeyEvent.VK_U); //Se asigna un atajo al menu
		ayudaHTML.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				ayudaHTMLActionPerformed();
			}
		});
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

		ayuda.add(acerca);

		return ayuda;
	}

	/**
	 * Panel cambiar estado: Reinicia el estado de la barra de estado
	 */
	private void panelPestStateChanged() {
		setNuevoEstado("");
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
	 * Seleccion idiomas: Cambia el idioma de la aplicaciï¿½n
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
	 * Seleccion menu salir: Cierra la aplicaciï¿½n
	 */
	private void salirActionPerformed() {
		System.exit(0);
	}

	/**
	 * Seleccion menu acerca de: Muestra la ventana con la informacion de aFirma
	 */
	private void acercaActionPerformed() {
		Acercade.main();
	}

	/**
	 * Seleccion menu ayuda: Muestra la ventana con el panel de ayuda
	 */
	private void ayudaHTMLActionPerformed() {
		HelpUtils.visualize();
	}

	/**
	 * Construye el panel principal de la aplicaci&oacute;n con las pesta&ntilde;as de
	 * las distintas funcionalidades.
	 */
	public void crearPaneles() {
		// Eliminamos los paneles que haya actualmente antes de insertar los nuevos
		panelPest.removeAll();

		// Insertar la pestana de firma
		JPanel panelFirma = new Firma();
		panelFirma.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.TabConstraints.tabTitleFirma.description"));
		panelPest.addTab(Messages.getString("PrincipalGUI.TabConstraints.tabTitleFirma"),
				new ImageIcon(getClass().getResource("/resources/images/firma_mini_ico.png")),
				panelFirma,
				Messages.getString("PrincipalGUI.TabConstraints.tabTitleFirma.description"));

		// Insertar la pestana de multifirma
		JPanel panelMultifirmaSimple = new MultifirmaSimple();
		panelMultifirmaSimple.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.TabConstraints.tabTitleMultifirma.description"));
		panelPest.addTab(Messages.getString("PrincipalGUI.TabConstraints.tabTitleMultifirma"),
				new ImageIcon(getClass().getResource("/resources/images/firma_mini_ico.png")),
				panelMultifirmaSimple,
				Messages.getString("PrincipalGUI.TabConstraints.tabTitleMultifirma.description"));


		// Insertar la pestana de multifirma masiva (solo en la vista avanzada)
		JPanel panelMultifirmaMasiva = null;
		if(GeneralConfig.isAvanzados()) {
			panelMultifirmaMasiva =  new MultifirmaMasiva();
			panelMultifirmaMasiva.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.TabConstraints.tabTitleMultifirma.description"));
			panelPest.addTab(Messages.getString("PrincipalGUI.TabConstraints.tabTitleMultifirmaMasiva"),
					new ImageIcon(getClass().getResource("/resources/images/multi_mini_ico.png")),
					panelMultifirmaMasiva,
					Messages.getString("PrincipalGUI.TabConstraints.tabTitleMultifirmaMasiva.description"));
		}

		// Insertar la pestana de validaciï¿½n
		JPanel panelValidacion = new Validacion();
		panelValidacion.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.TabConstraints.tabTitleValidacion.description"));
		panelPest.addTab(Messages.getString("PrincipalGUI.TabConstraints.tabTitleValidacion"),
				new ImageIcon(getClass().getResource("/resources/images/validate_mini_ico.png")),
				panelValidacion,
				Messages.getString("PrincipalGUI.TabConstraints.tabTitleValidacion.description"));

		// Insertar la pestana de Cifrado
		JPanel panelCifrado = new Cifrado();
		panelCifrado.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.TabConstraints.tabTitleDescifrado.description"));
		panelPest.addTab(Messages.getString("PrincipalGUI.TabConstraints.tabTitleCifrado"),
				new ImageIcon(getClass().getResource("/resources/images/cifrado_mini_ico.png")),
				panelCifrado,
				Messages.getString("PrincipalGUI.TabConstraints.tabTitleCifrado.description"));

		// Insertar la pestana de Descifrado
		JPanel panelDescifrado = new Descifrado();
		panelDescifrado.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.TabConstraints.tabTitleCifrado.description"));
		panelPest.addTab(Messages.getString("PrincipalGUI.TabConstraints.tabTitleDescifrado"),
				new ImageIcon(getClass().getResource("/resources/images/descifrado_mini_ico.png")),
				panelDescifrado,
				Messages.getString("PrincipalGUI.TabConstraints.tabTitleDescifrado.description"));

		// Insertar la pestana de Ensobrado  (solo en la vista avanzada)
		JPanel panelEnsobrado = null;
		if(GeneralConfig.isAvanzados()) {
			panelEnsobrado = new Ensobrado();
			panelEnsobrado.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.TabConstraints.tabTitleEnsobrado.description"));
			panelPest.addTab(Messages.getString("PrincipalGUI.TabConstraints.tabTitleEnsobrado"),
					new ImageIcon(getClass().getResource("/resources/images/sobre_mini_ico.png")),
					panelEnsobrado,
					Messages.getString("PrincipalGUI.TabConstraints.tabTitleEnsobrado.description"));
		}

		// Insertar la pestana de Desensobrado  (solo en la vista avanzada)
		JPanel panelDesensobrado = null;
		if(GeneralConfig.isAvanzados()) {
			panelDesensobrado = new Desensobrado();
			panelDesensobrado.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.TabConstraints.tabTitleDesensobrado.description"));
			panelPest.addTab(Messages.getString("PrincipalGUI.TabConstraints.tabTitleDesensobrado"),
					new ImageIcon(getClass().getResource("/resources/images/desensobrado_mini_ico.png")),
					panelDesensobrado,
					Messages.getString("PrincipalGUI.TabConstraints.tabTitleDesensobrado.description"));
		}

		panelPest.addMouseMotionListener(new MouseMotionAdapter() {
			public void mouseMoved(MouseEvent evt) {
				panelPestMouseMoved(evt);
			}
		});

		// Definicion de mnemonicos.
		int tabNum = 0;

		// Firma
		panelPest.setMnemonicAt(tabNum++, KeyEvent.VK_F);
		// Multifirma simple
		panelPest.setMnemonicAt(tabNum++, KeyEvent.VK_M);
		// Multifirma masiva
		if (GeneralConfig.isAvanzados()) 
			panelPest.setMnemonicAt(tabNum++, KeyEvent.VK_I);
		// Validacion
		panelPest.setMnemonicAt(tabNum++, KeyEvent.VK_V);
		// Cifrado
		panelPest.setMnemonicAt(tabNum++, KeyEvent.VK_C);
		// Descifrado
		panelPest.setMnemonicAt(tabNum++, KeyEvent.VK_D);
		// Ensobrado
		if(GeneralConfig.isAvanzados()) 
			panelPest.setMnemonicAt(tabNum++, KeyEvent.VK_S);
		// Desensobrado
		if(GeneralConfig.isAvanzados()) 
			panelPest.setMnemonicAt(tabNum++, KeyEvent.VK_N);

		HelpUtils.enableHelpKey(panelFirma,"firma");
		HelpUtils.enableHelpKey(panelMultifirmaSimple,"multifirma");
		if (panelMultifirmaMasiva != null)
			HelpUtils.enableHelpKey(panelMultifirmaMasiva,"firma.masiva");
		HelpUtils.enableHelpKey(panelValidacion,"validacion");
		HelpUtils.enableHelpKey(panelCifrado,"cifrado");
		HelpUtils.enableHelpKey(panelDescifrado,"descifrado");
		if (panelEnsobrado != null)
			HelpUtils.enableHelpKey(panelEnsobrado,"ensobrado");
		if (panelDesensobrado != null)
			HelpUtils.enableHelpKey(panelDesensobrado,"desensobrado");
		panelPest.addFocusListener(new FocusListener() {
			public void focusLost(FocusEvent e) { 
				
			}

			public void focusGained(FocusEvent arg0) {
				final int index = panelPest.getSelectedIndex();
				int tab = 0;
				if (index == tab)
					HelpUtils.enableHelpKey(panelPest,"firma");
				else{
					tab++;
					if (index == tab)
						HelpUtils.enableHelpKey(panelPest,"multifirma");
					else{
						tab++;
						if(GeneralConfig.isAvanzados()){

							if (index == tab){
								HelpUtils.enableHelpKey(panelPest,"firma.masiva");					    		
							}
							tab++;					    	   
						}
						if (index == tab)
							HelpUtils.enableHelpKey(panelPest,"validacion");
						else
						{
							tab++;
							if (index == tab)
								HelpUtils.enableHelpKey(panelPest,"cifrado");
							else{
								tab++;
								if (index == tab)
									HelpUtils.enableHelpKey(panelPest,"descifrado");
								else{
									tab++;
									if(GeneralConfig.isAvanzados()){
										if (index == tab)
											HelpUtils.enableHelpKey(panelPest,"ensobrado");
										tab++;
									}
									if(GeneralConfig.isAvanzados())
										if (index == tab)
											HelpUtils.enableHelpKey(panelPest,"desensobrado");
								}
							}
						}
					}
				}
			}
		});
		//Al repintar la pantalla principal para quitar o poner las opciones avanzadas hay que ajustar 
		//la fuente para que se mantenga tal y como la tenia el usuario antes de cambiar esta opcion
		this.callResize();
	}

	private void panelPestMouseMoved(MouseEvent evt){
		Object src = evt.getSource();
		if (src instanceof JTabbedPane){
			JTabbedPane panel = (JTabbedPane) src;
			setNuevoEstado(panel.getToolTipText(evt));
		}
	}

	/**
	 * Inicia los proveedores
	 */
	private void iniciarProveedores(){
		if (Platform.getOS().equals(Platform.OS.WINDOWS)) {
			Security.removeProvider("SunMSCAPI");
			try {
				Security.addProvider((Provider) Class.forName("sun.security.mscapi.SunMSCAPI").newInstance());
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
	 * Muestra la ventana de la aplicaciï¿½n
	 */
	public void main() {	
		if (System.getProperty("java.version").compareTo("1.6.0_18") < 0) {
			JOptionPane.showMessageDialog(
					null,
					Messages.getString("main.requerido")+
					System.getProperty("java.version")+
					Messages.getString("main.porfavor"),
					Messages.getString("main.cliente"),
					JOptionPane.ERROR_MESSAGE);
			System.exit(-5);
		}

		EventQueue.invokeLater(new Runnable() {
			public void run() {
				setVisible(true);
			}
		});
	}
	
	/**
	 * Evento de redimensionado. Redimensiona el tamaÃ±o de la barra de estado y de su contenido
	 * 
	 */
	public void resized(ComponentEvent e) {
		Dimension screenSize = this.getSize();
	    bar.setPreferredSize(new Dimension((int) screenSize.getWidth()*10/100,(int) screenSize.getHeight()*5/100));
	    bar.setLabelSize((int) screenSize.getWidth(),(int) screenSize.getHeight()*4/100);
	}
}
