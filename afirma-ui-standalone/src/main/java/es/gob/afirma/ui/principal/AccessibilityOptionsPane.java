package es.gob.afirma.ui.principal;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.Properties;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JMenuItem;
import javax.swing.JPanel;

import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;

/**
 * Manejador de la configuraci&oacute;n de accesibilidad de la interfaz.
 * @author inteco
 *
 */
public class AccessibilityOptionsPane {
	 
	/** Clave para la configuraci&oacute;n de tama&ntilde;o de fuente. */
	public static final String MAIN_FONT_SIZE = "main.fontSize";
	
	/** Clave para la configuraci&oacute;n de estilo de fuente. */
	public static final String MAIN_FONT_STYLE = "main.fontStyle";
	
	/** Clave para la configuraci&oacute;n de vista en alto contraste. */
	public static final String MAIN_HIGHT_CONTRAST = "main.hightContrast";
	
	/** Clave para la configuraci&oacute;n de visibilidad del foco. */
	public static final String MAIN_FOCUS_VISIBLE = "main.focusVisible";
	
	/** Clave para la configuraci&oacute;n de tama&ntilde;o de ventana. */
	public static final String MAIN_WINDOWS_SIZE = "main.windowsSize";
	
	/** Clave para la configuraci&oacute;n de tama&ntilde;o de ventana. */
	public static final String MAIN_WINDOWS_ACCESSIBILITY = "main.windowsAccessibility";
	
	/** Clave para la configuraci&oacute;n de tama&ntilde;o del cursor de texto. */
	public static final String MAIN_CURSOR_SIZE = "main.cursorSize";
	
	/** Panel sobre el que se montan los componentes. */
	private final JPanel panel;
	
	/** Casilla de verificacion de tama&ntilde;o de fuente grande. */ 
	private JCheckBox checkFontSize;
	 
	/** Casilla de verificacion de fuente en negrita. */ 
	private JCheckBox checkFontStyle;
	 
	/** Casilla de verificacion de alto contraste. */ 
	private JCheckBox checkHighContrast;
	
	/** Casilla de verificacion de la visibilidad del foco. */ 
	private JCheckBox checkFocusVisible;
	
	/** Casilla de verificacion del tama&ntilde;o de las ventanas. */ 
	private JCheckBox checkWindowSize;
	
	/** Casilla de verificacion de la desactivaci&oacute;n de la accesibilidad en las ventanas de selecci&oacute;n de archivos. */ 
//	private JCheckBox checkWindowAccessibility;
	
	/** Casilla de verificacion del tama&ntilde;o del cursor de texto. */ 
	private JCheckBox checkCursorSize;	
	
	private boolean isBigStyle = false;

	/**
	 * Control de estilo grande.
	 */
	public static boolean continueBigStyle = false;
	
	/** Pantalla principal de la aplicaci&oacute;n. */
    private PrincipalGUI mainGui;
    
    /**
     * Boton aplicar cambios.
     */
    public JButton aplicar = new JButton();
    
    private boolean isChangeHighContrast = false;
	
	/**
	 * Componente padre.
	 */
	private JDialog parent = null;

	/**
	 * Constructor.
	 * @param parent componente padre
	 * @param mainGui principalGUI
	 */
	public AccessibilityOptionsPane(JDialog parent, PrincipalGUI mainGui){
		this.parent = parent;
		this.panel = new JPanel(new GridBagLayout());
		this.mainGui = mainGui;
		initComponents();
	}

	/**
	 * Inicialización de componentes.
	 */
	private void initComponents(){
		this.panel.removeAll();
		GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.weightx = 1.0;
        c.insets = new Insets(5, 13, 5, 13);
        c.gridy = 0;
        
    	// Panel texto
    	JPanel textPanel = new JPanel(new GridBagLayout());
    	textPanel.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.accesibilidad.texto"))); // NOI18N
    	Utils.setContrastColor(textPanel);
    	Utils.setFontBold(textPanel);
    	
    	GridBagConstraints c2 = new GridBagConstraints();
        c2.fill = GridBagConstraints.HORIZONTAL;
        c2.insets = new Insets(0, 13, 0, 13);
        c2.weightx = 1.0;
        c2.gridy = 0;
        
        JPanel panelFontSize = new JPanel(new GridLayout(1, 1));
        panelFontSize.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.accesibilidad.texto"));
        // Checkbox para habilitar la opcion de configuracion del tama&ntilde;o de fuente
        this.checkFontSize = new JCheckBox();
        this.checkFontSize.setText(Messages.getString("Opciones.accesibilidad.texto.tamano")); // NOI18N
        //checkFontSize.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.general.habilitar")); // NOI18N
        this.checkFontSize.setSelected(GeneralConfig.isAvanzados()); 
        this.checkFontSize.setBounds(12, 20, 340, 23);
        this.checkFontSize.setMnemonic(KeyEvent.VK_D); // AsignaciÃ³n de mnemÃ³nico al checkbox
        Utils.remarcar(this.checkFontSize);
        Utils.setContrastColor(this.checkFontSize);
        Utils.setFontBold(this.checkFontSize);
        
        panelFontSize.add(this.checkFontSize);
        textPanel.add(panelFontSize, c2);
        
        JPanel panelFontStyle = new JPanel(new GridLayout(1, 1));
        panelFontStyle.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.accesibilidad.texto"));
        //Checkbox para habilitar la opcion de configuracion del estilo de fuente
        this.checkFontStyle = new JCheckBox();
        this.checkFontStyle.setText(Messages.getString("Opciones.accesibilidad.texto.estilo")); // NOI18N
        //checkFontStyle.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.general.habilitar")); // NOI18N
        this.checkFontStyle.setSelected(GeneralConfig.isAvanzados()); 
        this.checkFontStyle.setBounds(12, 20, 340, 23);
        this.checkFontStyle.setMnemonic(KeyEvent.VK_N); // AsignaciÃ³n de mnemÃ³nico al checkbox
        Utils.remarcar(this.checkFontStyle);
        Utils.setContrastColor(this.checkFontStyle);
        Utils.setFontBold(this.checkFontStyle);
        
        panelFontStyle.add(this.checkFontStyle);
        textPanel.add(panelFontStyle, c2);
    	
    	this.panel.add(textPanel, c);
    	c.gridy = c.gridy + 1;
    	
    	// Panel Color
    	JPanel colorPanel = new JPanel(new GridBagLayout());
    	colorPanel.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.accesibilidad.color"))); // NOI18N
    	Utils.setContrastColor(colorPanel);
    	Utils.setFontBold(colorPanel);
    	
        c2.fill = GridBagConstraints.HORIZONTAL;
        c2.insets = new Insets(5, 13, 0, 13);
        c2.weightx = 1.0;
        c2.gridy = c2.gridy + 1;
        
        JPanel panelHighContrast = new JPanel(new GridLayout(1, 1));
        panelHighContrast.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.accesibilidad.color"));
        // Checkbox para habilitar la opcion de configuracion de alto contraste
        this.checkHighContrast = new JCheckBox();
        this.checkHighContrast.setText(Messages.getString("Opciones.accesibilidad.color.contraste")); // NOI18N
        //checkFontSize.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.general.habilitar")); // NOI18N
        this.checkHighContrast.setSelected(GeneralConfig.isAvanzados()); 
        this.checkHighContrast.setBounds(12, 20, 340, 23);
        this.checkHighContrast.setMnemonic(KeyEvent.VK_L); // AsignaciÃ³n de mnemÃ³nico al checkbox
        this.checkHighContrast.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				if (isChangeHighContrast){
					isChangeHighContrast = false;
				} else {
					isChangeHighContrast = true;
				}
			}
		});
        Utils.remarcar(this.checkHighContrast);
        Utils.setContrastColor(this.checkHighContrast);
        Utils.setFontBold(this.checkHighContrast);
        
        panelHighContrast.add(this.checkHighContrast);
        colorPanel.add(panelHighContrast, c2);
        
        this.panel.add(colorPanel, c);
        c.gridy = c.gridy + 1;
        
    	// Panel Foco
    	JPanel focusPanel = new JPanel(new GridBagLayout());
    	focusPanel.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.accesibilidad.foco"))); // NOI18N
    	Utils.setContrastColor(focusPanel);
    	Utils.setFontBold(focusPanel);
    	
        c2.fill = GridBagConstraints.HORIZONTAL;
        c2.insets = new Insets(5, 13, 0, 13);
        c2.weightx = 1.0;
        c2.gridy = c2.gridy + 1;
        
        JPanel panelFocusVisible = new JPanel(new GridLayout(1, 1));
        panelFocusVisible.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.accesibilidad.foco"));
        // Checkbox para habilitar la opcion de configuracion de la visibilidad del foco
        this.checkFocusVisible = new JCheckBox();
        this.checkFocusVisible.setText(Messages.getString("Opciones.accesibilidad.foco.remarcar")); // NOI18N
        //checkFontSize.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.general.habilitar")); // NOI18N
        this.checkFocusVisible.setSelected(GeneralConfig.isAvanzados()); 
        this.checkFocusVisible.setBounds(12, 20, 340, 23);
        this.checkFocusVisible.setMnemonic(KeyEvent.VK_F); // AsignaciÃ³n de mnemÃ³nico al checkbox
        Utils.remarcar(this.checkFocusVisible);
        Utils.setContrastColor(this.checkFocusVisible);
        Utils.setFontBold(this.checkFocusVisible);
        
        panelFocusVisible.add(this.checkFocusVisible);
        focusPanel.add(panelFocusVisible, c2);
        
        this.panel.add(focusPanel, c);
        c.gridy = c.gridy + 1;
    	
    	// Panel Ventana
    	JPanel windowPanel = new JPanel(new GridBagLayout());
    	windowPanel.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.accesibilidad.ventana"))); // NOI18N
    	Utils.setContrastColor(windowPanel);
    	Utils.setFontBold(windowPanel);
    	
        c2.fill = GridBagConstraints.HORIZONTAL;
        c2.insets = new Insets(5, 13, 0, 13);
        c2.weightx = 1.0;
        c2.gridy = c2.gridy + 1;
        
        JPanel panelWindowSize = new JPanel(new GridLayout(1, 1));
        panelWindowSize.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.accesibilidad.ventana"));
        // Checkbox para habilitar la opcion de configuracion de ventanas maximizadas
        this.checkWindowSize = new JCheckBox();
        this.checkWindowSize.setText(Messages.getString("Opciones.accesibilidad.ventana.tamano")); // NOI18N
        //checkFontSize.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.general.habilitar")); // NOI18N
        this.checkWindowSize.setSelected(GeneralConfig.isAvanzados()); 
        this.checkWindowSize.setBounds(12, 20, 340, 23);
        this.checkWindowSize.setMnemonic(KeyEvent.VK_V); // AsignaciÃ³n de mnemÃ³nico al checkbox
        Utils.remarcar(this.checkWindowSize);
        Utils.setContrastColor(this.checkWindowSize);
        Utils.setFontBold(this.checkWindowSize);
        
        panelWindowSize.add(this.checkWindowSize);
        windowPanel.add(panelWindowSize, c2);
        
//        c2.gridy = c2.gridy + 1;
//        JPanel panelWindowAccessibility = new JPanel(new GridLayout(1, 1));
//        panelWindowAccessibility.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.accesibilidad.ventana"));
//        //Checkbox para deshabilitar la accesibilidad en los dialogos de seleccion de archivo
//        this.checkWindowAccessibility = new JCheckBox();
//        this.checkWindowAccessibility.setText(Messages.getString("Opciones.accesibilidad.ventana.accesibilidad")); // NOI18N
//        //checkWindowAccessibility.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.general.habilitar")); // NOI18N
//        this.checkWindowAccessibility.setSelected(GeneralConfig.isAvanzados()); 
//        this.checkWindowAccessibility.setBounds(12, 20, 340, 23);
//        this.checkWindowAccessibility.setMnemonic(KeyEvent.VK_B); // AsignaciÃ³n de mnemÃ³nico al checkbox
//        this.checkWindowAccessibility.setName("accesibilidadVentanas");
//        Utils.remarcar(this.checkWindowAccessibility);
//        Utils.setContrastColor(this.checkWindowAccessibility);
//        Utils.setFontBold(this.checkWindowAccessibility);
//        
//        panelWindowAccessibility.add(this.checkWindowAccessibility);
//        windowPanel.add(panelWindowAccessibility, c2);
        
        this.panel.add(windowPanel, c);   
        c.gridy = c.gridy + 1;
    	
    	// Panel Cursor
    	JPanel cursorPanel = new JPanel(new GridBagLayout());
    	cursorPanel.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.accesibilidad.cursor"))); // NOI18N
    	Utils.setContrastColor(cursorPanel);
    	Utils.setFontBold(cursorPanel);
    	
        c2.fill = GridBagConstraints.HORIZONTAL;
        c2.insets = new Insets(5, 13, 0, 13);
        c2.weightx = 1.0;
        c2.gridy = c2.gridy + 1;
        
        JPanel panelCursorSize = new JPanel(new GridLayout(1, 1));
        panelCursorSize.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.accesibilidad.cursor"));
        // Checkbox para habilitar la opcion de configuracion del tama&ntilde;o del cursor
        this.checkCursorSize = new JCheckBox();
        this.checkCursorSize.setText(Messages.getString("Opciones.accesibilidad.cursor.tamano")); // NOI18N
        //checkFontSize.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.general.habilitar")); // NOI18N
        this.checkCursorSize.setSelected(GeneralConfig.isAvanzados()); 
        this.checkCursorSize.setBounds(12, 20, 340, 23);
        this.checkCursorSize.setMnemonic(KeyEvent.VK_E); // AsignaciÃ³n de mnemÃ³nico al checkbox
        Utils.remarcar(this.checkCursorSize);
        Utils.setContrastColor(this.checkCursorSize);
        Utils.setFontBold(this.checkCursorSize);
        panelCursorSize.add(this.checkCursorSize);
        cursorPanel.add(panelCursorSize, c2);
        
        this.panel.add(cursorPanel, c);
        c.gridy = c.gridy + 1;
        
        //Botones
        JPanel buttonPanel = new JPanel( new FlowLayout(FlowLayout.RIGHT, 1, 1));
        
        //Definicion de botones
        final JButton valores = new JButton();
        //final JButton guardar = new JButton();
       
        
        JPanel panelValores = new JPanel(new GridLayout(1, 1));
        //Boton Valores por defecto
        valores.setText(Messages.getString("Opciones.accesibilidad.valores"));
        valores.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				valoresActionPerformed();
				
				
			}
		});
        valores.setMnemonic(KeyEvent.VK_O);
        Utils.remarcar(valores);
        Utils.setContrastColor(valores);
        Utils.setFontBold(valores);
        panelValores.add(valores);
        buttonPanel.add(panelValores);
        
//        JPanel panelGuardar = new JPanel(new GridLayout(1, 1));
//        //Boton guardar
//        guardar.setText(Messages.getString("Opciones.accesibilidad.guardar"));
//        guardar.setMnemonic(KeyEvent.VK_U);
//        guardar.addActionListener(new ActionListener() {
//			
//			@Override
//			public void actionPerformed(ActionEvent e) {
//				save();
//				
//			}
//		});
//        Utils.remarcar(guardar);
//        Utils.setContrastColor(guardar);
//        Utils.setFontBold(guardar);
//        
//        panelGuardar.add(guardar);
//        buttonPanel.add(panelGuardar);
        
        JPanel panelAplicar = new JPanel(new GridLayout(1, 1));
        //Boton aplicar
        aplicar.setText(Messages.getString("Opciones.accesibilidad.aplicar"));
        aplicar.setMnemonic(KeyEvent.VK_I);
        aplicar.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				aplicar();
				
			}
		});
        Utils.remarcar(aplicar);
        Utils.setContrastColor(aplicar);
        Utils.setFontBold(aplicar);
        
        panelAplicar.add(aplicar);
        buttonPanel.add(panelAplicar);

        this.panel.add(buttonPanel, c);
        // Rellenamos el hueco libre con un panel vacio
        c.gridy = c.gridy + 1;
        c.weighty = 1.0;
        this.panel.add(new JPanel(), c);
        
        //Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(this.checkFontSize, "accesibilidad.texto");
        HelpUtils.enableHelpKey(this.checkFontStyle, "accesibilidad.texto");
        HelpUtils.enableHelpKey(this.checkHighContrast, "accesibilidad.color");
        HelpUtils.enableHelpKey(this.checkFocusVisible, "accesibilidad.foco");
        HelpUtils.enableHelpKey(this.checkWindowSize, "accesibilidad.ventana");
        HelpUtils.enableHelpKey(this.checkCursorSize, "accesibilidad.cursor");
        HelpUtils.enableHelpKey(valores, "accesibilidad.defecto");
        HelpUtils.enableHelpKey(aplicar, "accesibilidad.aplicar");
	}

	/**
	 * Devuelve el panel de configuraciones.
	 * @return panel de configuraciones.
	 */
	public JPanel getConfigurationPanel() {
		return this.panel;
	}
	
	/**
	 * Introduce en un properties la configuraci&oacute;n establecida en el panel.
	 * @param config Configuraci&oacute;n para cargar en el panel.
	 */
	public void loadConfig(Properties config) {
		this.checkFontSize.setSelected(Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_FONT_SIZE, "false")));
		this.checkFontStyle.setSelected(Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_FONT_STYLE, "false")));
		this.checkHighContrast.setSelected(Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_HIGHT_CONTRAST, "false")));
		this.checkFocusVisible.setSelected(Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_FOCUS_VISIBLE, "false")));
		this.checkWindowSize.setSelected(Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_WINDOWS_SIZE, "false")));
//		this.checkWindowAccessibility.setSelected(Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_WINDOWS_ACCESSIBILITY, "true")));
		this.checkCursorSize.setSelected(Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_CURSOR_SIZE, "false")));

		// Comprobamos si esta activada al menos una de las opciones de accesibilidad sobre textos 
		if (Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_FONT_SIZE)) || Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_FONT_STYLE))){
    		isBigStyle = true;
    	}		
	}

	
	/**
	 * Recupera el estado actual del panel.
	 * return Relaci&oacute;n con toda la configuraci&oacute;n del panel.
	 */
	public Properties getConfig() {
		Properties config = new Properties();
		config.setProperty(AccessibilityOptionsPane.MAIN_FONT_SIZE, Boolean.toString(this.checkFontSize.isSelected()));
    	config.setProperty(AccessibilityOptionsPane.MAIN_FONT_STYLE, Boolean.toString(this.checkFontStyle.isSelected()));
    	config.setProperty(AccessibilityOptionsPane.MAIN_HIGHT_CONTRAST, Boolean.toString(this.checkHighContrast.isSelected()));
    	config.setProperty(AccessibilityOptionsPane.MAIN_FOCUS_VISIBLE, Boolean.toString(this.checkFocusVisible.isSelected()));
    	config.setProperty(AccessibilityOptionsPane.MAIN_WINDOWS_SIZE, Boolean.toString(this.checkWindowSize.isSelected()));
//    	config.setProperty(AccessibilityOptionsPane.MAIN_WINDOWS_ACCESSIBILITY, Boolean.toString(this.checkWindowAccessibility.isSelected()));
    	config.setProperty(AccessibilityOptionsPane.MAIN_CURSOR_SIZE, Boolean.toString(this.checkCursorSize.isSelected()));
    	
    	// Comprobamos si se han desactivados las dos opciones de accesibilidad sobre texto 
    	if (isBigStyle && (!this.checkFontSize.isSelected() && !this.checkFontStyle.isSelected())){
			continueBigStyle = true;
		}
    	
    	return config;
	}

	
	/**
	 * Aplica los valores por defecto.
	 */
	void valoresActionPerformed(){
		Opciones.setUpdate(true);
		restore(this.panel);
	}
	
	/**
	 * Aplica el estado por defecto de los componentes de la ventana 
	 */
	private void restore(JPanel panel){
		for (int i=0; i<panel.getComponentCount();i++){
			if (panel.getComponent(i) instanceof JCheckBox){
				if (((JCheckBox)panel.getComponent(i)).getName()!=null){
					((JCheckBox)panel.getComponent(i)).setSelected(true);
				} else {
					((JCheckBox)panel.getComponent(i)).setSelected(false);
				}
			} else if (panel.getComponent(i) instanceof JPanel){
				JPanel interiorPanel = (JPanel)panel.getComponent(i);
				restore(interiorPanel);
			}
		}
	}
	
	/**
	 * Aplica la configuraci&oacute;n de accesibilidad indicada en la pantalla
	 */
	private void aplicar(){
		// Guardamos la posicion y tamano actual de la ventana solo en caso de no estar maximizada por configuracion
    	if (!GeneralConfig.isMaximized()){
	    	PrincipalGUI.optionActualPositionX = this.parent.getX();
	    	PrincipalGUI.optionActualPositionY = this.parent.getY();
	    	PrincipalGUI.optionActualWidth = this.parent.getWidth();
	    	PrincipalGUI.optionActualHeight = this.parent.getHeight();
    	}
		((Opciones)this.parent).setAplicar(true);
		if (!isChangeHighContrast){
			//no se ha modificado el estado del Alto Contraste
			
			//se guarda el estado actual de la configuracion de la herramienta
			Properties config = new Properties();
			config.putAll(getConfig());
			config.putAll(((Opciones)this.parent).getMainOptions().getConfig());
        	config.putAll(((Opciones)this.parent).getContextOptions().getConfig());
	    	GeneralConfig.loadConfig(config);
	    	
	    	//aplicamos los cambios a la pantalla principal
			this.mainGui.crearPaneles();
			this.mainGui.generarMenuHerramientas();
			this.mainGui.generarMenuAccesibilidad();
			this.mainGui.generarMenuAyuda();
			
			//aplicamos los cambios a la pantalla de opciones
			((Opciones)this.parent).initComponents();
			((Opciones)this.parent).callResize();
		} else {
			// Se ha modificado el estado del Alto Contraste por lo que es necesario ocultar y volver a mostrar la ventana de opciones para que cargue el alto contraste
			
			((Opciones)this.parent).getAceptar().doClick();
			mainGui.setAplicar(true);
			((JMenuItem)mainGui.getMenu().getMenu(0).getMenuComponent(0)).doClick();			
		}
	}
}
