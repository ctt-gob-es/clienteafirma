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
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.JAccessibilityOptionPane;
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
	
	/** Casilla de verificacion del tama&ntilde;o del cursor de texto. */ 
	private JCheckBox checkCursorSize;	
	
	public boolean isBigStyle = false;
	
	public static boolean continueBigStyle = false;

	
	public AccessibilityOptionsPane(){
		this.panel = new JPanel(new GridBagLayout());
		initComponents();
	}
	
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
        final JButton guardar = new JButton();
        
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
        
        JPanel panelGuardar = new JPanel(new GridLayout(1, 1));
        //Boton guardar
        guardar.setText(Messages.getString("Opciones.accesibilidad.guardar"));
        guardar.setMnemonic(KeyEvent.VK_U);
        guardar.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				save();
				
			}
		});
        Utils.remarcar(guardar);
        Utils.setContrastColor(guardar);
        Utils.setFontBold(guardar);
        
        panelGuardar.add(guardar);
        buttonPanel.add(panelGuardar);

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
        HelpUtils.enableHelpKey(guardar, "accesibilidad.guardar");
	}
	
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
		this.checkCursorSize.setSelected(Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_CURSOR_SIZE, "false")));

		// Comprobamos si est� activada al menos una de las opciones de accesibilidad sobre textos 
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
	 * Desactiva la seleccion de todos los JcheckBox de la ventana 
	 */
	private void restore(JPanel panel){
		for (int i=0; i<panel.getComponentCount();i++){
			if (panel.getComponent(i) instanceof JCheckBox){
				((JCheckBox)panel.getComponent(i)).setSelected(false);
			} else if (panel.getComponent(i) instanceof JPanel){
				JPanel interiorPanel = (JPanel)panel.getComponent(i);
				restore(interiorPanel);
			}
		}
	}
	
	/**
	 * Guarda y/o modifica en el preferences la configuracion del usuario.
	 */
	private void save(){
		int user = 0;
		user = Integer.parseInt(Main.preferences.get("users", "0"));
		boolean exists = false;
		String name;
		if (user > 0){
			String text = "Nombre del perfil (debe ser una única palabra). Si el nombre ya existe será sobreescrita la configuración."+"<br>"+"Actualmente existen los siguientes perfiles: "+"<br>";
			for (int i = 0;i<user;i++){
				System.out.println(Main.preferences.get("user"+(i+1), "error"));
				text += Main.preferences.get("user"+(i+1), "error")+"<br>";
			}
			name = JAccessibilityOptionPane.showInputDialog(null, text, "Insercción de nombre de perfil de accesibilidad", JOptionPane.INFORMATION_MESSAGE);
		} else{
			name = JAccessibilityOptionPane.showInputDialog(null,"Nombre del perfil (debe ser una unica palabra).", "Inserccion de nombre de perfil de accesibilidad", JOptionPane.INFORMATION_MESSAGE);
		}
		if (name!=null){
			if (name.trim().length()!=0){
				if (Main.preferences.get("users","0").equals("0")){
					exists = true;
					Main.preferences.put("users","1");
					Main.preferences.put("user"+Main.preferences.get("users", "0"), name.trim());
				} else {
					
					user++;
					for (int i =0;i<user;i++){
						if (Main.preferences.get("user"+i, "0").equals(name.trim())){
							exists = true;
							Main.preferences.remove(name.trim()+".accesibility.fontBig");
							Main.preferences.remove(name.trim()+".accesibility.fontStyle");
							Main.preferences.remove(name.trim()+".accesibility.highContrast");
							Main.preferences.remove(name.trim()+".accesibility.focus");
							Main.preferences.remove(name.trim()+".accesibility.maximized");
							Main.preferences.remove(name.trim()+".accesibility.cursor");
						}
					}
				}
				if (!exists){
					exists=false;
					Main.preferences.put("users", String.valueOf(user));
					Main.preferences.put("user"+Main.preferences.get("users", "0"), name.trim());
				}
				Main.preferences.put(name.trim()+".accesibility.fontBig",String.valueOf(this.checkFontSize.isSelected()));
				Main.preferences.put(name.trim()+".accesibility.fontStyle",String.valueOf(this.checkFontStyle.isSelected()));
				Main.preferences.put(name.trim()+".accesibility.highContrast",String.valueOf(this.checkHighContrast.isSelected()));
				Main.preferences.put(name.trim()+".accesibility.focus",String.valueOf(this.checkFocusVisible.isSelected()));
				Main.preferences.put(name.trim()+".accesibility.maximized",String.valueOf(this.checkWindowSize.isSelected()));
				Main.preferences.put(name.trim()+".accesibility.cursor",String.valueOf(this.checkCursorSize.isSelected()));
			} else {
				JAccessibilityOptionPane.showMessageDialog(this.panel,"Debe introducir un nombre válido", "Error en el nombre", JOptionPane.ERROR_MESSAGE);
			}
		}
	}
}
