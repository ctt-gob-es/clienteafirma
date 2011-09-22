package es.gob.afirma.ui.principal;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.Properties;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JMenu;
import javax.swing.JPanel;

import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;

/**
 * Manejador de la configuraci&oacute;n de accesibilidad de la interfaz.
 * @author inteco
 *
 */
public class AccessibilityOptionsPane {
	
	/** Clave para la configuraci&oacute;n de tamaño de fuente. */
	public static final String MAIN_FONT_SIZE = "main.fontSize";
	
	/** Clave para la configuraci&oacute;n de estilo de fuente. */
	public static final String MAIN_FONT_STYLE = "main.fontStyle";
	
	/** Clave para la configuraci&oacute;n de vista en alto contraste. */
	public static final String MAIN_HIGHT_CONTRAST = "main.hightContrast";
	
	/** Clave para la configuraci&oacute;n de visibilidad del foco. */
	public static final String MAIN_FOCUS_VISIBLE = "main.focusVisible";
	
	/** Clave para la configuraci&oacute;n de tamaño de ventana. */
	public static final String MAIN_WINDOWS_SIZE = "main.windowsSize";
	
	/** Clave para la configuraci&oacute;n de tamaño del cursor de texto. */
	public static final String MAIN_CURSOR_SIZE = "main.cursorSize";
	
	/** Panel sobre el que se montan los componentes. */
	private final JPanel panel;
	
	/** Casilla de verificacion de tamaño de fuente grande. */ 
	private JCheckBox checkFontSize;
	 
	/** Casilla de verificacion de fuente en negrita. */ 
	private JCheckBox checkFontStyle;
	 
	/** Casilla de verificacion de alto contraste. */ 
	private JCheckBox checkHighContrast;
	
	/** Casilla de verificacion de la visibilidad del foco. */ 
	private JCheckBox checkFocusVisible;
	
	/** Casilla de verificacion del tamaño de las ventanas. */ 
	private JCheckBox checkWindowSize;
	
	/** Casilla de verificacion del tamaño del cursor de texto. */ 
	private JCheckBox checkCursorSize;
	
	/**	Estado de las opciones de accesibilidad. */
	private Boolean[] optionState;
	
	public AccessibilityOptionsPane(){
		panel = new JPanel(new GridBagLayout());
		initComponents();
	}
	
	private void initComponents(){
		panel.removeAll();
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
        
        // Checkbox para habilitar la opcion de configuracion del tamaño de fuente
        checkFontSize = new JCheckBox();
        checkFontSize.setText(Messages.getString("Opciones.accesibilidad.texto.tamaño")); // NOI18N
        //checkFontSize.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.general.habilitar")); // NOI18N
        checkFontSize.setSelected(GeneralConfig.isAvanzados()); 
        checkFontSize.setBounds(12, 20, 340, 23);
        checkFontSize.setMnemonic(KeyEvent.VK_D); // Asignación de mnemónico al checkbox
        if (GeneralConfig.isRemarked()){
        	Utils.remarcar(checkFontSize);
        }
        Utils.setContrastColor(checkFontSize);
        Utils.setFontBold(checkFontSize);
        textPanel.add(checkFontSize, c2);
        
        //Checkbox para habilitar la opcion de configuracion del estilo de fuente
        checkFontStyle = new JCheckBox();
        checkFontStyle.setText(Messages.getString("Opciones.accesibilidad.texto.estilo")); // NOI18N
        //checkFontStyle.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.general.habilitar")); // NOI18N
        checkFontStyle.setSelected(GeneralConfig.isAvanzados()); 
        checkFontStyle.setBounds(12, 20, 340, 23);
        checkFontStyle.setMnemonic(KeyEvent.VK_N); // Asignación de mnemónico al checkbox
        if (GeneralConfig.isRemarked()){
        	Utils.remarcar(checkFontStyle);
        }
        Utils.setContrastColor(checkFontStyle);
        Utils.setFontBold(checkFontStyle);
        textPanel.add(checkFontStyle, c2);
    	
    	panel.add(textPanel, c);
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
        
        // Checkbox para habilitar la opcion de configuracion de alto contraste
        checkHighContrast = new JCheckBox();
        checkHighContrast.setText(Messages.getString("Opciones.accesibilidad.color.contraste")); // NOI18N
        //checkFontSize.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.general.habilitar")); // NOI18N
        checkHighContrast.setSelected(GeneralConfig.isAvanzados()); 
        checkHighContrast.setBounds(12, 20, 340, 23);
        checkHighContrast.setMnemonic(KeyEvent.VK_L); // Asignación de mnemónico al checkbox
        if (GeneralConfig.isRemarked()){
        	Utils.remarcar(checkHighContrast);
        }
        Utils.setContrastColor(checkHighContrast);
        Utils.setFontBold(checkHighContrast);
        colorPanel.add(checkHighContrast, c2);
        
        panel.add(colorPanel, c);
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
        
        // Checkbox para habilitar la opcion de configuracion de la visibilidad del foco
        checkFocusVisible = new JCheckBox();
        checkFocusVisible.setText(Messages.getString("Opciones.accesibilidad.foco.remarcar")); // NOI18N
        //checkFontSize.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.general.habilitar")); // NOI18N
        checkFocusVisible.setSelected(GeneralConfig.isAvanzados()); 
        checkFocusVisible.setBounds(12, 20, 340, 23);
        checkFocusVisible.setMnemonic(KeyEvent.VK_F); // Asignación de mnemónico al checkbox
        if (GeneralConfig.isRemarked()){
        	Utils.remarcar(checkFocusVisible);
        }
        Utils.setContrastColor(checkFocusVisible);
        Utils.setFontBold(checkFocusVisible);
        focusPanel.add(checkFocusVisible, c2);
        
        panel.add(focusPanel, c);
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
        
        // Checkbox para habilitar la opcion de configuracion de ventanas maximizadas
        checkWindowSize = new JCheckBox();
        checkWindowSize.setText(Messages.getString("Opciones.accesibilidad.ventana.tamaño")); // NOI18N
        //checkFontSize.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.general.habilitar")); // NOI18N
        checkWindowSize.setSelected(GeneralConfig.isAvanzados()); 
        checkWindowSize.setBounds(12, 20, 340, 23);
        checkWindowSize.setMnemonic(KeyEvent.VK_V); // Asignación de mnemónico al checkbox
        if (GeneralConfig.isRemarked()){
        	Utils.remarcar(checkWindowSize);
        }
        Utils.setContrastColor(checkWindowSize);
        Utils.setFontBold(checkWindowSize);
        windowPanel.add(checkWindowSize, c2);
        
        panel.add(windowPanel, c);   
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
        
        // Checkbox para habilitar la opcion de configuracion del tamaño del cursor
        checkCursorSize = new JCheckBox();
        checkCursorSize.setText(Messages.getString("Opciones.accesibilidad.cursor.tamaño")); // NOI18N
        //checkFontSize.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.general.habilitar")); // NOI18N
        checkCursorSize.setSelected(GeneralConfig.isAvanzados()); 
        checkCursorSize.setBounds(12, 20, 340, 23);
        checkCursorSize.setMnemonic(KeyEvent.VK_E); // Asignación de mnemónico al checkbox
        if (GeneralConfig.isRemarked()){
        	Utils.remarcar(checkCursorSize);
        }
        Utils.setContrastColor(checkCursorSize);
        Utils.setFontBold(checkCursorSize);
        cursorPanel.add(checkCursorSize, c2);
        
        panel.add(cursorPanel, c);
        c.gridy = c.gridy + 1;
        
        //Botones
        JPanel buttonPanel = new JPanel( new FlowLayout(FlowLayout.RIGHT, 1, 1));
        
        //Definicion de botones
        final JButton valores = new JButton();
        final JButton guardar = new JButton();
        final JButton cargar = new JButton();
        
        //Boton Valores por defecto
        valores.setText("Valores por defecto");
        valores.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				valoresActionPerformed();
				
				
			}
		});
        valores.setMnemonic(KeyEvent.VK_O);
        if (GeneralConfig.isRemarked()){
        	Utils.remarcar(valores);
        }
        Utils.setContrastColor(valores);
        Utils.setFontBold(valores);
        buttonPanel.add(valores);
        
        //Boton guardar
        guardar.setText("Guardar");
        guardar.setMnemonic(KeyEvent.VK_U);
        if (GeneralConfig.isRemarked()){
        	Utils.remarcar(guardar);
        }
        Utils.setContrastColor(guardar);
        Utils.setFontBold(guardar);
        buttonPanel.add(guardar);
        
        //Boton cancelar
        cargar.setText("Cargar");
        cargar.setMnemonic(KeyEvent.VK_R);
        if (GeneralConfig.isRemarked()){
        	Utils.remarcar(cargar);
        }
        Utils.setContrastColor(cargar);
        Utils.setFontBold(cargar);
        buttonPanel.add(cargar);

        panel.add(buttonPanel, c);
        // Rellenamos el hueco libre con un panel vacio
        c.gridy = c.gridy + 1;
        c.weighty = 1.0;
        panel.add(new JPanel(), c);
	}
	
	public JPanel getConfigurationPanel() {
		return panel;
	}
	
	/**
	 * Introduce en un properties la configuraci&oacute;n establecida en el panel.
	 * @param config Configuraci&oacute;n para cargar en el panel.
	 */
	public void loadConfig(Properties config) {
		checkFontSize.setSelected(Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_FONT_SIZE, "false")));
		checkFontStyle.setSelected(Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_FONT_STYLE, "false")));
		checkHighContrast.setSelected(Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_HIGHT_CONTRAST, "false")));
		checkFocusVisible.setSelected(Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_FOCUS_VISIBLE, "false")));
		checkWindowSize.setSelected(Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_WINDOWS_SIZE, "false")));
		checkCursorSize.setSelected(Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_CURSOR_SIZE, "false")));
	}
	
	/**
	 * Recupera el estado actual del panel.
	 * return Relaci&oacute;n con toda la configuraci&oacute;n del panel.
	 */
	public Properties getConfig() {
		Properties config = new Properties();
		config.setProperty(AccessibilityOptionsPane.MAIN_FONT_SIZE, Boolean.toString(checkFontSize.isSelected()));
    	config.setProperty(AccessibilityOptionsPane.MAIN_FONT_STYLE, Boolean.toString(checkFontStyle.isSelected()));
    	config.setProperty(AccessibilityOptionsPane.MAIN_HIGHT_CONTRAST, Boolean.toString(checkHighContrast.isSelected()));
    	config.setProperty(AccessibilityOptionsPane.MAIN_FOCUS_VISIBLE, Boolean.toString(checkFocusVisible.isSelected()));
    	config.setProperty(AccessibilityOptionsPane.MAIN_WINDOWS_SIZE, Boolean.toString(checkWindowSize.isSelected()));
    	config.setProperty(AccessibilityOptionsPane.MAIN_CURSOR_SIZE, Boolean.toString(checkCursorSize.isSelected()));
    	
    	return config;
	}
	
	/**
	 * Aplica los valores por defecto.
	 */
	private void valoresActionPerformed(){
		Opciones.setUpdate(true);
		restore(panel);
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
}
