/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.utils;

import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.prefs.Preferences;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.JToggleButton;
import javax.swing.JTree;
import javax.swing.border.TitledBorder;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;

import es.gob.afirma.ui.principal.PrincipalGUI;

/**
 * Clase con utilidades varias
 */
public class Utils {
	
	/**
	 * Abre un fichero en la aplicaci\u00F3n predefinida por el sistema operativo actual.
	 * @param filepath Ruta completa al fichero.
	 */
	public static void openFile(String filepath){
		String os = System.getProperty("os.name").toLowerCase();
		Runtime rt = Runtime.getRuntime();
		try {
			if (os.indexOf( "win" ) >= 0) 
				rt.exec("cmd.exe /C \""+filepath+"\"");
			else if (os.indexOf( "mac" ) >= 0)
				rt.exec( "open " + filepath);
			else {
				//prioritized 'guess' of users' preference
				List<String> browsers = new ArrayList<String>(Arrays.asList("epiphany", "firefox", "mozilla", "konqueror",
						"netscape","opera","links","lynx"));

				StringBuffer cmd = new StringBuffer();
				for (String browser : browsers)
					cmd.append( (browsers.get(0).equals(browser)  ? "" : " || " ) + browser +" \"" + filepath + "\" ");

				rt.exec(new String[] { "sh", "-c", cmd.toString() });
			}
		} catch (IOException e) {
			e.printStackTrace();
			PrincipalGUI.setNuevoEstado(Messages.getString("Validacion.error.valide"));
		}
	}

	/**
     * Abre un fichero en la aplicaci\u00F3n predefinida por el sistema operativo actual.
     * @param filepath Ruta completa al fichero.
     */
    public static void openFile(File file){
        try {
            openFile(file.getCanonicalPath());
        } catch (Exception e) {
            openFile(file.getAbsolutePath());
        }
    }
	
	/**
	 * Método que devuelve un mnemónico válido para el lenguaje que recibe como parámetro.
	 * @param listMnemonic lista de mnemónicos que ya han sido utilizados para otros lenguajes.
	 * @param actualLanguage lenguaje para el que se está buscando un mnemónico
	 * @return mnemónico seleccionado o 0 en el caso de que no se haya encontrado ninguno disponible
	 */
	public static char getLanguageMnemonic(List<Character> mnemonicList, String actualLanguage){
		//Se recorren las letras del lenguaje actual
		for (int i=0; i< actualLanguage.length(); i++) {
			//Se lee el caracter correspondiente al índice i
			char caracter = actualLanguage.charAt(i);
			//Se comprueba si se ha utilizado
			if (!mnemonicList.contains(caracter)) {
				//se añade a la lista de caracteres utilizados
				mnemonicList.add(caracter);
				//Se devuelve
				return caracter;
			}
		}
		//TODO: mejorar para que en el caso de que no encuentre mnemónico pueda cambiar alguno de los anteriores
		return 0;
	}
	
	/**
	 * Configura el formato del remarcado del componente al ser seleccionado.
	 * @param component El componente seleccionado.
	 */
	public static void remarcar(JComponent component){
		if (GeneralConfig.isRemarked()){
			if (component instanceof JButton){
				final JButton button = (JButton) component;
				button.addFocusListener(new FocusListener() {
					public void focusLost(FocusEvent e) {
						((JPanel)button.getParent()).setBorder(BorderFactory.createEmptyBorder());
					}
					
					public void focusGained(FocusEvent e) {
						if (GeneralConfig.isHighContrast()){
							((JPanel)button.getParent()).setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
						} else {
							((JPanel)button.getParent()).setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
						}
					}
				});
			}
			if (component instanceof JToggleButton){
				final JToggleButton button = (JToggleButton) component;
				button.addFocusListener(new FocusListener() {
					public void focusLost(FocusEvent e) {
						((JPanel)button.getParent()).setBorder(BorderFactory.createEmptyBorder());
					}
					
					public void focusGained(FocusEvent e) {
						if (GeneralConfig.isHighContrast()){
							((JPanel)button.getParent()).setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
						} else {
							((JPanel)button.getParent()).setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
						}
					}
				});
			}		
			if (component instanceof JTextField){
				final JTextField textField = (JTextField) component;
				textField.addFocusListener(new FocusListener() {
					public void focusLost(FocusEvent e) {
						textField.setBorder(BorderFactory.createLineBorder(Color.GRAY, 1));
					}
					public void focusGained(FocusEvent e) {
						if (GeneralConfig.isHighContrast()){
							textField.setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
						} else {
							textField.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
						}
					}
				});
			}
			if (component instanceof JComboBox){
				final JComboBox comboBox = (JComboBox) component;
				comboBox.addFocusListener(new FocusListener() {
					public void focusLost(FocusEvent e) {
						comboBox.setBorder(BorderFactory.createLineBorder(Color.GRAY, 1));
					}
					
					public void focusGained(FocusEvent e) {
						if (GeneralConfig.isHighContrast()){
							comboBox.setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
						} else {
							comboBox.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
						}
					}
				});
			}
			if (component instanceof JRadioButton){
				final JRadioButton radioButton = (JRadioButton) component;
				radioButton.addFocusListener(new FocusListener() {
					public void focusLost(FocusEvent e) {
						((JPanel)radioButton.getParent()).setBorder(BorderFactory.createEmptyBorder());
					}
					
					public void focusGained(FocusEvent e) {
						if (GeneralConfig.isHighContrast()){
							((JPanel)radioButton.getParent()).setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
						} else {
							((JPanel)radioButton.getParent()).setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
						}
					}
				});
			}
			if (component instanceof JLabel){
				final JLabel label = (JLabel) component;
				label.addFocusListener(new FocusListener() {
					public void focusLost(FocusEvent e) {
						label.setBorder(BorderFactory.createEmptyBorder());
					}
					public void focusGained(FocusEvent e) {
						if (GeneralConfig.isHighContrast()){
							label.setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
						} else {
							label.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
						}
					}
				});
			}
			if (component instanceof JCheckBox){
				final JCheckBox checkBox = (JCheckBox) component;
				checkBox.addFocusListener(new FocusListener() {
					public void focusLost(FocusEvent e) {
						((JPanel)checkBox.getParent()).setBorder(BorderFactory.createEmptyBorder());
					}
					
					public void focusGained(FocusEvent e) {
						if (GeneralConfig.isHighContrast()){
							((JPanel)checkBox.getParent()).setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
						} else {
							((JPanel)checkBox.getParent()).setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
						}
					}
				});
			}
			if (component instanceof JTextPane){
				final JTextPane textPane = (JTextPane) component;
				textPane.addFocusListener(new FocusListener() {
					public void focusLost(FocusEvent e) {
						textPane.setBorder(BorderFactory.createEmptyBorder());
					}
					public void focusGained(FocusEvent e) {
						if (GeneralConfig.isHighContrast()){
							textPane.setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
						} else {
							textPane.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
						}
					}
				});
			}
			if (component instanceof JTree){
				final JTree tree = (JTree) component;
				tree.addFocusListener(new FocusListener() {
					public void focusLost(FocusEvent e) {
						tree.setBorder(BorderFactory.createEmptyBorder());
					}
					public void focusGained(FocusEvent e) {
						if (GeneralConfig.isHighContrast()){
							tree.setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
						} else {
							tree.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
						}
					}
				});
			}
			if (component instanceof JList){
				final JList list = (JList) component;
				list.addFocusListener(new FocusListener() {
					public void focusLost(FocusEvent e) {
						list.setBorder(BorderFactory.createEmptyBorder());
					}
					public void focusGained(FocusEvent e) {
						if (GeneralConfig.isHighContrast()){
							list.setBorder(BorderFactory.createLineBorder(Color.WHITE, 2));
						} else {
							list.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
						}
					}
				});
			}
			if (component instanceof JMenu){
				final JMenu menu = (JMenu) component;
				menu.addMenuListener(new MenuListener() {
					public void menuSelected(MenuEvent e) {
						menu.setFont(new Font(menu.getFont().getName(), menu.getFont().getStyle(), menu.getFont().getSize()+5));
					}
					public void menuDeselected(MenuEvent e) {
						menu.setFont(new Font(menu.getFont().getName(), menu.getFont().getStyle(), menu.getFont().getSize()-5));
					}
					public void menuCanceled(MenuEvent e) {
						
					}
				});
			}
		}
	}
	
	/**
	 * Configura el comportamiento de ciertos componentes en Alto Contraste
	 * @param component Componente al que aplicar el alto contraste
	 */
	public static void setContrastColor (JComponent component){
		if (GeneralConfig.isHighContrast()){
			if (component instanceof JComboBox || component instanceof JPasswordField || component instanceof JTextField){
				component.setBackground(Color.WHITE);
			} else if(component instanceof JCheckBox) {
				component.setForeground(Color.WHITE);
			} else if(component instanceof JTree){
				component.setForeground(Color.WHITE);
			} else if(component instanceof JList){
				component.setForeground(Color.BLACK);
			} else if(component instanceof JPanel){
				if (component.getBorder()!=null){
					if (component.getBorder().getClass().getName().equals("javax.swing.border.TitledBorder")){
						if (((TitledBorder)component.getBorder())!=null){
							((TitledBorder)component.getBorder()).setTitleColor(Color.WHITE);
						}
					}
				}
				component.setForeground(Color.WHITE);
				component.setBackground(Color.BLACK);
			} else if (component instanceof JStatusBar){
				((JLabel)component.getComponent(0)).setForeground(Color.WHITE);
			}			
			else{
				component.setForeground(Color.WHITE);
				component.setBackground(Color.BLACK);
			}
		} else {
			if (component instanceof JStatusBar){
				((JLabel)component.getComponent(0)).setForeground(Color.BLACK);
			}
		}
	}
		
	/**
	 * Aplica el estilo de fuente negrita
	 * @param component Componente al que aplicar el estilo de fuente negrita.
	 */
	public static void setFontBold(JComponent component){
		if (GeneralConfig.isFontBold()){
			if(component instanceof JPanel){
				if (component.getBorder()!=null){
					if (component.getBorder().getClass().getName().equals("javax.swing.border.TitledBorder")){
						if (((TitledBorder)component.getBorder())!=null){
							((TitledBorder)component.getBorder()).setTitleFont(new Font(component.getFont().getName(),Font.BOLD , component.getFont().getSize()));
						}
					}
				}
			}
			component.setFont(new Font(component.getFont().getName(),Font.BOLD , component.getFont().getSize()));
		}
	}

	 /** Recupera una de las preferencias establecidas para la aplicaci&oacute;n.
     * @param key
     *        Clave de la preferencia.
     * @param defaultValue
     *        Valor por defecto.
     * @return Devuelve el valor de la preferencia indicada o {@code defaultValue} si no est&aacute;a establecida. */
    public static String getPreference(final String key, final String defaultValue, Preferences preferences) {
        return preferences.get(key, defaultValue);
    }

    /** Establece una preferencia para la aplicaci&oacute;n.
     * @param key
     *        Clave de la preferencia.
     * @param value
     *        Valor asignado. */
    public static void setPreference(final String key, final String value, Preferences preferences) {
        preferences.put(key, value);
    }
		
}
