package es.gob.afirma.ui.utils;

import static org.junit.Assert.*;

import java.awt.Color;
import java.awt.Font;
import java.awt.Toolkit;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.JTree;
import javax.swing.JWindow;
import javax.swing.border.TitledBorder;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.ui.principal.AccessibilityOptionsPane;


/**
 * Testeo de funcionalidades relativas a la accesibilidad de la clase Utils
 * @author inteco
 *
 */
public class UtilsTest {

	/**
	 * Log.
	 */
	private static Logger LOGGER = Logger.getLogger(UtilsTest.class.getName());
	
//	/**
//	 * Comprobaci&oacute;n de que los componentes se remarcan al recibir el foco
//	 */
//	@Test	
//	public void testRemarcar(){
//		LOGGER.info("testRemarcar"); //$NON-NLS-1$
//		
//		
//		//Se obtiene la cofiguracion general
//		//Se anade el perfil por defecto
//		GeneralConfig.loadConfig(GeneralConfig.getConfig());
//		Properties config = GeneralConfig.getConfig();
//		//Se activa la opcion de remarcar elementos con foco
//		config.setProperty(AccessibilityOptionsPane.MAIN_FOCUS_VISIBLE, "true"); //$NON-NLS-1$
//		//Se asigna
//		GeneralConfig.loadConfig(config);
//		JTextField component = new JTextField();
//		
//		
//		
//		
//		Utils.remarcar(component);
//		
//		//component.addAncestorListener(new RequestFocusListener(false));
//		component.requestFocus();
//		
//		//component.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
//		//System.out.println(component.getBorder().getClass().getName());
////		assertTrue(remarcarJTextField(textField));
//		
//		
//	}
//	
//	/**
//	 * 
//	 * @param component
//	 * @return
//	 */
//	@Ignore
//	private boolean remarcarJTextField(JTextField component){
//		//JPanel panel = new JPanel();
//		component.addAncestorListener(new RequestFocusListener(false));
//		//panel.add(component);
//		Utils.remarcar(component);
//		//component.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
//		System.out.println(component.getBorder().getClass().getName());
////		if (component.getBorder()){
////			
////		}
//		return false;
//	}
	
	/**
	 * Comprobaci&oacute;n de la asignaci&oacute;n autom&aacute;tica de mnem&oacute;nicos
	 */
	@Test	
	public void testGetLanguageMnemonic(){
		LOGGER.info("testGetLanguageMnemonic"); //$NON-NLS-1$		
		
		try{
			// Lista de mnemonicos usados
	        final List<Character> mnemonicList = new ArrayList<Character>();
	        mnemonicList.add('a');
	        mnemonicList.add('b');
	        mnemonicList.add('c');
	        
	        // Nombre del texto al que asignar mnemonico
	        String nombreConLetraLibre = new String("abcd");
	        String nombreSinLetraLibre = new String("abc");		
			
			assertTrue(languageMnemonicSucces(mnemonicList, nombreConLetraLibre));
			assertTrue(languageMnemonicFail(mnemonicList, nombreSinLetraLibre));
		} catch (final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}
	}
	
	/**
	 * Comprobaci&oacute;n de que los componentes cambian al modo en alto contraste correctamente
	 */
	@Test	
	public void testSetContrastColor(){
		LOGGER.info("testSetContrastColor"); //$NON-NLS-1$		
		
		try {
			//Se obtiene la cofiguracion general
			GeneralConfig.loadConfig(GeneralConfig.getConfig());
			Properties config = GeneralConfig.getConfig();
			//Se activa la opcion de alto contraste
			config.setProperty(AccessibilityOptionsPane.MAIN_HIGHT_CONTRAST, "true"); //$NON-NLS-1$
			//Se asigna
			GeneralConfig.loadConfig(config);
			//Se crean los componentes para testear la correcta aplicacion del alto contraste
			JComboBox jComboBox = new JComboBox();
			assertTrue(highContrastJComboBox(jComboBox));
			JPasswordField jPasswordField = new JPasswordField();
			assertTrue(highContrastJPasswordField(jPasswordField));
			JTextField jTextField = new JTextField();
			assertTrue(highContrastJTextField(jTextField));
			JTree jTree = new JTree();
			assertTrue(highContrastJTree(jTree));
			JList jList = new JList();
			assertTrue(highContrastJList(jList));
			JPanel jPanel = new JPanel();
			jPanel.setBorder(BorderFactory.createTitledBorder("Test")); // NOI18N //$NON-NLS-1$
			assertTrue(highContrastJPanel(jPanel));
			JStatusBar jStatusBar = new JStatusBar();
			jStatusBar.setLabelWidth((int) Toolkit.getDefaultToolkit().getScreenSize().getWidth());
			jStatusBar.setStatus("Test"); //$NON-NLS-1$
			assertTrue(highContrastJStatusBar(jStatusBar));
			JEditorPane jEditorPane = new JEditorPane();
			assertTrue(highContrastJEditorPane(jEditorPane));
		} catch (final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}
	}
	
	/**
	 * Comprobaci&oacute;n de que los componentes cambian al estilo negrita correctamente
	 */
	@Test	
	public void testSetFontBold(){
		LOGGER.info("testSetFontBold"); //$NON-NLS-1$		
		
		try {
			//Se obtiene la cofiguracion general
			GeneralConfig.loadConfig(GeneralConfig.getConfig());
			Properties config = GeneralConfig.getConfig();
			//Se activa la opcion de fuente negrita
			config.setProperty(AccessibilityOptionsPane.MAIN_FONT_STYLE, "true"); //$NON-NLS-1$
			//Se asigna
			GeneralConfig.loadConfig(config);
			//Se crean los componentes para testear la correcta aplicacion del estilo de fuente en negrita
			JComboBox jComboBox = new JComboBox();
			assertTrue(fontBoldJComboBox(jComboBox));
			
			JPanel jPanel = new JPanel();
			jPanel.setBorder(BorderFactory.createTitledBorder("Test")); // NOI18N //$NON-NLS-1$
			assertTrue(fontBoldJPanel(jPanel));
	
			JToolBar jToolBar = new JToolBar();
			JButton jButton = new JButton("Test");
			jToolBar.add(jButton);
			assertTrue(fontBoldJToolBar(jToolBar));
		} catch (final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}
		
	}
	
	/**
	 * Comprobaci&oacute;n del correcto subrayado de los mnem&oacute;nicos para texto HTML
	 */
	@Test
	public void testRemarkMnemonic(){
		LOGGER.info("testRemarkMnemonic"); //$NON-NLS-1$
		
		try {
			assertTrue(Utils.remarkMnemonic("Test", 'T').equals("<u>T</u>est"));
			assertTrue(Utils.remarkMnemonic("Test", 'a').equals("Test"));
		} catch (final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}
	}
	
	/**
	 * Comprobaci&oacute;n del correcto visionado u ocultaci&oacute;n del tool tip
	 */
	@Test
	public void testShowToolTip(){
		LOGGER.info("testShowToolTip"); //$NON-NLS-1$
		try {
			boolean show;
			JWindow tip = new JWindow();
			JButton boton = new JButton();
			JLabel tipText = new JLabel();
			
			show = true;
			Utils.showToolTip(show, tip, boton, tipText);
			assertTrue(tip.isVisible()==true);
			show = false;
			Utils.showToolTip(show, tip, boton, tipText);
			assertTrue(tip.isVisible()==false);
		} catch (final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}
	}
	
	/**
	 * Comprueba si el componente aplica el alto contraste correctamente
	 * @param jComboBox Componente del tipo JComboBox para el que se comprueba el cambio de color
	 * @return Boolean indicando si pasa la validaci&oacute;n
	 */
	@Ignore
	private boolean highContrastJComboBox(JComboBox jComboBox){
		Utils.setContrastColor(jComboBox);
		if (jComboBox.getBackground()==Color.WHITE){
			return true;
		}		
		return false;
	}
	
	/**
	 * Comprueba si el componente aplica el alto contraste correctamente
	 * @param jPasswordField Componente del tipo JPasswordField para el que se comprueba el cambio de color
	 * @return Boolean indicando si pasa la validaci&oacute;n
	 */
	@Ignore
	private boolean highContrastJPasswordField(JPasswordField jPasswordField){
		Utils.setContrastColor(jPasswordField);
		if (jPasswordField.getBackground()==Color.WHITE){
			return true;
		}		
		return false;
	}
	
	/**
	 * Comprueba si el componente aplica el alto contraste correctamente
	 * @param jTextField Componente del tipo JTextField para el que se comprueba el cambio de color
	 * @return Boolean indicando si pasa la validaci&oacute;n
	 */
	@Ignore
	private boolean highContrastJTextField(JTextField jTextField){
		Utils.setContrastColor(jTextField);
		if (jTextField.getBackground()==Color.WHITE){
			return true;
		}		
		return false;
	}
	
	/**
	 * Comprueba si el componente aplica el alto contraste correctamente
	 * @param jTree Componente del tipo JTree para el que se comprueba el cambio de color
	 * @return Boolean indicando si pasa la validaci&oacute;n
	 */
	@Ignore
	private boolean highContrastJTree(JTree jTree){
		Utils.setContrastColor(jTree);
		if (jTree.getForeground()==Color.WHITE){
			return true;
		}		
		return false;
	}
	
	/**
	 * Comprueba si el componente aplica el alto contraste correctamente
	 * @param jList Componente del tipo JList para el que se comprueba el cambio de color
	 * @return Boolean indicando si pasa la validaci&oacute;n
	 */
	@Ignore
	private boolean highContrastJList(JList jList){
		Utils.setContrastColor(jList);
		if (jList.getForeground()==Color.BLACK){
			return true;
		}		
		return false;
	}
	
	/**
	 * Comprueba si el componente aplica el alto contraste correctamente
	 * @param jPanel Componente del tipo JPanel para el que se comprueba el cambio de color
	 * @return Boolean indicando si pasa la validaci&oacute;n
	 */
	@Ignore
	private boolean highContrastJPanel(JPanel jPanel){
		Utils.setContrastColor(jPanel);
		if (jPanel.getBorder() != null){
			if (jPanel.getBorder().getClass().getName().equals("javax.swing.border.TitledBorder")){ //$NON-NLS-1$
				if (((TitledBorder)jPanel.getBorder())!=null){
					if (((TitledBorder)jPanel.getBorder()).getTitleColor()!=Color.WHITE){
						return false;
					}
				}
			}
		}
		if (jPanel.getForeground()==Color.WHITE && jPanel.getBackground()==Color.BLACK){
			return true;
		}		
		return false;
	}
	
	/**
	 * Comprueba si el componente aplica el alto contraste correctamente
	 * @param jStatusBar Componente del tipo JStatusBar para el que se comprueba el cambio de color
	 * @return Boolean indicando si pasa la validaci&oacute;n
	 */
	@Ignore
	private boolean highContrastJStatusBar(JStatusBar jStatusBar){
		Utils.setContrastColor(jStatusBar);
		if (((JLabel)jStatusBar.getComponent(0)).getForeground()==Color.WHITE){
			return true;
		}		
		return false;
	}
	
	/**
	 * Comprueba si el componente aplica el alto contraste correctamente
	 * @param jEditorPane Componente del tipo JEditorPane para el que se comprueba el cambio de color
	 * @return Boolean indicando si pasa la validaci&oacute;n
	 */
	@Ignore
	private boolean highContrastJEditorPane(JEditorPane jEditorPane){
		Utils.setContrastColor(jEditorPane);
		if (jEditorPane.getBackground()==Color.BLACK){
			return true;
		}		
		return false;
	}
	
	/**
	 * Comprueba si el componente aplica el estilo de fuente negrita correctamente
	 * @param jComboBox Componente del tipo JComboBox para el que se comprueba el cambio de estilo de fuente
	 * @return Boolean indicando si pasa la validaci&oacute;n
	 */
	@Ignore
	private boolean fontBoldJComboBox(JComboBox jComboBox){
		Utils.setFontBold(jComboBox);
		if (jComboBox.getFont().getStyle() == Font.BOLD){
			return true;
		}		
		return false;
	}
	
	/**
	 * Comprueba si el componente aplica el estilo de fuente negrita correctamente
	 * @param jPanel Componente del tipo JPanel para el que se comprueba el cambio de estilo de fuente
	 * @return Boolean indicando si pasa la validaci&oacute;n
	 */
	@Ignore
	private boolean fontBoldJPanel(JPanel jPanel){
		Utils.setFontBold(jPanel);
		if (jPanel.getBorder() != null){
			if (jPanel.getBorder().getClass().getName().equals("javax.swing.border.TitledBorder")){ //$NON-NLS-1$
				if (((TitledBorder)jPanel.getBorder())!=null){
					if (((TitledBorder)jPanel.getBorder()).getTitleFont().getStyle() == Font.BOLD){
						return true;
					}
				}
			}
		}
		return false;
	}
	
	/**
	 * Comprueba si el componente aplica el estilo de fuente negrita correctamente
	 * @param jComboBox Componente del tipo JComboBox para el que se comprueba el cambio de estilo de fuente
	 * @return Boolean indicando si pasa la validaci&oacute;n
	 */
	@Ignore
	private boolean fontBoldJToolBar(JToolBar jToolBar){
		Utils.setFontBold(jToolBar);
		for (int i=0;i<jToolBar.getComponentCount();i++){
			if (jToolBar.getComponent(i).getFont().getStyle() == Font.BOLD){
				return true;
			}
		}	
		return false;
	}
	
	/**
	 * Comprueba si el gestor de mnem&oacute;nicos libres asigna correctamente un mnem&oacute;nico
	 * @param mnemonicList Lista con los mnem&oacute;nicos empleados
	 * @param nombreConLetraLibre Nombre con una letra disponible a la que asignar mnem&oacute;nicos
	 * @return Boolean indicando si pasa la validaci&oacute;n
	 */
	@Ignore
	private boolean languageMnemonicSucces(List<Character> mnemonicList, String nombreConLetraLibre){
		if (Utils.getLanguageMnemonic(mnemonicList, nombreConLetraLibre) == 'd'){
			return true;
		}
		return false;
	}
	
	/**
	 * Comprueba si el gestor de mnem&oacute;nicos libres indica que no hay un mnem&oacute;nico disponible
	 * @param mnemonicList Lista con los mnem&oacute;nicos empleados
	 * @param nombreConLetraLibre Nombre que no tiene letra disponible a la que asignar mnem&oacute;nicos
	 * @return Boolean indicando si pasa la validaci&oacute;n
	 */
	@Ignore
	private boolean languageMnemonicFail(List<Character> mnemonicList, String nombreConLetraLibre){
		if (Utils.getLanguageMnemonic(mnemonicList, nombreConLetraLibre) == 0){
			return true;
		}
		return false;
	}
}
