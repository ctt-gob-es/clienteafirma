package es.gob.afirma.ui.principal;

import static org.junit.Assert.*;

import java.awt.Component;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.ui.utils.Constants;
import es.gob.afirma.ui.utils.GeneralConfig;

/**
 * Testeo de accesibilidad para la clase MultifirmaSimple.
 * @author lmerayo
 *
 */
public class MultifirmaSimpleAccessibilityTest {

	/**
	 * Log.
	 */
	static Logger logger = Logger.getLogger(MultifirmaSimpleAccessibilityTest.class.getName());
	
	/**
	 * Comprobación de que el campo labelFor de las etiquetas no esté duplicado. 
	 */
	@Test
	public void testNotDuplicatedLabelForProperty() {
		logger.info("testNotDuplicatedLabelForProperty");

		//Instancia del panel que se va a analizar
		MultifirmaSimple multifirmaPanel = new MultifirmaSimple();
		//Lista de componentes asociados
		List <Component> componentList = new ArrayList<Component>();
		//Conjunto de componentes asociados
		Set <Component> componentSet = null;
		
		//Array de componentes
		Component[] components = multifirmaPanel.getComponents();
		//Se recorren los componentes del panel
		for (int i = 0; i< components.length; i++) {
			//Se comprueba si es una etiqueta
			if (components[i] instanceof JLabel) {
				JLabel label = (JLabel) components[i];
				Component component = label.getLabelFor();
				//Para este panel hasta el momento todas las etiquetas tienen asociado el campo labelFor
				assertNotNull(component);
				//Se añade a la lista el componente
				componentList.add(component);
			}
		}
		//Se crea un conjunto a partir de la lista para eliminar duplicados
		componentSet = new HashSet<Component>(componentList);
		//Si el tamaño de la lista y del conjunto no son iguales, no hay duplicados
		assertTrue(componentSet.size() == componentList.size());

	}

	/**
	 * Comprobación de que el campo Mnemocic de las etiquetas, botones y botones de radio
	 *  no esté duplicado. Modo Simple.
	 */
	@Test
	public void testNotDuplicatedDisplayedMnemonic_SimpleMode() {
		logger.info("testNotDuplicatedDisplayedMnemonic_SimpleMode");

		//Instancia del panel que se va a analizar
		MultifirmaSimple multifirmaPanel = new MultifirmaSimple();
		
		//Lista de mnemónicos
		List <Integer> keyCodes = new ArrayList<Integer>();
		//Conjunto de mnemónicos
		Set <Integer> keyCodesSet = null;
		
		//Se llama al método que obtiene una lista de códigos de atajos asociados a los componentes del panel
		getKeyCodeList (multifirmaPanel, keyCodes);

		//Se crea un conjunto a partir de la lista para eliminar duplicados
		keyCodesSet = new HashSet<Integer>(keyCodes);
		//Si el tamaño de la lista y del conjunto no son iguales, no hay duplicados
		assertTrue(keyCodesSet.size() == keyCodes.size());
	}

	/**
	 * Comprobación de que el campo Mnemocic de las etiquetas, botones y botones de radio
	 *  no esté duplicado. Modo Avanzado.
	 */
	@Test
	public void testNotDuplicatedDisplayedMnemonic_AdvancedMode() {
		logger.info("testNotDuplicatedDisplayedMnemonic_AdvancedMode");

		//Se obtiene la cofiguración general
		//Se añade el perfil por defecto
		UserProfile.currentUser=Constants.defaultUser;
		GeneralConfig.loadConfig(GeneralConfig.getConfig());
		Properties config = GeneralConfig.getConfig();
		
		//Se cambia al modo avanzado
		config.setProperty(MainOptionsPane.MAIN_ADVANCED_VIEW, "true");

		//Se asigna
		GeneralConfig.loadConfig(config);
				
		//Instancia del panel que se va a analizar
		MultifirmaSimple multifirmaPanel = new MultifirmaSimple();
		
		//Lista de mnemónicos
		List <Integer> keyCodes = new ArrayList<Integer>();
		//Conjunto de mnemónicos
		Set <Integer> keyCodesSet = null;
		
		//Se llama al método que obtiene una lista de códigos de atajos asociados a los componentes del panel
		getKeyCodeList (multifirmaPanel, keyCodes);

		//Se crea un conjunto a partir de la lista para eliminar duplicados
		keyCodesSet = new HashSet<Integer>(keyCodes);
		//Si el tamaño de la lista y del conjunto no son iguales, no hay duplicados
		assertTrue(keyCodesSet.size() == keyCodes.size());
	}
	
	/**
	 * Comprobación de que el campo nombre accesible para botones, combos y checks
	 * no esté vacío. 
	 */
	@Test
	public void testNotEmptyAccessibleName_SimpleMode() {
		logger.info("testNotEmptyAccessibleName_SimpleMode()");
		//Instancia del panel que se va a analizar
		MultifirmaSimple multifirmaSimplePanel = new MultifirmaSimple();
		//Se llama al método que comprueba que el nombre no sea vacío
		assertTrue(checkAccessibleName(multifirmaSimplePanel));
	}
	
	/**
	 * Comprobación de que el campo nombre accesible para botones, radioButtons, combos y checks
	 * no esté vacío. 
	 */
	@Test
	public void testNotEmptyAccessibleName_AdvancedMode() {
		logger.info("testNotEmptyAccessibleName_AdvancedMode()");
		//Se obtiene la cofiguración general
		//Se añade el perfil por defecto
		UserProfile.currentUser=Constants.defaultUser;
		GeneralConfig.loadConfig(GeneralConfig.getConfig());
		Properties config = GeneralConfig.getConfig();
		
		//Se cambia al modo avanzado
		config.setProperty(MainOptionsPane.MAIN_ADVANCED_VIEW, "true");
				
		//Instancia del panel que se va a analizar
		MultifirmaSimple multifirmaSimplePanel = new MultifirmaSimple();
		//Se llama al método que comprueba que el nombre no sea vacío
		assertTrue(checkAccessibleName(multifirmaSimplePanel));
	}
	
	/**
	 * Recorre el panel comprobando que todos sus componentes (botones, radioButtons, checks y combos)
	 * tienen un nombre accesible asignado.
	 * @param panel panel
	 * @return verdadero -> si los componentes tienen un nombre accesible asignado
	 * 		   falso -> si algún componente no tiene un nombre accesible asignado
	 */
	@Ignore
	private boolean checkAccessibleName(JPanel panel) {
		boolean result = true;
		//Array de componentes del panel
		Component[] components = panel.getComponents();
		for (int i = 0; i < components.length; i++) {
			//Se obtiene el componente
			Component component = panel.getComponent(i);
			if (!(component instanceof JPanel)) {
				if (component instanceof JButton) { //Se comprueba si es un botón
					JButton button = (JButton) component;
					if (button.getAccessibleContext().getAccessibleName().equalsIgnoreCase("")) {
						return false; //Si no tiene asignado un nombre accesible se sale del método
					}
				} else if (component instanceof JCheckBox) { //Se comprueba si es un checkBox
					JCheckBox checkBox = (JCheckBox) component;
					if (checkBox.getAccessibleContext().getAccessibleName().equalsIgnoreCase("")) {
						return false; //Si no tiene asignado un nombre accesible se sale del método
					}
				} else if (component instanceof JComboBox) { //Se comprueba si es un combo
					JComboBox comboBox = (JComboBox) component;
					if (comboBox.getAccessibleContext().getAccessibleName().equalsIgnoreCase("")) {
						return false; //Si no tiene asignado un nombre accesible se sale del método
					}
				} else if (component instanceof JRadioButton) { //Se comprueba si es un radioButton
					JRadioButton radioButton = (JRadioButton) component;
					if (radioButton.getAccessibleContext().getAccessibleName().equalsIgnoreCase("")) {
						return false; //Si no tiene asignado un nombre accesible se sale del método
					}
				} else if (component instanceof JTextField) { //Se comprueba si es un campo de texto
					JTextField textField = (JTextField) component;
					if (textField.getAccessibleContext().getAccessibleName().equalsIgnoreCase("")) {
						return false; //Si no tiene asignado un nombre accesible se sale del método
					}
				}
				
			} else {
				//Si es un panel se vuelve a llamar recursivamente al método
				result = checkAccessibleName((JPanel)component);
			}
		} //for
		return result;
	}

	/**
	 * Método que obtiene una lista de códigos de atajos a los componentes (Etiqueta, Botón, radio button) de un panel.
	 */
	@Ignore
	private void getKeyCodeList(JPanel panel, List <Integer> keyCodeList) {
		//Array de componentes del panel
		Component[] components = panel.getComponents();
		int keyCode = 0;
		for (int i = 0; i < components.length; i++) {
			//Se obtiene el componente
			Component component = panel.getComponent(i);
			if (!(component instanceof JPanel)) {
				//Se comprueba si es una etiqueta
				if (component instanceof JLabel) {
					JLabel label = (JLabel) component;
					//Se obtiene el código del atajo asociado
					keyCode = label.getDisplayedMnemonic();
					//Se añade a la lista si existe este código, es decir, si es distinto de 0
					if (keyCode != 0) {
						keyCodeList.add(new Integer(keyCode));
					}
				} else if (component instanceof JButton) { //Se comprueba si es un botón
					JButton button = (JButton) component;
					//Se obtiene el código del atajo asociado
					keyCode = button.getMnemonic();
					//Se añade a la lista si existe este código, es decir, si es distinto de 0
					if (keyCode != 0) {
						keyCodeList.add(new Integer(keyCode));
					}
				} else if (component instanceof JRadioButton) { //Se comprueba si es un botón de radio
					JRadioButton radioButton = (JRadioButton) component;
					//Se obtiene el código del atajo asociado
					keyCode = radioButton.getMnemonic();
					//Se añade a la lista si existe este código, es decir, si es distinto de 0
					if (keyCode != 0) {
						keyCodeList.add(new Integer(keyCode));
					}
				}
				
			} else {
				//Si es un panel se vuelve a llamar recursivamente al método
				getKeyCodeList((JPanel)component, keyCodeList);
			}
		} //for
	}//getKeyCodeList


}
