package es.gob.afirma.ui.principal;

import static org.junit.Assert.*;

import java.awt.Component;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
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

/**
 * Testeo de accesibilidad para la clase MainOptionsPane.
 * @author lmerayo
 *
 */
public class MainOptionsPaneAccessibilityTest {

	/**
	 * Log.
	 */
	static Logger logger = Logger.getLogger(MainOptionsPaneAccessibilityTest.class.getName());
	
	/**
	 * Comprobacion de que el campo labelFor de las etiquetas no este duplicado. 
	 */
	@Test
	public void testNotDuplicatedLabelForProperty() {
		logger.info("testNotDuplicatedLabelForProperty"); //$NON-NLS-1$

		//Instancia de la clase que se va a analizar
		MainOptionsPane mainOptionsPane = new MainOptionsPane();
		//Lista de componentes asociados
		List <Component> componentList = new ArrayList<Component>();
		//Conjunto de componentes asociados
		Set <Component> componentSet = null;
		
		//Se obtiene el panel principal
		JPanel mainPanel = mainOptionsPane.getConfigurationPanel();
		//Se llama a un metodo que selecciona todos los checkbox, porque asi se habilitan
		//todos los componentes que estuvieran deshabilitados
		activateAllCheckBox(mainPanel);
		
		//Se llama al metodo que obtiene una lista de componentes asociados a la propiedad labelFor del panel
		getLabelForComponentList (mainPanel, componentList);

		//Se crea un conjunto a partir de la lista para eliminar duplicados
		componentSet = new HashSet<Component>(componentList);
		//Si el tamano de la lista y del conjunto no son iguales, no hay duplicados
		assertTrue(componentSet.size() == componentList.size());

	}
	
	/**
	 * Comprobacion de que el campo Mnemocic de las etiquetas, botones y checkbox no este duplicado. 
	 */
	@Test
	public void testNotDuplicatedDisplayedMnemonic() {
		logger.info("testNotDuplicatedDisplayedMnemonic"); //$NON-NLS-1$

		//Instancia de la clase que se va a analizar
		MainOptionsPane mainOptionsPane = new MainOptionsPane();
		//Lista de mnemonicos
		List <Integer> keyCodes = new ArrayList<Integer>();
		//Conjunto de mnemonicos
		Set <Integer> keyCodesSet = null;
		
		//Se obtiene el panel principal
		JPanel mainPanel = mainOptionsPane.getConfigurationPanel();
		
		//Se llama a un metodo que selecciona todos los checkbox, porque asi se habilitan
		//todos los componentes que estuvieran deshabilitados
		activateAllCheckBox(mainPanel);
		
		//Se llama al metodo que obtiene una lista de codigos de atajos asociados a los componentes del panel
		getKeyCodeList (mainPanel, keyCodes);
		
		//Se crea un conjunto a partir de la lista para eliminar duplicados
		keyCodesSet = new HashSet<Integer>(keyCodes);
		//Si el tamano de la lista y del conjunto no son iguales, no hay duplicados
		assertTrue(keyCodesSet.size() == keyCodes.size());
	}
	
	/**
	 * Comprobacion de que el campo nombre accesible para botones, radiobuttons, combos y checks
	 * no este vacio. 
	 */
	@Test
	public void testNotEmptyAccessibleName() {
		logger.info("testNotEmptyAccessibleName"); //$NON-NLS-1$
		//Instancia del panel que se va a analizar
		MainOptionsPane mainOptionsPane = new MainOptionsPane();
		//Se obtiene el panel principal
		JPanel mainPanel = mainOptionsPane.getConfigurationPanel();
		//Se llama al metodo que comprueba que el nombre no sea vacio
		assertTrue(checkAccessibleName(mainPanel));
	}
	
	
	/**
	 * Recorre el panel comprobando que todos sus componentes (botones, radioButtons, checks y combos)
	 * tienen un nombre accesible asignado.
	 * @param panel panel
	 * @return verdadero -> si los componentes tienen un nombre accesible asignado
	 * 		   falso -> si algun componente no tiene un nombre accesible asignado
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
				if (component instanceof JButton) { //Se comprueba si es un boton
					JButton button = (JButton) component;
					if (button.getAccessibleContext().getAccessibleName().equalsIgnoreCase("")) { //$NON-NLS-1$
						return false; //Si no tiene asignado un nombre accesible se sale del metodo
					}
				} else if (component instanceof JCheckBox) { //Se comprueba si es un checkBox
					JCheckBox checkBox = (JCheckBox) component;
					if (checkBox.getAccessibleContext().getAccessibleName().equalsIgnoreCase("")) { //$NON-NLS-1$
						return false; //Si no tiene asignado un nombre accesible se sale del metodo
					}
				} else if (component instanceof JComboBox) { //Se comprueba si es un combo
					JComboBox comboBox = (JComboBox) component;
					if (comboBox.getAccessibleContext().getAccessibleName().equalsIgnoreCase("")) { //$NON-NLS-1$
						return false; //Si no tiene asignado un nombre accesible se sale del metodo
					}
				} else if (component instanceof JRadioButton) { //Se comprueba si es un radioButton
					JRadioButton radioButton = (JRadioButton) component;
					if (radioButton.getAccessibleContext().getAccessibleName().equalsIgnoreCase("")) { //$NON-NLS-1$
						return false; //Si no tiene asignado un nombre accesible se sale del metodo
					}
				} else if (component instanceof JTextField) { //Se comprueba si es un campo de texto
					JTextField textField = (JTextField) component;
					if (textField.getAccessibleContext().getAccessibleName().equalsIgnoreCase("")) { //$NON-NLS-1$
						return false; //Si no tiene asignado un nombre accesible se sale del metodo
					}
				}
				
			} else {
				//Si es un panel se vuelve a llamar recursivamente al metodo
				result = checkAccessibleName((JPanel)component);
			}
		} //for
		return result;
	}
	
	/**
	 * Metodo que obtiene una lista de codigos de atajos a los botones, etiquetas y checkbox de un panel.
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
					//Se obtiene el codigo del atajo asociado
					keyCode = label.getDisplayedMnemonic();
					//Se anade a la lista si existe este codigo, es decir, si es distinto de 0
					if (keyCode != 0) {
						keyCodeList.add(new Integer(keyCode));
					}
				} else if (component instanceof JButton) { //Se comprueba si es un boton
					JButton button = (JButton) component;
					//Se obtiene el codigo del atajo asociado
					keyCode = button.getMnemonic();
					//Se anade a la lista si existe este codigo, es decir, si es distinto de 0
					if (keyCode != 0) {
						keyCodeList.add(new Integer(keyCode));
					}
				} else if (component instanceof JCheckBox) { //Se comprueba si es un checkbox
					JCheckBox checkBox = (JCheckBox) component;
					//Se obtiene el codigo del atajo asociado
					keyCode = checkBox.getMnemonic();
					//Se anade a la lista si existe este codigo, es decir, si es distinto de 0
					if (keyCode != 0) {
						keyCodeList.add(new Integer(keyCode));
					}
				}
				
			} else {
				//Si es un panel se vuelve a llamar recursivamente al metodo
				getKeyCodeList((JPanel) component, keyCodeList);
			}
		} //for
	}//getKeyCodeList

	/**
	 * Metodo que activa todos los checkBox de un panel.
	 * @param panel panel
	 */
	@Ignore
	private void activateAllCheckBox(JPanel panel) {
		//Array de componentes del panel
		Component[] components = panel.getComponents();
		for (int i = 0; i < components.length; i++) {
			//Se obtiene el componente
			Component component = panel.getComponent(i);
			if (!(component instanceof JPanel)) {
				//Se comprueba si es una etiqueta
				if (component instanceof JCheckBox) { //Se comprueba si es un checkbox
					JCheckBox checkBox = (JCheckBox) component;
					//Se comprueba si esta seleccionado
					if (!checkBox.isSelected()) {
						checkBox.setSelected(true); //Se selecciona
					}
				}
				
			} else {
				//Si es un panel se vuelve a llamar recursivamente al metodo
				activateAllCheckBox((JPanel) component);
			}
		} //for
	}
	
	/**
	 * Metodo que obtiene la propiedad labelFor de las etiquetas de un panel.
	 */
	@Ignore
	private void getLabelForComponentList(JPanel panel, List <Component> componentList) {
		//Array de componentes del panel
		Component[] components = panel.getComponents();
		Component labelForComponent = null;
		for (int i = 0; i < components.length; i++) {
			//Se obtiene el componente
			Component component = panel.getComponent(i);
			if (!(component instanceof JPanel)) {
				//Se comprueba si es una etiqueta
				if (component instanceof JLabel) {
					JLabel label = (JLabel) component;
					//Se obtiene el componente asociado a la propiedad labelFor
					labelForComponent = label.getLabelFor();
					//Se anade a la lista si no es nulo
					if (labelForComponent != null) {
						componentList.add(labelForComponent);
					}
				}
				
			} else {
				//Si es un panel se vuelve a llamar recursivamente al metodo
				getLabelForComponentList((JPanel) component, componentList);
			}
		} //for
	}//getLabelForComponentList
}
