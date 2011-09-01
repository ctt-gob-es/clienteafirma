package es.gob.afirma.ui.principal;

import static org.junit.Assert.*;

import java.awt.Component;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.junit.Ignore;
import org.junit.Test;

/**
 * Testeo de accesibilidad para la clase Firma.
 * @author lmerayo
 *
 */
public class FirmaAccessibilityTest {

	/**
	 * Log.
	 */
	static Logger logger = Logger.getLogger(FirmaAccessibilityTest.class.getName());
	
	/**
	 * Comprobación de que el campo labelFor de las etiquetas no esté duplicado. 
	 */
	@Test
	public void testNotDuplicatedLabelForProperty() {
		logger.info("testNotDuplicatedLabelForProperty");

		//Instancia del panel que se va a analizar
		Firma firmaPanel = new Firma();
		//Lista de componentes asociados
		List <Component> componentList = new ArrayList<Component>();
		//Conjunto de componentes asociados
		Set <Component> componentSet = null;
		
		//Array de componentes
		Component[] components = firmaPanel.getComponents();
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
	 * Comprobación de que el campo Mnemocic de las etiquetas y botones no esté duplicado. 
	 */
	@Test
	public void testNotDuplicatedDisplayedMnemonic() {
		logger.info("testNotDuplicatedDisplayedMnemonic");

		//Instancia del panel que se va a analizar
		Firma firmaPanel = new Firma();
		//Lista de mnemónicos
		List <Integer> keyCodes = new ArrayList<Integer>();
		//Conjunto de mnemónicos
		Set <Integer> keyCodesSet = null;
		
		//Se llama al método que obtiene una lista de códigos de atajos asociados a los componentes del panel
		getKeyCodeList (firmaPanel, keyCodes);

		//Se crea un conjunto a partir de la lista para eliminar duplicados
		keyCodesSet = new HashSet<Integer>(keyCodes);
		//Si el tamaño de la lista y del conjunto no son iguales, no hay duplicados
		assertTrue(keyCodesSet.size() == keyCodes.size());
	}

	/**
	 * Método que obtiene una lista de códigos de atajos a los componentes (Etiqueta, Botón) de un panel.
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
				} else if (component instanceof JButton) {
					JButton button = (JButton) component;
					//Se obtiene el código del atajo asociado
					keyCode = button.getMnemonic();
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
