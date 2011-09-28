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
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.ui.utils.Constants;
import es.gob.afirma.ui.utils.GeneralConfig;

/**
 * Testeo de accesibilidad para la clase Cifrado.
 * @author lmerayo
 *
 */
public class CifradoAccessibilityTest {

	/**
	 * Log.
	 */
	static Logger logger = Logger.getLogger(CifradoAccessibilityTest.class.getName());
	
	/**
	 * Comprobación de que el campo labelFor de las etiquetas no esté duplicado. 
	 */
	@Test
	public void testNotDuplicatedLabelForProperty_SimpleMode() {
		logger.info("testNotDuplicatedLabelForProperty_SimpleMode");

		//Instancia del panel que se va a analizar
		Cifrado cifradoPanel = new Cifrado();
		//Lista de componentes asociados
		List <Component> componentList = new ArrayList<Component>();
		//Conjunto de componentes asociados
		Set <Component> componentSet = null;

		//Array de componentes
		Component[] components = cifradoPanel.getComponents();
		//Se recorren los componentes del panel
		for (int i = 0; i< components.length; i++) {
			//Se comprueba si es una etiqueta
			if (components[i] instanceof JLabel) {
				JLabel label = (JLabel) components[i];
				Component component = label.getLabelFor();
				//Para el modo simple puede haber etiquetas no asociadas a componentes
				//Si el componente es nulo se ignora
				if (component != null) {
					//Se añade a la lista el componente
					componentList.add(component);
				}
			} else if (components[i] instanceof JPanel) {
				getLabelForComponentList((JPanel)components[i],componentList);
			}
		}

		//Se crea un conjunto a partir de la lista para eliminar duplicados
		componentSet = new HashSet<Component>(componentList);
		//Si el tamaño de la lista y del conjunto no son iguales, no hay duplicados
		assertTrue(componentSet.size() == componentList.size());

	}
	
	/**
	 * Comprobación de que el campo labelFor de las etiquetas no esté duplicado. 
	 */
	@Test
	public void testNotDuplicatedLabelForProperty_AdvancedMode() {
		logger.info("testNotDuplicatedLabelForProperty_AdvancedMode");
		
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
		Cifrado cifradoPanel = new Cifrado();
		//Lista de componentes asociados
		List <Component> componentList = new ArrayList<Component>();
		//Conjunto de componentes asociados
		Set <Component> componentSet = null;

		
		//Array de componentes
		Component[] components = cifradoPanel.getComponents();
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
			} else if (components[i] instanceof JPanel) {
				getLabelForComponentList((JPanel)components[i],componentList);
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
		Cifrado cifradoPanel = new Cifrado();
		//Lista de mnemónicos
		List <Integer> keyCodes = new ArrayList<Integer>();
		//Conjunto de mnemónicos
		Set <Integer> keyCodesSet = null;
		
		//Se llama al método que obtiene una lista de códigos de atajos asociados a los componentes del panel
		getKeyCodeList (cifradoPanel, keyCodes);

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
	
	/**
	 * Método que obtiene la propiedad labelFor de las etiquetas de un panel.
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
					//Se añade a la lista si no es nulo
					if (labelForComponent != null) {
						componentList.add(labelForComponent);
					}
				}
				
			} else {
				//Si es un panel se vuelve a llamar recursivamente al método
				getLabelForComponentList((JPanel) component, componentList);
			}
		} //for
	}//getLabelForComponentList


}
