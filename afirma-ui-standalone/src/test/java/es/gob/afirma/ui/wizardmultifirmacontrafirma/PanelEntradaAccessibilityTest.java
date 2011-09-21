package es.gob.afirma.ui.wizardmultifirmacontrafirma;

import static org.junit.Assert.*;

import java.awt.Component;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.JPanel;
import javax.swing.JRootPane;

import org.junit.Ignore;
import org.junit.Test;


/**
 * Testeo de accesibilidad para la clase PanelEntrada.
 * @author lmerayo
 *
 */
public class PanelEntradaAccessibilityTest {

	/**
	 * Log.
	 */
	static Logger logger = Logger.getLogger(PanelEntradaAccessibilityTest.class.getName());
	
	/**
	 * Comprobación de que el campo labelFor de las etiquetas no esté duplicado. 
	 */
	@Test
	public void testNotDuplicatedLabelForProperty() {
		logger.info("testNotDuplicatedLabelForProperty");

		//Instancia del panel que se va a analizar
		PanelEntrada panelEntrada = new PanelEntrada( null);
		//Lista de componentes asociados
		List <Component> componentList = new ArrayList<Component>();
		//Conjunto de componentes asociados
		Set <Component> componentSet = null;

		//Componentes del wizard
		Component[] components = panelEntrada.getComponents();
		
		//Se recorren
		for (int i = 0; i< components.length; i++) {
			Component componentWizard = components[i];
			//Se trata el panel principal del wizard
			if (componentWizard instanceof JRootPane) {
				//Se obtienen los componentes del panel principal
				Component[] componentsRootPane = ((JRootPane)componentWizard).getComponents();
				//Se recorren
				for (int j = 0; j< componentsRootPane.length; j++) {
					//Se obtienen un elemento
					Component componentRootPane = componentsRootPane[j];
					//Si es un panel se trata
					if (componentRootPane instanceof JPanel) {
						//Se llama al método que obtiene una lista de componentes asociados a la propiedad labelFor del panel
						getLabelForComponentList ((JPanel) componentRootPane, componentList);
						
					} else if (componentRootPane instanceof JLayeredPane) { //Si es un layeredPane se obtienen sus componentes
						Component[] componentsLayeredPane = ((JLayeredPane) componentRootPane).getComponents();
						//Se recorren
						for (int z = 0; z< componentsLayeredPane.length; z++) {
							//Se obtienen un elemento
							Component componentLayeredPane = componentsLayeredPane[z];
							//Si es instancia de JPanel se trata
							if (componentLayeredPane instanceof JPanel) {

								//Se llama al método que obtiene una lista de componentes asociados a la propiedad labelFor del panel
								getLabelForComponentList ((JPanel) componentLayeredPane, componentList);
							}
						}
					}
				}
			}
		}
		//Se crea un conjunto a partir de la lista para eliminar duplicados
		componentSet = new HashSet<Component>(componentList);
		//Si el tamaño de la lista y del conjunto no son iguales, no hay duplicados
		assertTrue(componentSet.size() == componentList.size());

	}

	/**
	 * Comprobación de que el campo Mnemocic de las etiquetas,botones y checkbox no estén duplicados. 
	 */
	@Test
	public void testNotDuplicatedDisplayedMnemonic() {
		logger.info("testNotDuplicatedDisplayedMnemonic");

		//Instancia del panel que se va a analizar
		PanelEntrada panelEntrada = new PanelEntrada( null);

		//Lista de mnemónicos
		List <Integer> keyCodes = new ArrayList<Integer>();
		//Conjunto de mnemónicos
		Set <Integer> keyCodesSet = null;
		
		//Componentes del wizard
		Component[] components = panelEntrada.getComponents();
		
		//Se recorren
		for (int i = 0; i< components.length; i++) {
			Component componentWizard = components[i];
			//Se trata el panel principal del wizard
			if (componentWizard instanceof JRootPane) {
				//Se obtienen los componentes del panel principal
				Component[] componentsRootPane = ((JRootPane)componentWizard).getComponents();
				//Se recorren
				for (int j = 0; j< componentsRootPane.length; j++) {
					//Se obtienen un elemento
					Component componentRootPane = componentsRootPane[j];
					//Si es un panel se trata
					if (componentRootPane instanceof JPanel) {
						//Se llama al método que obtiene una lista de códigos de atajos asociados a los componentes del panel
						getKeyCodeList ((JPanel) componentRootPane, keyCodes);
						
					} else if (componentRootPane instanceof JLayeredPane) { //Si es un layeredPane se obtienen sus componentes
						Component[] componentsLayeredPane = ((JLayeredPane) componentRootPane).getComponents();
						//Se recorren
						for (int z = 0; z< componentsLayeredPane.length; z++) {
							//Se obtienen un elemento
							Component componentLayeredPane = componentsLayeredPane[z];
							//Si es instancia de JPanel se trata
							if (componentLayeredPane instanceof JPanel) {
								//Se llama al método que obtiene una lista de códigos de atajos asociados a los componentes del panel
								getKeyCodeList ((JPanel) componentLayeredPane, keyCodes);
							}
						}
					}
				}
			}
		}

		//Se crea un conjunto a partir de la lista para eliminar duplicados
		keyCodesSet = new HashSet<Integer>(keyCodes);
		//Si el tamaño de la lista y del conjunto no son iguales, no hay duplicados
		assertTrue(keyCodesSet.size() == keyCodes.size());
	}

	/**
	 * Método que obtiene una lista de códigos de atajos a los componentes (Etiqueta, Botón, Checkbox) de un panel.
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
				} else if (component instanceof JCheckBox) { //Se comprueba si es un checkbox
					JCheckBox checkBox = (JCheckBox) component;
					//Se obtiene el código del atajo asociado
					keyCode = checkBox.getMnemonic();
					//Se añade a la lista si existe este código, es decir, si es distinto de 0
					if (keyCode != 0) {
						keyCodeList.add(new Integer(keyCode));
					}
				}
				
			} else {
				//Si es un panel se vuelve a llamar recursivamente al método
				getKeyCodeList((JPanel) component, keyCodeList);
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
