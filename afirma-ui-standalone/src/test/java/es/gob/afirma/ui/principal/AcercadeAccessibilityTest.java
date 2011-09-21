package es.gob.afirma.ui.principal;

import static org.junit.Assert.*;

import java.awt.Component;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JLayeredPane;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.JTextPane;

import org.junit.Ignore;
import org.junit.Test;

/**
 * Testeo de accesibilidad para la clase Acercade.
 * @author lmerayo
 *
 */
public class AcercadeAccessibilityTest {

	/**
	 * Log.
	 */
	static Logger logger = Logger.getLogger(AcercadeAccessibilityTest.class.getName());

	/**
	 * Comprobación de que el campo Mnemocic de los botones no esté duplicado. 
	 */
	@Test
	public void testNotDuplicatedMnemonic() {
		logger.info("testNotDuplicatedMnemonic");

		//Instancia del panel que se va a analizar
		Acercade acercade = new Acercade();
		//Lista de mnemónicos
		List <Integer> keyCodes = new ArrayList<Integer>();
		//Conjunto de mnemónicos
		Set <Integer> keyCodesSet = null;
		
		//Componentes del wizard
		Component[] components = acercade.getComponents();
		
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
	 * Comprobación de que el campo Mnemocic de los botones no esté duplicado. 
	 */
	@Test
	public void testEditableTextPane() {
		logger.info("testEditableTextPane");

		//Instancia del panel que se va a analizar
		Acercade acercade = new Acercade();
		//Componentes del wizard
		Component[] components = acercade.getComponents();
		
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
						//Se llama al método que indica si hay algun panel de texto editable
						assertFalse(isJTextPaneEditable((JPanel) componentRootPane));
						
					} else if (componentRootPane instanceof JLayeredPane) { //Si es un layeredPane se obtienen sus componentes
						Component[] componentsLayeredPane = ((JLayeredPane) componentRootPane).getComponents();
						//Se recorren
						for (int z = 0; z< componentsLayeredPane.length; z++) {
							//Se obtienen un elemento
							Component componentLayeredPane = componentsLayeredPane[z];
							//Si es instancia de JPanel se trata
							if (componentLayeredPane instanceof JPanel) {
								//Se llama al método que indica si hay algun panel de texto editable
								assertFalse(isJTextPaneEditable ((JPanel) componentLayeredPane));
							}
						}
					}
				}
			}
		}
	}

	/**
	 * Método que obtiene una lista de códigos de atajos a los botones de un panel.
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
				if (component instanceof JButton) {
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
				getKeyCodeList((JPanel) component, keyCodeList);
			}
		} //for
	}//getKeyCodeList
	
	/**
	 * Método que comprueba si los JKTextPane son editables.
	 */
	@Ignore
	private boolean isJTextPaneEditable(JPanel panel) {
		boolean result = false;
		//Array de componentes del panel
		Component[] components = panel.getComponents();
		//Se recorren los componentes
		for (int i = 0; i < components.length; i++) {
			//Se obtiene el componente
			Component component = panel.getComponent(i);
			if (!(component instanceof JPanel)) {
				//Se comprueba si el componente es un JTextPane
				if (component instanceof JTextPane) {
					JTextPane textPane = (JTextPane) component;
					//Si el textPane es editable se sale del bucle
					if (textPane.isEditable()) {
						return true;
					}
				}
				
			} else {
				//Si es un panel se vuelve a llamar recursivamente al método
				isJTextPaneEditable((JPanel) component);
			}
		} //for

		return result;
	}//isJTextPaneEditable

}
