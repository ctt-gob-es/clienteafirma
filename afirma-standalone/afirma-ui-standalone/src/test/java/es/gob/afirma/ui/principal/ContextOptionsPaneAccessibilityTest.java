package es.gob.afirma.ui.principal;

import static org.junit.Assert.assertTrue;

import java.awt.Component;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.junit.Ignore;
import org.junit.Test;

/**
 * Testeo de accesibilidad para la clase ContextOptionsPane.
 * @author lmerayo
 *
 */
public class ContextOptionsPaneAccessibilityTest {

	/**
	 * Log.
	 */
	private static Logger LOGGER = Logger.getLogger(ContextOptionsPaneAccessibilityTest.class.getName());

	/**
	 * Comprobacion de que el campo labelFor de las etiquetas no este duplicado.
	 */
	@Test
	public void testNotDuplicatedLabelForProperty() {
		LOGGER.info("testNotDuplicatedLabelForProperty"); //$NON-NLS-1$

		try {
			//Instancia de la clase que se va a analizar
			final ContextOptionsPane contextOptionsPane = new ContextOptionsPane();
			//Lista de componentes asociados
			final List <Component> componentList = new ArrayList<>();
			//Conjunto de componentes asociados
			Set <Component> componentSet = null;

			//Se obtiene el panel principal
			final JPanel mainPanel = contextOptionsPane.getConfigurationPanel();

			//Se llama al metodo que obtiene una lista de componentes asociados a la propiedad labelFor del panel
			getLabelForComponentList (mainPanel, componentList);

			//Se crea un conjunto a partir de la lista para eliminar duplicados
			componentSet = new HashSet<>(componentList);
			//Si el tamano de la lista y del conjunto no son iguales, no hay duplicados
			assertTrue(componentSet.size() == componentList.size());
		}
		catch(final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}

	}

	/**
	 * Comprobacion de que el campo Mnemocic de las etiquetas, botones y checkbox no este duplicado.
	 */
	@Test
	public void testNotDuplicatedDisplayedMnemonic() {
		LOGGER.info("testNotDuplicatedDisplayedMnemonic"); //$NON-NLS-1$

		try {
			//Instancia de la clase que se va a analizar
			final ContextOptionsPane contextOptionsPane = new ContextOptionsPane();
			//Lista de mnemonicos
			final List <Integer> keyCodes = new ArrayList<>();
			//Conjunto de mnemonicos
			Set <Integer> keyCodesSet = null;

			//Se obtiene el panel principal
			final JPanel mainPanel = contextOptionsPane.getConfigurationPanel();

			//Se llama al metodo que obtiene una lista de codigos de atajos asociados a los componentes del panel
			getKeyCodeList (mainPanel, keyCodes);

			//Se crea un conjunto a partir de la lista para eliminar duplicados
			keyCodesSet = new HashSet<>(keyCodes);
			//Si el tamano de la lista y del conjunto no son iguales, no hay duplicados
			assertTrue(keyCodesSet.size() == keyCodes.size());
		}
		catch(final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}
	}

	/**
	 * Metodo que obtiene una lista de codigos de atajos a los botones, etiquetas y checkbox de un panel.
	 */
	@Ignore
	private void getKeyCodeList(final JPanel panel, final List <Integer> keyCodeList) {
		//Array de componentes del panel
		final Component[] components = panel.getComponents();
		int keyCode = 0;
		for (int i = 0; i < components.length; i++) {
			//Se obtiene el componente
			final Component component = panel.getComponent(i);
			if (!(component instanceof JPanel)) {
				//Se comprueba si es una etiqueta
				if (component instanceof JLabel) {
					final JLabel label = (JLabel) component;
					//Se obtiene el codigo del atajo asociado
					keyCode = label.getDisplayedMnemonic();
					//Se anade a la lista si existe este codigo, es decir, si es distinto de 0
					if (keyCode != 0) {
						keyCodeList.add(new Integer(keyCode));
					}
				} else if (component instanceof JButton) { //Se comprueba si es un boton
					final JButton button = (JButton) component;
					//Se obtiene el codigo del atajo asociado
					keyCode = button.getMnemonic();
					//Se anade a la lista si existe este codigo, es decir, si es distinto de 0
					if (keyCode != 0) {
						keyCodeList.add(new Integer(keyCode));
					}
				} else if (component instanceof JCheckBox) { //Se comprueba si es un checkbox
					final JCheckBox checkBox = (JCheckBox) component;
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
	 * Metodo que obtiene la propiedad labelFor de las etiquetas de un panel.
	 */
	@Ignore
	private void getLabelForComponentList(final JPanel panel, final List <Component> componentList) {
		//Array de componentes del panel
		final Component[] components = panel.getComponents();
		Component labelForComponent = null;
		for (int i = 0; i < components.length; i++) {
			//Se obtiene el componente
			final Component component = panel.getComponent(i);
			if (!(component instanceof JPanel)) {
				//Se comprueba si es una etiqueta
				if (component instanceof JLabel) {
					final JLabel label = (JLabel) component;
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
