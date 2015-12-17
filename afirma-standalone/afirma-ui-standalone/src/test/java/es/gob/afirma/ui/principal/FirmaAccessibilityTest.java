package es.gob.afirma.ui.principal;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

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
	 * Comprobacion de que el campo labelFor de las etiquetas no este duplicado.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void testNotDuplicatedLabelForProperty() {
		logger.info("testNotDuplicatedLabelForProperty"); //$NON-NLS-1$

		try {
			//Instancia del panel que se va a analizar
			final Firma firmaPanel = new Firma();
			//Lista de componentes asociados
			final List <Component> componentList = new ArrayList<>();
			//Conjunto de componentes asociados
			Set <Component> componentSet = null;

			//Array de componentes
			final Component[] components = firmaPanel.getComponents();
			//Se recorren los componentes del panel
			for (final Component component2 : components) {
				//Se comprueba si es una etiqueta
				if (component2 instanceof JLabel) {
					final JLabel label = (JLabel) component2;
					final Component component = label.getLabelFor();
					//Para este panel hasta el momento todas las etiquetas tienen asociado el campo labelFor
					assertNotNull(component);
					//Se anade a la lista el componente
					componentList.add(component);
				}
			}
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
	 * Comprobacion de que el campo Mnemocic de las etiquetas y botones no este duplicado.
	 */
	@Test
	public void testNotDuplicatedDisplayedMnemonic() {
		logger.info("testNotDuplicatedDisplayedMnemonic"); //$NON-NLS-1$

		try {
			//Instancia del panel que se va a analizar
			final Firma firmaPanel = new Firma();
			//Lista de mnemonicos
			final List <Integer> keyCodes = new ArrayList<>();
			//Conjunto de mnemonicos
			Set <Integer> keyCodesSet = null;

			//Se llama al metodo que obtiene una lista de codigos de atajos asociados a los componentes del panel
			getKeyCodeList (firmaPanel, keyCodes);

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
	 * Metodo que obtiene una lista de codigos de atajos a los componentes (Etiqueta, Boton) de un panel.
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
				} else if (component instanceof JButton) {
					final JButton button = (JButton) component;
					//Se obtiene el codigo del atajo asociado
					keyCode = button.getMnemonic();
					//Se anade a la lista si existe este codigo, es decir, si es distinto de 0
					if (keyCode != 0) {
						keyCodeList.add(new Integer(keyCode));
					}
				}

			} else {
				//Si es un panel se vuelve a llamar recursivamente al metodo
				getKeyCodeList((JPanel)component, keyCodeList);
			}
		} //for
	}//getKeyCodeList

}
