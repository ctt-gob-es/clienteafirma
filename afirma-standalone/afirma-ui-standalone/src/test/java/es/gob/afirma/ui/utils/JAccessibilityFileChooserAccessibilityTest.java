package es.gob.afirma.ui.utils;

import static org.junit.Assert.assertTrue;

import java.awt.Component;
import java.awt.Container;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JToggleButton;

import org.junit.Ignore;
import org.junit.Test;


/**
 * Testeo de accesibilidad para la clase JAccessibilityFileChooser.
 * @author lmerayo
 *
 */
public class JAccessibilityFileChooserAccessibilityTest {

	/**
	 * Log.
	 */
	static Logger logger = Logger.getLogger(JAccessibilityFileChooserAccessibilityTest.class.getName());


	/** Comprobacion de que el campo Mnemocic de las etiquetas y botones no este duplicado. */
	@Test
	public void testNotDuplicatedDisplayedMnemonic() {
		logger.info("testNotDuplicatedDisplayedMnemonic"); //$NON-NLS-1$

		try {
			//Instancia del contenedor que se va a analizar
			final JAccessibilityFileChooser fileChooser= new JAccessibilityFileChooser(null);

			//Lista de mnemonicos
			final List <Integer> keyCodes = new ArrayList<>();
			//Conjunto de mnemonicos
			Set <Integer> keyCodesSet = null;

			//Se llama al metodo que obtiene una lista de codigos de atajos asociados a los componentes del panel
			getKeyCodeList (fileChooser, keyCodes);

			//Se crea un conjunto a partir de la lista para eliminar duplicados
			keyCodesSet = new HashSet<>(keyCodes);
			//Si el tamano de la lista y del conjunto no son iguales, no hay duplicados
			assertTrue(keyCodesSet.size() == keyCodes.size());
		} catch (final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}
	}

	/**
	 * Metodo que obtiene una lista de codigos de atajos a los componentes (Etiqueta, Boton) de un panel.
	 */
	@Ignore
	private void getKeyCodeList(final Container panel, final List <Integer> keyCodeList) {
		//Array de componentes del panel
		final Component[] components = panel.getComponents();
		int keyCode = 0;
		for (int i = 0; i < components.length; i++) {
			//Se obtiene el componente
			final Component component = panel.getComponent(i);
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
			} else if (component instanceof JToggleButton) {
					final JToggleButton toggleButton = (JToggleButton) component;

					//Se obtiene el codigo del atajo asociado
					keyCode = toggleButton.getMnemonic();
					//Se anade a la lista si existe este codigo, es decir, si es distinto de 0
					if (keyCode != 0) {
						keyCodeList.add(new Integer(keyCode));
					}
			} else if (component instanceof Container) {
				//Si es un contenedor se vuelve a llamar recursivamente al metodo
				getKeyCodeList((Container)component, keyCodeList);
			}
		} //for
	}//getKeyCodeList

}
