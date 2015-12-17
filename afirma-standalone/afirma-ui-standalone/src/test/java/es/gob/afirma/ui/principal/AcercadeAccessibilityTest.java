package es.gob.afirma.ui.principal;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

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
	private static Logger LOGGER = Logger.getLogger(AcercadeAccessibilityTest.class.getName());

	/**
	 * Comprobacion de que el campo Mnemocic de los botones no este duplicado.
	 */
	@Test
	public void testNotDuplicatedMnemonic() {
		LOGGER.info("testNotDuplicatedMnemonic"); //$NON-NLS-1$

		try {
			//Instancia del panel que se va a analizar
			final Acercade acercade = new Acercade();
			//Lista de mnemonicos
			final List <Integer> keyCodes = new ArrayList<>();
			//Conjunto de mnemonicos
			Set <Integer> keyCodesSet = null;

			//Componentes del wizard
			final Component[] components = acercade.getComponents();

			//Se recorren
			for (final Component componentWizard : components) {
				//Se trata el panel principal del wizard
				if (componentWizard instanceof JRootPane) {
					//Se obtienen los componentes del panel principal
					final Component[] componentsRootPane = ((JRootPane)componentWizard).getComponents();
					//Se recorren
					for (final Component componentRootPane : componentsRootPane) {
						//Si es un panel se trata
						if (componentRootPane instanceof JPanel) {
							//Se llama al metodo que obtiene una lista de codigos de atajos asociados a los componentes del panel
							getKeyCodeList ((JPanel) componentRootPane, keyCodes);

						} else if (componentRootPane instanceof JLayeredPane) { //Si es un layeredPane se obtienen sus componentes
							final Component[] componentsLayeredPane = ((JLayeredPane) componentRootPane).getComponents();
							//Se recorren
							for (final Component componentLayeredPane : componentsLayeredPane) {
								//Si es instancia de JPanel se trata
								if (componentLayeredPane instanceof JPanel) {
									//Se llama al metodo que obtiene una lista de codigos de atajos asociados a los componentes del panel
									getKeyCodeList ((JPanel) componentLayeredPane, keyCodes);
								}
							}
						}
					}
				}
			}


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
	 * Comprobacion de que los JTextPane no sean editables.
	 */
	@Test
	public void testEditableTextPane() {
		LOGGER.info("testEditableTextPane"); //$NON-NLS-1$

		try {
			//Instancia del panel que se va a analizar
			final Acercade acercade = new Acercade();
			//Componentes del wizard
			final Component[] components = acercade.getComponents();

			//Se recorren
			for (final Component componentWizard : components) {
				//Se trata el panel principal del wizard
				if (componentWizard instanceof JRootPane) {
					//Se obtienen los componentes del panel principal
					final Component[] componentsRootPane = ((JRootPane)componentWizard).getComponents();
					//Se recorren
					for (final Component componentRootPane : componentsRootPane) {
						//Si es un panel se trata
						if (componentRootPane instanceof JPanel) {
							//Se llama al metodo que indica si hay algun panel de texto editable
							assertFalse(isJTextPaneEditable((JPanel) componentRootPane));

						} else if (componentRootPane instanceof JLayeredPane) { //Si es un layeredPane se obtienen sus componentes
							final Component[] componentsLayeredPane = ((JLayeredPane) componentRootPane).getComponents();
							//Se recorren
							for (final Component componentLayeredPane : componentsLayeredPane) {
								//Si es instancia de JPanel se trata
								if (componentLayeredPane instanceof JPanel) {
									//Se llama al metodo que indica si hay algun panel de texto editable
									assertFalse(isJTextPaneEditable ((JPanel) componentLayeredPane));
								}
							}
						}
					}
				}
			}
		}
		catch(final java.awt.HeadlessException e) {
			// Ignoramos este error, pero no otros, para evitar fallos en tests automaticos en servidor
		}
	}

	/**
	 * Metodo que obtiene una lista de codigos de atajos a los botones de un panel.
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
				if (component instanceof JButton) {
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
				getKeyCodeList((JPanel) component, keyCodeList);
			}
		} //for
	}//getKeyCodeList

	/**
	 * Metodo que comprueba si los JTextPane son editables.
	 */
	@Ignore
	private boolean isJTextPaneEditable(final JPanel panel) {
		final boolean result = false;
		//Array de componentes del panel
		final Component[] components = panel.getComponents();
		//Se recorren los componentes
		for (int i = 0; i < components.length; i++) {
			//Se obtiene el componente
			final Component component = panel.getComponent(i);
			if (!(component instanceof JPanel)) {
				//Se comprueba si el componente es un JTextPane
				if (component instanceof JTextPane) {
					final JTextPane textPane = (JTextPane) component;
					//Si el textPane es editable se sale del bucle
					if (textPane.isEditable()) {
						return true;
					}
				}

			} else {
				//Si es un panel se vuelve a llamar recursivamente al metodo
				isJTextPaneEditable((JPanel) component);
			}
		} //for

		return result;
	}//isJTextPaneEditable

}
