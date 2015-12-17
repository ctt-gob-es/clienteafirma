package es.gob.afirma.ui.wizardmultifirmamasiva;

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
import javax.swing.JLayeredPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JRootPane;

import org.junit.Ignore;
import org.junit.Test;

/**
 * Testeo de accesibilidad para la clase PanelFormatos.
 * @author lmerayo
 *
 */
public class PanelFormatosAccessibilityTest {

	/**
	 * Log.
	 */
	static Logger logger = Logger.getLogger(PanelFormatosAccessibilityTest.class.getName());

	/**
	 * Comprobacion de que el campo labelFor de las etiquetas no este duplicado.
	 */
	@Test
	public void testNotDuplicatedLabelForProperty() {
		logger.info("testNotDuplicatedLabelForProperty"); //$NON-NLS-1$

		try {
			//Instancia del panel que se va a analizar
			final PanelFormatos panelFormatos = new PanelFormatos();
			//Lista de componentes asociados
			final List <Component> componentList = new ArrayList<>();
			//Conjunto de componentes asociados
			Set <Component> componentSet = null;

			//Componentes del wizard
			final Component[] components = panelFormatos.getComponents();

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
							//Se llama al metodo que obtiene una lista de componentes asociados a la propiedad labelFor del panel
							getLabelForComponentList ((JPanel) componentRootPane, componentList);

						} else if (componentRootPane instanceof JLayeredPane) { //Si es un layeredPane se obtienen sus componentes
							final Component[] componentsLayeredPane = ((JLayeredPane) componentRootPane).getComponents();
							//Se recorren
							for (final Component componentLayeredPane : componentsLayeredPane) {
								//Si es instancia de JPanel se trata
								if (componentLayeredPane instanceof JPanel) {

									//Se llama al metodo que obtiene una lista de componentes asociados a la propiedad labelFor del panel
									getLabelForComponentList ((JPanel) componentLayeredPane, componentList);
								}
							}
						}
					}
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
	 * Comprobacion de que el campo Mnemocic de las etiquetas,botones y checkbox no esten duplicados.
	 */
	@Test
	public void testNotDuplicatedDisplayedMnemonic() {
		logger.info("testNotDuplicatedDisplayedMnemonic"); //$NON-NLS-1$

		try {
			//Instancia del panel que se va a analizar
			final PanelFormatos panelFormatos = new PanelFormatos();

			//Lista de mnemonicos
			final List <Integer> keyCodes = new ArrayList<>();
			//Conjunto de mnemonicos
			Set <Integer> keyCodesSet = null;

			//Componentes del wizard
			final Component[] components = panelFormatos.getComponents();

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
	 * Metodo que obtiene una lista de codigos de atajos a los componentes (Etiqueta, Boton, Checkbox, Radiobutton) de un panel.
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
				} else if (component instanceof JCheckBox) { //Se comprueba si es un checkbox
					final JCheckBox checkBox = (JCheckBox) component;
					//Se obtiene el codigo del atajo asociado
					keyCode = checkBox.getMnemonic();
					//Se anade a la lista si existe este codigo, es decir, si es distinto de 0
					if (keyCode != 0) {
						keyCodeList.add(new Integer(keyCode));
					}
				} else if (component instanceof JRadioButton) { //Se comprueba si es un boton de radio
					final JRadioButton radioButton = (JRadioButton) component;
					//Se obtiene el codigo del atajo asociado
					keyCode = radioButton.getMnemonic();
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
