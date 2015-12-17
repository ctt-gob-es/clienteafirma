package es.gob.afirma.ui.principal;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

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
	private static Logger LOGGER = Logger.getLogger(CifradoAccessibilityTest.class.getName());

	/**
	 * Comprobacion de que el campo labelFor de las etiquetas no este duplicado.
	 */
	@Test
	public void testNotDuplicatedLabelForProperty_SimpleMode() {
		LOGGER.info("testNotDuplicatedLabelForProperty_SimpleMode"); //$NON-NLS-1$

		try {
			//Instancia del panel que se va a analizar
			final Cifrado cifradoPanel = new Cifrado();
			//Lista de componentes asociados
			final List <Component> componentList = new ArrayList<>();
			//Conjunto de componentes asociados
			Set <Component> componentSet = null;

			//Array de componentes
			final Component[] components = cifradoPanel.getComponents();
			//Se recorren los componentes del panel
			for (final Component component2 : components) {
				//Se comprueba si es una etiqueta
				if (component2 instanceof JLabel) {
					final JLabel label = (JLabel) component2;
					final Component component = label.getLabelFor();
					//Para el modo simple puede haber etiquetas no asociadas a componentes
					//Si el componente es nulo se ignora
					if (component != null) {
						//Se anade a la lista el componente
						componentList.add(component);
					}
				} else if (component2 instanceof JPanel) {
					getLabelForComponentList((JPanel)component2,componentList);
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
	 * Comprobacion de que el campo labelFor de las etiquetas no este duplicado.
	 */
	@Test
	public void testNotDuplicatedLabelForProperty_AdvancedMode() {
		LOGGER.info("testNotDuplicatedLabelForProperty_AdvancedMode"); //$NON-NLS-1$

		try {
			//Se obtiene la cofiguracion general
			//Se anade el perfil por defecto
			UserProfile.setCurrentProfileId(Constants.DEFAULT_USER);
			GeneralConfig.loadConfig(GeneralConfig.getConfig());
			final Properties config = GeneralConfig.getConfig();
			//Se cambia al modo avanzado
			config.setProperty(MainOptionsPane.MAIN_ADVANCED_VIEW, "true"); //$NON-NLS-1$
			//Se asigna
			GeneralConfig.loadConfig(config);

			//Instancia del panel que se va a analizar
			final Cifrado cifradoPanel = new Cifrado();
			//Lista de componentes asociados
			final List <Component> componentList = new ArrayList<>();
			//Conjunto de componentes asociados
			Set <Component> componentSet = null;


			//Array de componentes
			final Component[] components = cifradoPanel.getComponents();
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
				} else if (component2 instanceof JPanel) {
					getLabelForComponentList((JPanel)component2,componentList);
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
	public void testNotDuplicatedDisplayedMnemonic_AdvancedMode() {
		LOGGER.info("testNotDuplicatedDisplayedMnemonic_AdvancedMode"); //$NON-NLS-1$

		try {
			//Se obtiene la cofiguracion general
			//Se anade el perfil por defecto
			UserProfile.setCurrentProfileId(Constants.DEFAULT_USER);
			GeneralConfig.loadConfig(GeneralConfig.getConfig());
			final Properties config = GeneralConfig.getConfig();
			//Se cambia al modo avanzado
			config.setProperty(MainOptionsPane.MAIN_ADVANCED_VIEW, "true"); //$NON-NLS-1$
			//Se asigna
			GeneralConfig.loadConfig(config);

			//Instancia del panel que se va a analizar
			final Cifrado cifradoPanel = new Cifrado();
			//Lista de mnemonicos
			final List <Integer> keyCodes = new ArrayList<>();
			//Conjunto de mnemonicos
			Set <Integer> keyCodesSet = null;

			//Se llama al metodo que obtiene una lista de codigos de atajos asociados a los componentes del panel
			getKeyCodeList (cifradoPanel, keyCodes);

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
