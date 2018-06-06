package es.gob.afirma.standalone.ui;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.ImageIcon;
import javax.swing.JButton;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.standalone.plugins.AfirmaPlugin;
import es.gob.afirma.standalone.plugins.PluginButton;
import es.gob.afirma.standalone.plugins.PluginException;
import es.gob.afirma.standalone.plugins.PluginIntegrationWindow;
import es.gob.afirma.standalone.plugins.PluginsManager;

/**
 * Clase para la creaci&oacute;n de componentes gr&aacute;ficos para la integraci&oacute;n de los plugins.
 */
public class PluginsUiComponentsBuilder {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$


	/**
	 * Construye los botones correspondientes a una pantalla definidos por los
	 * plugins instalados.
	 * @param currentWindow Ventana para la que se solicita el listado de botones.
	 * @return Listado de botones o {@code null} si no se definen botones.
	 */
	public static List<PluginGraphicButton> getPluginsButtons(PluginIntegrationWindow currentWindow) {

		final List<PluginGraphicButton> jButtons = new ArrayList<>();
        List<AfirmaPlugin> plugins = null;
		try {
			plugins = PluginsManager.getInstance().getPluginsLoadedList();
		} catch (final PluginException e) {
			LOGGER.log(Level.SEVERE, "No se han podido cargar los plugins en la aplicacion", e); //$NON-NLS-1$
		}
		if (plugins != null) {
			for (final AfirmaPlugin plugin : plugins) {
				final PluginButton[] buttons = plugin.getInfo().getButtons();
				if (buttons != null) {
					for (final PluginButton button : buttons) {
						try {
							final PluginIntegrationWindow targetWindow = PluginIntegrationWindow.getWindow(button.getWindow());
							if (targetWindow == currentWindow) {
								final JButton jButton = buildButton(button, plugin.getClass().getClassLoader());
								jButtons.add(new PluginGraphicButton(button, jButton));
							}
						}
						catch (final Exception e) {
							LOGGER.log(Level.WARNING, String.format("El plugin %1s ha declarado botones invalidos", plugin.getInfo().getName()), e); //$NON-NLS-1$
						}
					}
				}
			}
		}
		return jButtons;
	}

	/**
	 * Crea un bot&oacute;n para mostrar en la barra de plugins de alguna de las
	 * pantallas de la aplicaci&oacute;n.
	 * @param button Bot&oacute;n que ejecuta una acci&oacute;n de plugin.
	 * @param classLoader ClassLoader a partir del cual cargar los recursos necesarios para el bot&oacute;n.
	 * @return Bot&oacute;n.
	 */
	public static JButton buildButton(PluginButton button, ClassLoader classLoader) {

		final JButton jButton = new JButton();
		jButton.setText(button.getTitle());
		if (button.getIcon() != null) {
			final ClassLoader loader = classLoader != null ? classLoader : PluginsUiComponentsBuilder.class.getClassLoader();
			try (InputStream is = loader.getResourceAsStream(button.getIcon())) {
				final byte[] image = AOUtil.getDataFromInputStream(is);
					jButton.setIcon(new ImageIcon(image));
			} catch (final IOException e) {
				LOGGER.warning("No se pudo cargar el icono de un boton de plugin: " + e); //$NON-NLS-1$
			}
		}
		jButton.setToolTipText(button.getToolTip());
		jButton.getAccessibleContext().setAccessibleDescription(button.getAccesibleDescription());

		return jButton;
	}
}
