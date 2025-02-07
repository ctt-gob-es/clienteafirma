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
import es.gob.afirma.standalone.SimpleAfirma;
import es.gob.afirma.standalone.plugins.AfirmaPlugin;
import es.gob.afirma.standalone.plugins.GenericMenuOption;
import es.gob.afirma.standalone.plugins.Permission;
import es.gob.afirma.standalone.plugins.PluginButton;
import es.gob.afirma.standalone.plugins.PluginInfo;
import es.gob.afirma.standalone.plugins.manager.PermissionChecker;
import es.gob.afirma.standalone.plugins.manager.PluginException;

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
	public static List<PluginGraphicButton> getPluginsButtons(final PluginIntegrationWindow currentWindow) {

		final List<PluginGraphicButton> jButtons = new ArrayList<>();
        List<AfirmaPlugin> plugins = null;
		try {
			plugins = SimpleAfirma.getPluginsManager().getPluginsLoadedList();
		} catch (final PluginException e) {
			LOGGER.log(Level.SEVERE, "No se han podido cargar los plugins en la aplicacion", e); //$NON-NLS-1$
		}
		if (plugins != null) {
			for (final AfirmaPlugin plugin : plugins) {
				final PluginInfo info = plugin.getInfo();
				if (PermissionChecker.check(info, Permission.BUTTONS)) {
					final PluginButton[] buttons = info.getButtons();
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
		}
		return jButtons;
	}

	/**
	 * Recupera la lista de menus de los plugins cargados por Autofirma.
	 * @return Listado de menus encontrados.
	 */
	public static List<GenericMenuOption> getPluginsMenus() {
		final List<GenericMenuOption> menuList = new ArrayList<>();
		List<AfirmaPlugin> plugins = null;
		try {
			plugins = SimpleAfirma.getPluginsManager().getPluginsLoadedList();
		} catch (final PluginException e) {
			LOGGER.log(Level.SEVERE, "No se han podido cargar los plugins en la aplicacion", e); //$NON-NLS-1$
		}
		if(plugins != null) {
			for (final AfirmaPlugin plugin : plugins) {
				final PluginInfo info = plugin.getInfo();
				if (PermissionChecker.check(info, Permission.MENU)) {
					final GenericMenuOption menu = info.getMenu();
					if(menu != null) {
						menuList.add(menu);
					}
				}
			}
		}
		return menuList;
	}

	/**
	 * Crea un bot&oacute;n para mostrar en la barra de plugins de alguna de las
	 * pantallas de la aplicaci&oacute;n.
	 * @param button Bot&oacute;n que ejecuta una acci&oacute;n de plugin.
	 * @param classLoader ClassLoader a partir del cual cargar los recursos necesarios para el bot&oacute;n.
	 * @return Bot&oacute;n.
	 */
	public static JButton buildButton(final PluginButton button, final ClassLoader classLoader) {

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
