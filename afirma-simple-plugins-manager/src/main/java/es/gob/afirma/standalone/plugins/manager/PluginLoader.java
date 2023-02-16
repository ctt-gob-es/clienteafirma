package es.gob.afirma.standalone.plugins.manager;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.ServiceLoader;

import es.gob.afirma.standalone.plugins.AfirmaPlugin;
import es.gob.afirma.standalone.plugins.GenericMenuOption;
import es.gob.afirma.standalone.plugins.PluginAction;
import es.gob.afirma.standalone.plugins.PluginButton;
import es.gob.afirma.standalone.plugins.PluginCommand;
import es.gob.afirma.standalone.plugins.PluginCommandAction;
import es.gob.afirma.standalone.plugins.PluginInfo;

/**
 * Clase encargada de cargar y mantener los objetos correspondientes a un plugin.<br>
 * Esta clase permite cargar una colecci&oacute;n de archivos JARs entre los que
 * se buscar&acute;n uno o m&acute;s plugins. Los plugins podr&aacute;n hacer uso
 * de las clases de las bibliotecas que los acompa&ntilde;en, pero si estas tienen
 * clases que ya existen en la aplicaci&oacute;n, se priorizar&aacute;n las de la
 * aplicaci&oacute;n (comportamiento por defecto del ClassLoader).
 */
public class PluginLoader {

	private static final String CONFIG_FILE = "/plugin.json"; //$NON-NLS-1$

	public static final Map<String, AfirmaPlugin> classLoaderForPlugin = new HashMap<>();

	/**
	 * Carga la colecci&oacute;n de JARs de un plugin y devuelve un objeto
	 * con el cual poder hacer referencia a los objetos y caracter&iacute;sticas
	 * de ese plugin. Ya que en en esa coleccion de JAR podr&iacute; encontrarse
	 * m&aacute;s de un plugin, se devolver&aacute; un PluginLoad por cada uno de ellos
	 * @param jars Ficheros JAR a cargar.
	 * @return Plugins encontrados en los JAR indicados.
	 * @throws IOException Cuando ocurre un error al cargar las clases.
	 * @throws PluginException Cuando no se encuentran plugins o se encuentra mas de uno
	 * en el archivo.
	 */
	static AfirmaPlugin loadPlugin(final File[] jars) throws IOException, PluginException {

		AfirmaPlugin loadedPlugin;

		// Cargamos los JAR
		final List<URL> urls = new ArrayList<>();
		for (final File jar : jars) {
			urls.add(jar.toURI().toURL());
		}

		final URLClassLoader classLoader = new URLClassLoader(
				urls.toArray(new URL[urls.size()]),
				PluginLoader.class.getClassLoader());

		try {
			// Cargamos las clases de plugin
			final List<AfirmaPlugin> plugins = new ArrayList<>();
			final ServiceLoader<AfirmaPlugin> loader;
			try {
				loader = ServiceLoader.load(AfirmaPlugin.class, classLoader);
				for (final AfirmaPlugin plugin : loader) {
					plugins.add(plugin);
				}
			}
			catch (final Error e) {
				classLoader.close();
				throw new PluginException("Se han contrado plugins mal definidos en el fichero importado"); //$NON-NLS-1$
			}

			if (plugins.size() == 0) {
				classLoader.close();
				throw new PluginException("No se encontro ningun plugin en los archivos"); //$NON-NLS-1$
			}

			if (plugins.size() > 1) {
				classLoader.close();
				throw new PluginException("No se permite la carga simulatea de varios plugins"); //$NON-NLS-1$
			}

			loadedPlugin = plugins.get(0);
			loadedPlugin.setClassLoader(classLoader);

			final PluginInfo info = loadPluginConfiguration(loadedPlugin);

			registryPluginReferences(loadedPlugin, info);

			// TODO: Puesto que ya almacenamos el classLoader de cada clase del plugin, ya
			// no es necesario configurar las acciones de los botones de los plugins. Habria
			// que ver donde se usa cada accion y cargar las clases involucradas a partir de
			// su classLoader, igual que se hace con los menus.
			if (info.getButtons() != null) {
				for (final PluginButton button : info.getButtons()) {

					if (button.getActionClassName() == null) {
						classLoader.close();
						throw new PluginException(String.format("El plugin '%1s' no ha definido accion para un boton", info.getName())); //$NON-NLS-1$
					}
					if (button.getWindow() == null) {
						classLoader.close();
						throw new PluginException(String.format("El plugin '%1s' no ha definido la ventana en la que debe aparecer un boton", info.getName())); //$NON-NLS-1$
					}
					try {
						final PluginAction action = (PluginAction)
								loadAction(button.getActionClassName(), PluginAction.class, classLoader);
						action.setPlugin(loadedPlugin);
						button.setAction(action);
					}
					catch (final Exception e) {
						classLoader.close();
						throw new PluginException(String.format("El plugin '%1s' definio una clase de accion erronea: %2s", //$NON-NLS-1$
								info.getName(), button.getActionClassName()), e);
					}
				}
			}
			// Configuramos la informacion obtenida del plugin, en el propio plugin
			loadedPlugin.setInfo(info);
		} catch (final Exception e) {
			classLoader.close();
			throw new PluginException(String.format("Ha ocurrido un error al intentar cargar el plugin"), e); //$NON-NLS-1$
		}

		return loadedPlugin;
	}

	/**
	 * M&eacute;todo encargado de asociar una lista de actions a un determinado classLoader.
	 * @param plugin Plugin.
	 * @param info Informaci&oacute;n asociada al plugin.
	 */
	private static void registryPluginReferences(final AfirmaPlugin plugin, final PluginInfo info) {
		final List<String> actions = new ArrayList<>();
		getListActions(info, actions);
		for (final String action : actions) {
			if (!classLoaderForPlugin.containsKey(action)) {
				classLoaderForPlugin.put(action, plugin);
			}
		}
	}

	/**
	 * M&eacute;todo encargado de recuperar la lista de actions del plugin dado.
	 * @param info Informaci%oaacute;n del plugin.
	 * @param actionsList Lista donde almacenar los actions encontrados.
	 */
	private static void getListActions(final PluginInfo info, final List<String> actionsList) {
		if (info != null) {
			// Acciones asociadas a botones
			if (info.getButtons() != null) {
				for(final PluginButton btn : info.getButtons()) {
					actionsList.add(btn.getActionClassName());
				}
			}
			// Acciones asociadas a comandos
			if (info.getCommands() != null) {
				for(final PluginCommand cmd : info.getCommands()) {
					actionsList.add(cmd.getCommandActionClass());
				}
			}
			// Acciones asociadas a menus
			if (info.getMenu() != null) {
				getListActionsFromMenus(info.getMenu(), actionsList);
			}
		}
	}

	/**
	 * M&eacute;todo encargado de recuperar la lista de acciones de los menus de un plugin dado.
	 * @param menu Menu a recorrer para extraer los actions.
	 * @param actionsList lista donde almacenar los actions encontrados.
	 */
	private static void getListActionsFromMenus(final GenericMenuOption menu, final List<String> actionsList) {
		if (menu.getActionClassName() != null) {
			actionsList.add(menu.getActionClassName());
		}
		if (menu.getMenus() != null) {
			for (final GenericMenuOption subMenu : menu.getMenus()) {
				getListActionsFromMenus(subMenu, actionsList);
			}
		}
	}

	/**
	 * Carga la configuracion del plugin.
	 * @param plugin Plugin del que cargar la configuraci&oacute;n.
	 * @return Informaci&oacute;n extra&iacute;da del plugin.
	 * @throws IOException Cuando se produce alg&uacute;n error en la lectura del plugin.
	 * @throws PluginException Cuando se encuentra un error en el fichero de
	 * configuraci&oacute;n del plugin.
	 */
	private static PluginInfo loadPluginConfiguration(final AfirmaPlugin plugin) throws IOException, PluginException {
		final String classPackage = plugin.getClass().getPackage().getName();
		final String infoResource = '/' + classPackage.replace('.', '/') + CONFIG_FILE;

		PluginInfo info;
		try (InputStream is = plugin.getClass().getResourceAsStream(infoResource)) {
			if (is == null) {
				throw new IOException(String.format("No se encontro el fichero %1s en el plugin", infoResource)); //$NON-NLS-1$
			}
			try (InputStreamReader reader = new InputStreamReader(is, StandardCharsets.UTF_8)) {
				info = PluginInfoLoader.parseInfo(reader);
			}
		} catch (final IOException e) {
			throw new IOException("Error en la lectura del fichero " + CONFIG_FILE, e); //$NON-NLS-1$

		} catch (final Exception e) {
			throw new PluginException("Se ha encontrado un error en el fichero " + CONFIG_FILE, e); //$NON-NLS-1$
		}

		return info;
	}

	/**
	 * Carga la acci&oacute;n conocida su clase.
	 * @param actionClassName Nombre de la acci&oacute;n.
	 * @return Acci&oacute;n del plugin.
	 * @throws PluginException Cuando no se pueda cargar la acci&oacute;n.
	 */
	public static PluginAction getPluginAction(final String actionClassName)
			throws PluginException {

		final AfirmaPlugin plugin = classLoaderForPlugin.get(actionClassName);

		final PluginAction action = (PluginAction)
				loadAction(actionClassName, PluginAction.class, plugin.getClassLoader());
		action.setPlugin(plugin);

		return action;
	}

	/**
	 * Carga una acci&oacute;n de l&iacute;nea de comandos conocida su clase.
	 * @param actionClassName Nombre de la acci&oacute;n.
	 * @return Acci&oacute;n de l&iacute;nea de comandos del plugin.
	 * @throws PluginException Cuando no se pueda cargar la acci&oacute;n.
	 */
	public static PluginCommandAction getPluginCommandAction(final String actionClassName)
			throws PluginException {

		final AfirmaPlugin plugin = classLoaderForPlugin.get(actionClassName);

		final PluginCommandAction action = (PluginCommandAction)
				loadAction(actionClassName, PluginCommandAction.class, plugin.getClassLoader());
		action.setPlugin(plugin);

		return action;
	}

	private static Object loadAction(final String actionClassName, final Class<?> actionClassType,
			final ClassLoader classLoader) throws PluginException {
		Class<?> actionClass;
		try {
			actionClass = Class.forName(actionClassName, false, classLoader);
		} catch (final Throwable e) {
			throw new PluginException(String.format("La clase de accion %1s no existe", //$NON-NLS-1$
					actionClassName), e);
		}

		if (!actionClassType.isAssignableFrom(actionClass)) {
			throw new PluginException(String.format(
					"Se ha establecido un boton que no define una accion de tipo %1s", //$NON-NLS-1$
					PluginAction.class.getName()));
		}

		Object actionObject;
		try {
			actionObject = actionClass.getConstructor().newInstance();
		} catch (final Throwable e) {
			throw new PluginException(String.format("No se puede instanciar la clase %s. El constructor por defecto deber existir y ser publico", //$NON-NLS-1$
					actionClassName), e);
		}
		return actionObject;
	}
}
