package es.gob.afirma.standalone.plugins;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.List;
import java.util.ServiceLoader;

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

		// Cargamos los JAR
		final List<URL> urls = new ArrayList<>();
		for (final File jar : jars) {
			urls.add(jar.toURI().toURL());
		}
		final URLClassLoader classLoader = new URLClassLoader(
				urls.toArray(new URL[urls.size()]),
				PluginLoader.class.getClassLoader());

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

		final AfirmaPlugin plugin = plugins.get(0);
		final PluginInfo info = loadPluginConfiguration(plugin);

		for (final PluginButton button : info.getButtons()) {

			if (button.getActionClassName() == null) {
				classLoader.close();
				throw new PluginException(String.format("El plugin %1s no ha definido accion para un boton", info.getName())); //$NON-NLS-1$
			}
			if (button.getWindow() == null) {
				classLoader.close();
				throw new PluginException(String.format("El plugin %1s no ha definido la ventana en la que debe aparecer un boton", info.getName())); //$NON-NLS-1$
			}
			final PluginIntegrationWindow window = PluginIntegrationWindow.getWindow(button.getWindow());
			if (window == null) {
				classLoader.close();
				throw new PluginException(String.format("El plugin %1s definio un boton en una ventana desconocida", info.getName())); //$NON-NLS-1$
			}
			try {
				final PluginAction action = getPluginAction(button.getActionClassName(), window, classLoader);
				action.setPlugin(plugin);
				button.setAction(action);
			}
			catch (final Exception e) {
				classLoader.close();
				throw new PluginException(String.format("El plugin '%1s' definio una clase de accion erronea", info.getName()), e); //$NON-NLS-1$
			}
		}

		// Configuramos la informacion obtenida del plugin, en el propio plugin
		plugin.setInfo(info);

		return plugin;
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
				info = PluginInfoLoader.parseInfo(is);
		} catch (final IOException e) {
			throw new IOException("Error en la lectura del fichero " + CONFIG_FILE, e); //$NON-NLS-1$

		} catch (final Exception e) {
			throw new PluginException("Se ha encontrado un error en el fichero " + CONFIG_FILE, e); //$NON-NLS-1$
		}

		return info;
	}

	private static PluginAction getPluginAction(final String actionClassName,
			final PluginIntegrationWindow window, final ClassLoader classLoader) throws PluginException {
		Class<?> actionClass;
		try {
			actionClass = Class.forName(actionClassName, true, classLoader);
		} catch (final Throwable e) {
			throw new PluginException(String.format("La clase de accion %1s no existe", //$NON-NLS-1$
					actionClassName), e);
		}
		Object actionObject;
		try {
			actionObject = actionClass.newInstance();
		} catch (final Throwable e) {
			throw new PluginException(String.format("No se puede instanciar la clase %s. El constructor por defecto deber existir y ser publico", //$NON-NLS-1$
					actionClassName), e);
		}

		if (!(actionObject instanceof PluginAction)) {
			throw new PluginException(String.format(
					"Se ha establecido un boton que no define una accion de tipo %1s", //$NON-NLS-1$
					PluginAction.class.getName()));
		}

		return (PluginAction) actionObject;
	}
}
