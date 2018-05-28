package es.gob.afirma.standalone.plugins;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.List;
import java.util.ServiceLoader;

/**
 * Clase encargada de cargar y mantener los objetos correspondientes a un plugin.<br/>
 * Esta clase permite cargar una colecci&oacute;n de archivos JARs entre los que
 * se buscar&acute;n uno o m&acute;s plugins. Los plugins podr&aacute;n hacer uso
 * de las clases de las bibliotecas que los acompa&ntilde;en, pero si estas tienen
 * clases que ya existen en la aplicaci&oacute;n, se priorizar&aacute;n las de la
 * aplicaci&oacute;n (comportamiento por defecto del ClassLoader).
 */
public class PluginLoader {

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
	static AfirmaPlugin loadPlugin(File[] jars) throws IOException, PluginException {

		// Cargamos los JAR
		final List<URL> urls = new ArrayList<>();
		for (final File jar : jars) {
			urls.add(jar.toURI().toURL());
		}
		final URLClassLoader classLoader = new URLClassLoader(urls.toArray(new URL[urls.size()]), PluginLoader.class.getClassLoader());

		// Cargamos las clases de plugin
		final List<AfirmaPlugin> plugins = new ArrayList<>();
		final ServiceLoader<AfirmaPlugin> loader = ServiceLoader.load(AfirmaPlugin.class, classLoader);
		for (final AfirmaPlugin plugin : loader) {
		    plugins.add(plugin);
		}

		if (plugins.size() == 0) {
			throw new PluginException("No se encontro ningun plugin en los archivos");
		}

		if (plugins.size() > 1) {
			throw new PluginException("No se permite la carga simulatea de varios plugins");
		}

		return plugins.get(0);
	}
}
