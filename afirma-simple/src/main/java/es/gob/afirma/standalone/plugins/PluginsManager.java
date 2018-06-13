package es.gob.afirma.standalone.plugins;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.standalone.AutoFirmaUtil;

/**
 * Clase de gesti&oacute;n de plugins.
 */
public class PluginsManager {

	private static final String PLUGINS_DIRNAME = "plugins"; //$NON-NLS-1$

	private static final String PLUGIN_RELATION_FILENAME = "installed"; //$NON-NLS-1$

	private static final String RELATIONS_SEPARATOR = ":"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Extensi&oacute;n de fichero asociada a los plugins de la aplicaci&oacute;n. */
	public static final String[] PLUGIN_EXTENSIONS = new String[] { "jar", "zip" }; //$NON-NLS-1$ //$NON-NLS-2$

	private List<AfirmaPlugin> pluginsLoadedList = null;

	private static PluginsManager instance = null;

	/**
	 * Obtiene una instancia del gestor de plugins.
	 * @return Gestor de plugins.
	 */
	public static PluginsManager getInstance() {
		if (instance == null) {
			instance = new PluginsManager();
		}
		return instance;
	}

	private PluginsManager() {
		// Impedimos que se puedan crear objetos desde fuera de la clase
	}

	/**
	 * Obtiene el listado con la informacion de los plugins instalados y cargados.
	 * Si los plugins no estan cargados en memoria, lo hace.
	 * @return Listado de plugins.
	 * @throws PluginException Cuando no se han podido cargar los plugins.
	 */
	public List<AfirmaPlugin> getPluginsLoadedList() throws PluginException {
		return getPluginsLoadedList(false);
	}

	/**
	 * Obtiene el listado con la informacion de los plugins instalados y cargados.
	 * Si los plugins no estan cargados en memoria, lo hace.
	 * @param force Indica si se debe obligar a recargar la lista de plugins.
	 * @return Listado de plugins.
	 * @throws PluginException Cuando no se han podido cargar los plugins.
	 */
	public List<AfirmaPlugin> getPluginsLoadedList(boolean force) throws PluginException {
		if (this.pluginsLoadedList == null || force) {
			this.pluginsLoadedList = loadPlugins();
		}

		final ArrayList<AfirmaPlugin> tempList = new ArrayList<>();
		for (final AfirmaPlugin item : this.pluginsLoadedList) {
			tempList.add(item);
		}
		return tempList;
	}

	private static List<AfirmaPlugin> loadPlugins() throws PluginException {

		final List<AfirmaPlugin> list = new ArrayList<>();

		// Cargamos los plugins que haya configurados
		final File relationFile = new File(getPluginsDir(), PLUGIN_RELATION_FILENAME);
		if (relationFile.isFile() && relationFile.canRead()) {
			MinimalPluginInfo[] installedPlugins;
			try {
				installedPlugins = loadPluginsList(getRelationFile());
			}
			catch (final Exception e) {
				throw new PluginException("Error al cargar el listado de plugins", e); //$NON-NLS-1$
			}
			for (final MinimalPluginInfo info : installedPlugins) {
				try {
					final AfirmaPlugin plugin = loadPlugin(info);
					list.add(plugin);
				}
				catch (final PluginException e) {
					LOGGER.warning(String.format("No se ha podido cargar la informacion del plugin %s. Se eliminara del listado", info.getInternalName())); //$NON-NLS-1$
				}
			}

			try {
				savePluginsList(relationFile, list);
			} catch (final IOException e) {
				throw new PluginException("No se pudo guardar la lista de plugins cargados", e); //$NON-NLS-1$
			}
		}

		return list;
	}

	private static File getRelationFile() {
		return new File(getPluginsDir(), PLUGIN_RELATION_FILENAME);
	}

	private static MinimalPluginInfo[] loadPluginsList(File relationFile) throws IOException {

		if (!relationFile.isFile() || !relationFile.canRead()) {
			return new MinimalPluginInfo[0];
		}

		final List<MinimalPluginInfo> list = new ArrayList<>();

		try (	FileInputStream fis = new FileInputStream(relationFile);
				final InputStreamReader isr = new InputStreamReader(fis);
				final BufferedReader reader = new BufferedReader(isr); ) {
			String line;
			do {
				line = reader.readLine();
				if (line != null && !line.trim().isEmpty()) {
					try {
						final String[] pluginData = line.trim().split(RELATIONS_SEPARATOR);
						list.add(new MinimalPluginInfo(pluginData[0], Integer.parseInt(pluginData[1])));
					}
					catch (final Exception e) {
						LOGGER.warning(String.format("No se puede cargar el plugin correspondiente a la linea %s. Se descartara.", line)); //$NON-NLS-1$
					}
				}
			} while (line != null);
		}
		catch (final Exception e) {
			throw new IOException("No se pudo leer el listado de plugins instalados", e); //$NON-NLS-1$
		}

		return list.toArray(new MinimalPluginInfo[list.size()]);
	}

	private static AfirmaPlugin loadPlugin(final MinimalPluginInfo info) throws PluginException {

		final File pluginDir = new File(getPluginsDir(), info.getInternalName());
		if (!pluginDir.isDirectory()) {
			throw new PluginException("No se ha encontrado el plugin " + pluginDir.getName()); //$NON-NLS-1$
		}

		final File[] pluginFiles = pluginDir.listFiles(new FileFilter() {
			@Override public boolean accept(File pathname) {
				return pathname.isFile() && pathname.canRead();
			}
		});
		if (pluginFiles == null || pluginFiles.length == 0) {
			throw new PluginException("No se han encontrado los ficheros del plugin " + info.getInternalName()); //$NON-NLS-1$
		}

		return loadPluginFromFiles(pluginFiles);
	}

	/**
	 * Guarda el listado de plugins en el fichero de plugins instalados.
	 * @param relationsFile Fichero de plugins instalados.
	 * @param infos Listado de plugins cargados.
	 * @throws IOException Cuando ocurre un error al guardar el listado de plugins.
	 */
	private static void savePluginsList(File relationsFile, List<AfirmaPlugin> plugins)
			throws IOException {

		try (final FileOutputStream fos = new FileOutputStream(relationsFile);) {
			for (final AfirmaPlugin plugin : plugins) {
				final PluginInfo info = plugin.getInfo();
				final String line = info.getInternalName() + RELATIONS_SEPARATOR +
						info.getVersionCode() + "\n"; //$NON-NLS-1$
				fos.write(line.getBytes(StandardCharsets.UTF_8));
			}
			fos.flush();
		}

	}

	/**
	 * Comprueba que un fichero se corresponda con un plugin compatible.
	 * @param files Ficheros del plugin a evaluar.
	 * @return Plugins cargados desde los archivos indicados o {@code null} si no
	 * se trata de ficheros de un plugin.
	 */
	public static AfirmaPlugin loadPluginFromFiles(File[] files) {

		AfirmaPlugin plugin;
		try {
			plugin = PluginLoader.loadPlugin(files);
		} catch (final IOException e) {
			LOGGER.log(Level.SEVERE, "Error en la carga de un conjunto de ficheros de plugin", e); //$NON-NLS-1$
			return null;
		} catch (final PluginException e) {
			LOGGER.log(Level.SEVERE, "El plugin importado no es valido", e); //$NON-NLS-1$
			return null;
		}

		return plugin;
	}

	/**
	 * Descarga un plugin de memoria siempre y cuando se haya cargado desde fichero.
	 * @param plugin Plugin a descargar.
	 */
	public static void closePlugin(AfirmaPlugin plugin) {
		final ClassLoader classloader = plugin.getClass().getClassLoader();
		if (classloader instanceof URLClassLoader) {
			try {
				((URLClassLoader) classloader).close();
			} catch (final IOException e) {
				LOGGER.log(Level.WARNING, "No se pudo descargar un plugin de memoria", e); //$NON-NLS-1$
			}
		}
	}

	/**
	 * Importa un plugin al directorio de plugins de la aplicaci&oacute;n.
	 * @param pluginFile Archivo con el plugin.
	 * @param pluginName Nombre del plugin a instalar.
	 * @return Plugin reci&eacute;n instalado.
	 * @throws PluginInstalledException Cuando el plugin ya se encontraba instalado.
	 * @throws PluginException Cuando ocurre un error en la instalaci&oacute;n del plugin.
	 * @throws IOException Cuando ocurre un error en la carga del fichero o creaci&oacute;n
	 * del directorio de plugins.
	 * @throws PluginControlledException Cuando se produce un error en la instalaci&oacute;n
	 * emitido por el propio plugin.
	 */
	public AfirmaPlugin installPlugin(File pluginFile, String pluginName) throws PluginInstalledException, PluginException, IOException, PluginControlledException {

		// Obtenemos el directorio de plugins
		final File pluginsDir = getPluginsDir();
		if (!pluginsDir.isDirectory()) {
			if (!pluginsDir.mkdir()) {
				throw new IOException("No se ha podido crear el directorio interno de plugins"); //$NON-NLS-1$
			}
		}

		// Creamos un directorio para el nuevo plugin
		final File pluginDir = new File(pluginsDir, pluginName);
		if (pluginDir.exists()) {
			throw new PluginInstalledException("El plugin seleccionado ya se encuentra instalado"); //$NON-NLS-1$
		}
		pluginDir.mkdir();

		// Copiamos los JAR del plugin a su directorio
		final File outPluginFile = new File(pluginDir, pluginFile.getName());
		try (OutputStream fos = new FileOutputStream(outPluginFile)) {
			Files.copy(pluginFile.toPath(), fos);
		}

		// Cargamos el nuevo plugin
		AfirmaPlugin plugin;
		try {
			plugin = loadPluginFromFiles(new File[] { outPluginFile });
		}
		catch (final Exception e) {
			deleteDirectory(pluginDir);
			throw new PluginException("No se pudo cargar el plugin recien importado", e); //$NON-NLS-1$
		}

		// Ejecutamos la operacion de instalacion del propio plugin
		try {
			plugin.install();
		}
		catch (final Exception e) {
			LOGGER.log(Level.SEVERE, "Ocurrio un error al instalar el plugin. Lo desinstalamos", e); //$NON-NLS-1$
			try {
				uninstallPlugin(plugin);
			}
			catch (final Exception ex) {
				LOGGER.warning("No se han podido eliminar los ficheros importados del plugin"); //$NON-NLS-1$
			}
			if (e instanceof PluginControlledException) {
				throw (PluginControlledException) e;
			}
			throw new PluginException("Ocurrio un error al instalar el plugin", e); //$NON-NLS-1$
		}

		// Agregamos el plugin a la lista de plugins instalados
		addPluginToRelationsFile(plugin.getInfo());

		// Agregamos el plugin a la lista de plugins cargados
		if (this.pluginsLoadedList == null) {
			this.pluginsLoadedList = new ArrayList<>();
		}
		this.pluginsLoadedList.add(plugin);

		return plugin;
	}

	private static void addPluginToRelationsFile(MinimalPluginInfo info) throws IOException {

		final File pluginsDir = getPluginsDir();
		if (!pluginsDir.isDirectory()) {
			if (!pluginsDir.mkdir()) {
				throw new IOException("No se ha podido crear el directorio interno de plugins"); //$NON-NLS-1$
			}
		}

		final byte[] pluginRegistry = (info.getInternalName() + RELATIONS_SEPARATOR +
				info.getVersionCode() + "\n").getBytes(StandardCharsets.UTF_8); //$NON-NLS-1$

		final File relationsFile = new File(pluginsDir, PLUGIN_RELATION_FILENAME);
		if (!relationsFile.isFile()) {
			try (FileOutputStream fos = new FileOutputStream(relationsFile)) {
				fos.write(pluginRegistry);
				fos.flush();
			}
		}
		else {
			Files.write(relationsFile.toPath(), pluginRegistry, StandardOpenOption.APPEND);
		}
	}

	/**
	 * Elimina de memoria y borra del directorio de plugins un plugins concreto.
	 * @param plugin Informaci&oacute;n del plugin a eliminar.
	 * @throws IOException Cuando no se puede eliminar el plugin.
	 */
	public void uninstallPlugin(AfirmaPlugin plugin) throws IOException {

		// Ejecutamos el proceso de desinstalacion del propio plugin
		try {
			plugin.uninstall();
		} catch (final Exception e) {
			LOGGER.log(Level.WARNING, "El proceso de desinstalacion interno del plugin devolvio un error", e); //$NON-NLS-1$
		}

		// Descargamos el plugin de memoria
		final String internalName = plugin.getInfo().getInternalName();
		this.pluginsLoadedList.remove(plugin);
		((URLClassLoader) plugin.getClass().getClassLoader()).close();

		// Identificamos el directorio del plugin y lo eliminamos
		final File pluginsDir = getPluginsDir();
		if (!pluginsDir.isDirectory()) {
			LOGGER.warning("No existe el directorio de plugins, asi que no deberia haber ninguno instalado"); //$NON-NLS-1$
			return;
		}

		final File pluginDir = new File(pluginsDir, internalName);
		if (!pluginDir.exists()) {
			LOGGER.warning("El plugin seleccionado, no se encuentra instalado"); //$NON-NLS-1$
			return;
		}

		deleteDirectory(pluginDir);

		final File relationFile = new File(pluginsDir, PLUGIN_RELATION_FILENAME);

		savePluginsList(relationFile, this.pluginsLoadedList);
	}

	/**
	 * Elimina los ficheros de un plugin sin tener consideracion de si esta cargado o no.
	 * @param pluginName Nombre interno del plugin que se desea eliminar.
	 * @throws IOException Si ocurre un error durante la eliminaci&oacute;n.
	 */
	public static void forceRemove(String pluginName) throws IOException {
		final File pluginsDir = getPluginsDir();
		final File pluginDir = new File(pluginsDir, pluginName);
		if (!pluginDir.exists()) {
			LOGGER.warning("El plugin seleccionado, no se encuentra instalado"); //$NON-NLS-1$
			return;
		}
		deleteDirectory(pluginDir);
	}

	/**
	 * Elimina un directorio con todo su contenido.
	 * @param dir Directorio que se desea eliminar.
	 * @throws IOException Cuando se produce un error al eliminar el directorio
	 * o cualquiera de los ficheros que contiene.
	 */
	private static void deleteDirectory(File dir) throws IOException {

		for (final File file : dir.listFiles()) {
			if (file.isFile()) {
				Files.delete(file.toPath());
			}
			else {
				deleteDirectory(file);
			}
		}
		Files.delete(dir.toPath());
	}

	private static File getPluginsDir() {
		File appDir = AutoFirmaUtil.getAlternativeDirectory();
		if (appDir == null) {
			appDir = AutoFirmaUtil.getApplicationDirectory();
		}
		return new File(appDir, PLUGINS_DIRNAME);
	}
}
