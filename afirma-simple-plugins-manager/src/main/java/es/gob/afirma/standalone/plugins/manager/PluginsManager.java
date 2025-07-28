package es.gob.afirma.standalone.plugins.manager;

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

import es.gob.afirma.core.misc.AOFileUtils;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.standalone.plugins.AfirmaPlugin;
import es.gob.afirma.standalone.plugins.MinimalPluginInfo;
import es.gob.afirma.standalone.plugins.Permission;
import es.gob.afirma.standalone.plugins.PluginControlledException;
import es.gob.afirma.standalone.plugins.PluginInfo;

/**
 * Clase de gesti&oacute;n de plugins.
 */
public class PluginsManager {

	private static final String PLUGIN_RELATION_FILENAME = "installed"; //$NON-NLS-1$

	private static final String RELATIONS_SEPARATOR = ":"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private List<AfirmaPlugin> pluginsLoadedList = null;

	private File pluginsDir = null;

	public PluginsManager(final File pluginsDir) {
		this.pluginsDir = pluginsDir;
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
	public List<AfirmaPlugin> getPluginsLoadedList(final boolean force) throws PluginException {
		if (this.pluginsLoadedList == null || force) {
			this.pluginsLoadedList = loadPlugins();
		}

		return new ArrayList<>(this.pluginsLoadedList);
	}

	private List<AfirmaPlugin> loadPlugins() throws PluginException {

		final List<AfirmaPlugin> list = new ArrayList<>();

		// Cargamos los plugins que haya configurados
		final File relationFile = new File(this.pluginsDir, PLUGIN_RELATION_FILENAME);

		if (relationFile.isFile() && relationFile.canRead()) {
			MinimalPluginInfo[] installedPlugins;
			try {
				installedPlugins = loadPluginsList(relationFile);
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
					LOGGER.log(Level.WARNING, String.format("No se ha podido cargar la informacion del plugin %s. Se eliminara del listado", info.getInternalName()), e); //$NON-NLS-1$
				}
			}
		}

		return list;
	}

	private static MinimalPluginInfo[] loadPluginsList(final File relationFile) throws IOException {

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

	private AfirmaPlugin loadPlugin(final MinimalPluginInfo info) throws PluginException {

		final File pluginDir = new File(this.pluginsDir, info.getInternalName());
		if (!pluginDir.isDirectory()) {
			throw new PluginException("No se ha encontrado el plugin " + info.getInternalName()); //$NON-NLS-1$
		}

		final File[] pluginFiles = pluginDir.listFiles((FileFilter) pathname -> pathname.isFile() && pathname.canRead());
		if (pluginFiles == null || pluginFiles.length == 0) {
			throw new PluginException("No se han encontrado los ficheros del plugin " + info.getInternalName()); //$NON-NLS-1$
		}

		return loadPluginFromFiles(pluginFiles);
	}

	/**
	 * Guarda el listado de plugins en el fichero de plugins instalados.
	 * @param relationsFile Fichero de plugins instalados.
	 * @param plugins Listado de plugins cargados.
	 * @throws IOException Cuando ocurre un error al guardar el listado de plugins.
	 */
	private static void savePluginsList(final File relationsFile, final List<AfirmaPlugin> plugins)
			throws IOException {

		// Si hay plugins en el listado, actualizamos la relacion de plugins
		if (!plugins.isEmpty()) {
			try (final FileOutputStream fos = new FileOutputStream(relationsFile);) {
				for (final AfirmaPlugin plugin : plugins) {
					final PluginInfo info = plugin.getInfo();
					final String line = info.getInternalName() + RELATIONS_SEPARATOR +
							info.getVersionCode() + "\n"; //$NON-NLS-1$
					fos.write(line.getBytes(StandardCharsets.UTF_8));
				}
				fos.flush();
			}
			AOFileUtils.setAllPermissions(relationsFile);
		}
		// Si ya no hay plugins, eliminamos la relacion y el directorio de plugins
		else {
			deleteDirectoryAndContent(relationsFile.getParentFile());
		}
	}

	/**
	 * Comprueba que un fichero se corresponda con un plugin compatible.
	 * @param files Ficheros del plugin a evaluar.
	 * @return Plugins cargados desde los archivos indicados.
	 * @throws PluginException Cuando no se ha podido cargar el plugin.
	 */
	public static AfirmaPlugin loadPluginFromFiles(final File[] files) throws PluginException {

		AfirmaPlugin plugin;
		try {
			plugin = PluginLoader.loadPlugin(files);
		} catch (final IOException e) {
			LOGGER.log(Level.SEVERE, "Error en la carga de un conjunto de ficheros de plugin", e); //$NON-NLS-1$
			throw new PluginException("Error en la carga de un conjunto de ficheros de plugin",  e); //$NON-NLS-1$
		} catch (final PluginException e) {
			LOGGER.log(Level.SEVERE, "El plugin importado no es valido", e); //$NON-NLS-1$
			throw e;
		}

		return plugin;
	}

	/**
	 * Descarga un plugin de memoria siempre y cuando se haya cargado desde fichero.
	 * @param plugin Plugin a descargar.
	 */
	public static void closePlugin(final AfirmaPlugin plugin) {
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
	public AfirmaPlugin installPlugin(final File pluginFile, final String pluginName) throws PluginInstalledException, PluginException, IOException, PluginControlledException {

		// Copiamos el plugin al directorio de plugins
		final File outPluginFile = copyPluginToDirectory(pluginFile, pluginName, this.pluginsDir);

		// Cargamos el nuevo plugin
		AfirmaPlugin plugin;
		try {
			plugin = loadPluginFromFiles(new File[] { outPluginFile });
		}
		catch (final Exception e) {
			deleteDirectoryAndContent(outPluginFile.getParentFile());
			throw new PluginException("No se pudo cargar el plugin recien importado", e); //$NON-NLS-1$
		}

		final PluginInfo info = plugin.getInfo();

		// Si el plugin tenia el permiso de instalacion, se ejecuta su operacion
		// de instalacion
		if (PermissionChecker.check(info, Permission.INSTALL)) {
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

	/**
	 * Copia un fichero de plugin al directorio de plugins de manera corriente, presuponiendo que se dispone
	 * de permisos en el directorio de plugins y su directorio padre. Si no existiese el directorio de plugins,
	 * se crear&oacute;a.
	 * @param pluginFile Fichero de plugin.
	 * @param pluginName Nombre del plugin.
	 * @param pluginsDir Directorio de plugins.
	 * @return Fichero del plugin ya instalado en el directorio.
	 * @throws IOException Cuando ocurre un error de permisos o durante la copia.
	 * @throws PluginInstalledException Cuando el plugin ya existe.
	 */
	private static File copyPluginToDirectory(final File pluginFile, final String pluginName, final File pluginsDir) throws IOException, PluginInstalledException {

		// Creamos el directorio de plugins si es preciso
		if (!pluginsDir.isDirectory()) {
			try {
				if (!pluginsDir.getParentFile().exists()) {
					createDirWithPermissions(pluginsDir.getParentFile());
				}
				createDirWithPermissions(pluginsDir);
			}
			catch (final Exception e) {
				throw new IOException("No se ha podido crear el directorio interno de plugins: " + LoggerUtil.getCleanUserHomePath(pluginsDir.getAbsolutePath()), e); //$NON-NLS-1$
			}
		}

		// Creamos un directorio para el nuevo plugin
		final File pluginDir = new File(pluginsDir, pluginName);
		if (pluginDir.exists()) {
			throw new PluginInstalledException("El plugin seleccionado ya se encuentra instalado"); //$NON-NLS-1$
		}
		try {
			createDirWithPermissions(pluginDir);
		}
		catch (final Exception e) {
			throw new IOException("No se ha podido crear el directorio para la copia del plugin: " + LoggerUtil.getCleanUserHomePath(pluginDir.getAbsolutePath()), e); //$NON-NLS-1$
		}

		// Copiamos los JAR del plugin a su directorio
		final File outPluginFile = new File(pluginDir, pluginFile.getName());
		try (OutputStream fos = new FileOutputStream(outPluginFile)) {
			Files.copy(pluginFile.toPath(), fos);
		}
		AOFileUtils.setAllPermissions(outPluginFile);
		return outPluginFile;
	}

	/**
	 * Agrega la referencia a un plugin instalado en la relaci&oacute;n de plugins de Autofirma para
	 * que lo cargue a partir de ahora.
	 * @param info Informaci&oacute;n del plugin necesaria para el registro.
	 * @throws IOException Cuando no se pueda agregar la referencia.
	 */
	private void addPluginToRelationsFile(final MinimalPluginInfo info) throws IOException {

		final byte[] pluginRegistry = (info.getInternalName() + RELATIONS_SEPARATOR +
				info.getVersionCode() + "\n").getBytes(StandardCharsets.UTF_8); //$NON-NLS-1$

		final File relationsFile = new File(this.pluginsDir, PLUGIN_RELATION_FILENAME);
		if (!relationsFile.isFile()) {
			try (FileOutputStream fos = new FileOutputStream(relationsFile)) {
				fos.write(pluginRegistry);
				fos.flush();
			}
			AOFileUtils.setAllPermissions(relationsFile);
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
	public void uninstallPlugin(final AfirmaPlugin plugin) throws IOException {

		final PluginInfo info = plugin.getInfo();

		LOGGER.info("Se desinstala el plugin: " + info.getName()); //$NON-NLS-1$

		// Ejecutamos el proceso de desinstalacion del plugin si este lo tenia declarado
		if (PermissionChecker.check(info, Permission.INSTALL)) {

			LOGGER.info("Se ejecuta la funcion de desinstalacion del plugin"); //$NON-NLS-1$
			try {
				plugin.uninstall();
			} catch (final Exception e) {
				LOGGER.log(Level.WARNING, "El proceso de desinstalacion interno del plugin devolvio un error", e); //$NON-NLS-1$
			}
		}

		// Descargamos el plugin de memoria
		final String internalName = info.getInternalName();
		this.pluginsLoadedList.remove(plugin);
		((URLClassLoader) plugin.getClass().getClassLoader()).close();

		// Identificamos el directorio del plugin y lo eliminamos
		if (!this.pluginsDir.isDirectory()) {
			LOGGER.warning("No existe el directorio de plugins, asi que no deberia haber ninguno instalado"); //$NON-NLS-1$
			return;
		}

		final File pluginDir = new File(this.pluginsDir, internalName);
		if (!pluginDir.exists()) {
			LOGGER.warning("El plugin seleccionado, no se encuentra instalado"); //$NON-NLS-1$
			return;
		}

		deleteDirectoryAndContent(pluginDir);

		final File relationFile = new File(this.pluginsDir, PLUGIN_RELATION_FILENAME);

		savePluginsList(relationFile, this.pluginsLoadedList);
	}

	/**
	 * Elimina los ficheros de un plugin sin tener consideracion de si esta cargado o no.
	 * @param pluginName Nombre interno del plugin que se desea eliminar.
	 * @throws IOException Si ocurre un error durante la eliminaci&oacute;n.
	 */
	public void forceRemove(final String pluginName) throws IOException {
		final File pluginDir = new File(this.pluginsDir, pluginName);
		if (!pluginDir.exists()) {
			LOGGER.warning("El plugin seleccionado, no se encuentra instalado"); //$NON-NLS-1$
			return;
		}
		deleteDirectoryAndContent(pluginDir);
	}

	/**
	 * Elimina un directorio con todo su contenido.
	 * @param dir Directorio que se desea eliminar.
	 * @throws IOException Cuando se produce un error al eliminar el directorio
	 * o cualquiera de los ficheros que contiene.
	 */
	private static void deleteDirectoryAndContent(final File dir) throws IOException {

		for (final File file : dir.listFiles()) {
			if (file.isFile()) {
				Files.delete(file.toPath());
			}
			else {
				deleteDirectoryAndContent(file);
			}
		}
		Files.delete(dir.toPath());
	}

	/**
	 * Crea un directorio y sus subdirectorios y le concede permisos de lectura, escritura y ejecucion a todos los usuarios.
	 * @param dir Directorio.
	 * @throws IOException Cuando falla la creaci&oacute;n del directorio.
	 */
	private static void createDirWithPermissions(final File dir) throws IOException {

		if (!dir.mkdirs()) {
			throw new IOException("No se pudo crear el directorio: " + LoggerUtil.getCleanUserHomePath(dir.getAbsolutePath())); //$NON-NLS-1$
		}
		AOFileUtils.setAllPermissions(dir);
	}
}

