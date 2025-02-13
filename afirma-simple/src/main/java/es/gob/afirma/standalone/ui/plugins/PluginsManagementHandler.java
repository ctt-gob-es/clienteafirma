package es.gob.afirma.standalone.ui.plugins;

import java.awt.Window;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.DefaultListModel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.ListModel;
import javax.swing.SwingUtilities;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.DesktopUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.plugins.AfirmaPlugin;
import es.gob.afirma.standalone.plugins.Permission;
import es.gob.afirma.standalone.plugins.PluginControlledException;
import es.gob.afirma.standalone.plugins.PluginInfo;
import es.gob.afirma.standalone.plugins.manager.JarNoSignedException;
import es.gob.afirma.standalone.plugins.manager.JarVerifier;
import es.gob.afirma.standalone.plugins.manager.PermissionChecker;
import es.gob.afirma.standalone.plugins.manager.PluginException;
import es.gob.afirma.standalone.plugins.manager.PluginInstalledException;
import es.gob.afirma.standalone.plugins.manager.PluginsManager;
import es.gob.afirma.standalone.plugins.manager.PluginsPreferences;
import es.gob.afirma.standalone.so.macos.MacUtils;

/**
 * Manegador de eventos de PluginsManagementPanel.
 */
public class PluginsManagementHandler implements KeyListener, ListSelectionListener {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$


	/** Extensi&oacute;n de fichero asociada a los plugins de la aplicaci&oacute;n. */
	private static final String[] PLUGIN_EXTENSIONS = { "jar", "zip" }; //$NON-NLS-1$ //$NON-NLS-2$

	private final PluginsManagementPanel view;
	private final PluginsManager pluginsManager;

	private List<AfirmaPlugin> pluginsList;

	/**
	 * Construye el objeto para la gesti&oacute;n de los eventos del di&aacute;logo de
	 * gesti&oacute;n de plugins.
	 * @param view Panel sobre en el que encuentran los componentes que gestinar.
	 * @param pluginsManager Administrador de plugins.
	 */
	public PluginsManagementHandler(final PluginsManagementPanel view, final PluginsManager pluginsManager) {
		this.view = view;
		this.pluginsManager = pluginsManager;
	}

	/**
	 * Establece el comportamiento sobre los componentes del panel.
	 */
	void registerComponents() {

		// Listado de plugins
		this.view.getPluginsList().addKeyListener(this);
		this.view.getPluginsList().addListSelectionListener(this);

		// Boton para agregar un nuevo plugin
		this.view.getAddButton().addKeyListener(this);
		this.view.getAddButton().addActionListener(
			ae -> addPlugin()
		);

		// Boton para eliminar un plugin
		this.view.getRemoveButton().addKeyListener(this);
		this.view.getRemoveButton().addActionListener(
			ae -> removePlugin()
		);

		// Boton para configurar un plugin
		this.view.getConfigButton().addKeyListener(this);
		this.view.getConfigButton().addActionListener(
			ae -> configPlugin()
		);

		// Boton de cierre del dialogo
		this.view.getCloseButton().addKeyListener(this);
		this.view.getCloseButton().addActionListener(
			ae -> this.view.getParentWindow().dispose()
		);
	}

	/** Importa y agrega al listado un nuevo plugin. */
	void addPlugin() {

		// Cargamos el fichero de plugin
		final File pluginFile;
		try {
			pluginFile = selectPluginFile();
		}
		catch (final AOCancelledOperationException e) {
			return;
		}

		// Comprobamos que el JAR este correctamente firmado y mostramos un dialogo
		// de advertencia si no lo esta y de informacion en caso de estarlo
		final boolean allowed = verifyJar(pluginFile);
		if (!allowed) {
			return;
		}

		// Comprobamos que el plugin sea valido
		final AfirmaPlugin plugin;
		try {
			plugin = PluginsManager.loadPluginFromFiles(new File[] { pluginFile });
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se pudo cargar el plugin", e); //$NON-NLS-1$
			showError(SimpleAfirmaMessages.getString("PluginsManagementHandler.0"), null); //$NON-NLS-1$
			return;
		}

		final PluginInfo info = plugin.getInfo();

		PluginsManager.closePlugin(plugin);

		// Mostramos los permisos requeridos por el plugin y no lo instalamos salvo
		// que se acepten
		if (info.getPermissions() != null && info.getPermissions().length > 0) {
			final boolean accepted = showPermissionDialog(info, this.view.getParentWindow());
			if (!accepted) {
				return;
			}
		}

		// Lo importamos a la aplicacion
		addPlugin(pluginFile, info);
	}

	/**
	 * Muestra el di&aacute;logo de solicitud de permisos.
	 * @param info Informaci&oacute;n del plugin con el listado de permisos.
	 * @param parent Componente sobre el que mostrar el di&aacute;logo.
	 * @return {@code true} si el usuario concedi&oacute; los permisos al plugin,
	 * {@code false} en caso contrario.
	 */
	private static boolean showPermissionDialog(final PluginInfo info, final Window parent) {

		final PermissionsDialog dialog = new PermissionsDialog(info, parent);
		dialog.setVisible(true);

		return dialog.isAccepted();
	}

	private void addPlugin(final File pluginFile, final PluginInfo info) {

		// Copiamos el plugin al subdirectorio correspondiente dentro del
		// directorio de instalacion
		AfirmaPlugin plugin;
		try {
			plugin = this.pluginsManager.installPlugin(pluginFile, info.getInternalName());
		}
		catch (final PluginControlledException e) {
			LOGGER.log(Level.WARNING, "El propio plugin devolvio un error durante su instalacion", e); //$NON-NLS-1$
			showError(e.getLocalizedMessage(), e);
			return;
		}
		catch (final PluginInstalledException e) {
			LOGGER.log(Level.WARNING, "Ya existe una version instalada del plugin"); //$NON-NLS-1$
			// Preguntamos si se desea susituir la version instalada del plugin por la nueva
			final int option = JOptionPane.showConfirmDialog(
					this.view,
					String.format(SimpleAfirmaMessages.getString("PluginsManagementHandler.1"), info.getName()), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("PluginsManagementHandler.16"), //$NON-NLS-1$
					JOptionPane.YES_NO_CANCEL_OPTION);
			if (option != JOptionPane.YES_OPTION) {
				return;
			}
			try {
				removeLoadedPlugin(info);
				plugin = this.pluginsManager.installPlugin(pluginFile, info.getInternalName());
			}
			catch (final Exception e2) {
				LOGGER.log(Level.WARNING, "No se ha podido reemplazar la version preexistente del plugin", e2); //$NON-NLS-1$
				showError(SimpleAfirmaMessages.getString("PluginsManagementHandler.15"), e2); //$NON-NLS-1$
				return;
			}
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "Ocurrio un error al instalar el plugin", e); //$NON-NLS-1$
			showError(SimpleAfirmaMessages.getString("PluginsManagementHandler.2"), e); //$NON-NLS-1$
			return;
		}

		// Si el plugin requiere reiniciar, reiniciamos;
		if (PermissionChecker.check(plugin.getInfo(), Permission.RESET)) {
			resetApplication();
		}

		// Mostramos la informacion del plugin
		showPluginInfo(plugin);
	}

	private boolean verifyJar(final File pluginFile) {

		List<X509Certificate[]> certs = null;
		try {
			certs = JarVerifier.verify(pluginFile);
		}
		catch (final JarNoSignedException e) {
			LOGGER.log(Level.WARNING, "Se han encontrado entradas del plugin sin firmar", e); //$NON-NLS-1$
			final int option = JOptionPane.showConfirmDialog(
					this.view,
					SimpleAfirmaMessages.getString("PluginsManagementHandler.20"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("PluginsManagementHandler.21"), //$NON-NLS-1$
					JOptionPane.YES_NO_OPTION,
					JOptionPane.WARNING_MESSAGE);
			return option == JOptionPane.YES_OPTION;
		}
		catch (final SecurityException e) {
			LOGGER.log(Level.WARNING, "Se han encontrado problemas en la firma del plugin", e); //$NON-NLS-1$
			final int option = JOptionPane.showConfirmDialog(
					this.view,
					SimpleAfirmaMessages.getString("PluginsManagementHandler.22"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("PluginsManagementHandler.23"), //$NON-NLS-1$
					JOptionPane.YES_NO_OPTION,
					JOptionPane.ERROR_MESSAGE);
			return option == JOptionPane.YES_OPTION;
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "Ocurrio un error durante la lectura del fichero de plugin", e); //$NON-NLS-1$
			showError(SimpleAfirmaMessages.getString("PluginsManagementHandler.24"), e); //$NON-NLS-1$
			return false;
		}

		// Mostramos un dialogo en el que pedimos al usuario que confirme que confia en el firmante
		// del plugin
		final int option = JOptionPane.showConfirmDialog(
				this.view,
				new CertificateConfirmPanel(certs.toArray(new X509Certificate[0][])),
				SimpleAfirmaMessages.getString("PluginsManagementHandler.25"), //$NON-NLS-1$
				JOptionPane.YES_NO_OPTION);
		return option == JOptionPane.YES_OPTION;
	}

	/**
	 * Carga un fichero de plugin.
	 * @return Fichero de plugin.
	 */
	private File selectPluginFile() {
		final File[] files = AOUIFactory.getLoadFiles(
				SimpleAfirmaMessages.getString("PluginsManagementHandler.3"), //$NON-NLS-1$
				null,
				null,
				PLUGIN_EXTENSIONS,
				SimpleAfirmaMessages.getString("PluginsManagementHandler.4"), //$NON-NLS-1$
				false,
				false,
				DesktopUtil.getDefaultDialogsIcon(),
				this.view);

		return files[0];
	}

	private void showPluginInfo(final AfirmaPlugin plugin) {

		// Actualizamos la lista con los plugins cargados
		final JList<AfirmaPlugin> list = this.view.getPluginsList();
		final DefaultListModel<AfirmaPlugin> listModel = (DefaultListModel<AfirmaPlugin>) list.getModel();
		listModel.removeAllElements();

		List<AfirmaPlugin> loadedPlugins;
		try {
			loadedPlugins = this.pluginsManager.getPluginsLoadedList();
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se pudo cargar la nueva lista de plugins cargados, se usara la anterior ", e); //$NON-NLS-1$
			loadedPlugins = this.pluginsList;
		}
		for (final AfirmaPlugin pluginItem : loadedPlugins) {
			listModel.addElement(pluginItem);
		}

		// Seleccionamos el nuevo plugin
		list.setSelectedValue(plugin, true);

		// Mostramos la informacion del plugin en el panel lateral
		showPluginDetails(plugin.getInfo());
	}

	/**
	 * Elimina una instalaci&oacute;n de un plugin que probablemente este cargado en memoria.
	 * @param info Informaci&oacute;n del plugin a eliminar.
	 * @throws PluginException Cuando no se puede reemplazar la versi&oacute;n
	 * preexistente del plugin.
	 */
	private void removeLoadedPlugin(final PluginInfo info) throws PluginException {

		// Para desinstalar el plugin anterior, antes vemos si esta en la lista de plugins
		AfirmaPlugin previousPlugin = null;
		final ListModel<AfirmaPlugin> model = this.view.getPluginsList().getModel();
		for (int i = 0; i < model.getSize(); i++) {
			if (model.getElementAt(i).getInfo().equals(info)) {
				previousPlugin = model.getElementAt(i);
				break;
			}
		}

		// Si esta en la lista, lo desinstalamos normalmente
		if (previousPlugin != null) {
			try {
				removePlugin(previousPlugin);
			}
			catch (final Exception e) {
				throw new PluginException("Error al eliminar la version preexistente del plugin", e); //$NON-NLS-1$
			}
		}
		// Si no esta, sera un resto residual de un plugin y tendremos que eliminar su directorio
		else {
			try {
				this.pluginsManager.forceRemove(info.getInternalName());
			} catch (final IOException e) {
				throw new PluginException("No se pudo eliminar el directorio residual del plugin anterior", e); //$NON-NLS-1$
			}
		}
	}

	/**
	 * Pide permiso al usuario para eliminar el plugin seleccionado y lo desinstala
	 * en caso de que lo conceda.
	 */
	void removePlugin() {

		// Obtenemos la informacion del plugin seleccionado
		final JList<AfirmaPlugin> list = this.view.getPluginsList();
		final AfirmaPlugin plugin = list.getSelectedValue();
		if (plugin == null) {
			return;
		}

		// Pedimos confirmacion para el borrado
		if (JOptionPane.OK_OPTION != JOptionPane.showConfirmDialog(this.view.getParentWindow(),
				String.format(SimpleAfirmaMessages.getString("PluginsManagementHandler.5"), //$NON-NLS-1$
						plugin.getInfo().getName()))) {
			return;
		}

		// Desinstalamos el plugin
		try {
			removePlugin(plugin);
		} catch (final IOException e) {
			LOGGER.log(Level.SEVERE, "Ocurrio un error al desinstalar el plugin", e); //$NON-NLS-1$
			showError(SimpleAfirmaMessages.getString("PluginsManagementHandler.6"), e); //$NON-NLS-1$
			return;
		}

		// Eliminamos la configuracion almacenada del plugin
		PluginsPreferences.getInstance(plugin).removeConfig();

		// Si el plugin requiere reiniciar, reiniciamos;
		if (PermissionChecker.check(plugin.getInfo(), Permission.RESET)) {
			resetApplication();
		}
	}

	/**
	 * Desinstala un plugin.
	 * @param plugin Plugin a desinstalar.
	 * @throws IOException Cuando ocurre un error durante la desinstalaci&oacute;n.
	 */
	private void removePlugin(final AfirmaPlugin plugin) throws IOException {
		// Desinstalamos el plugin
		this.pluginsManager.uninstallPlugin(plugin);

		// Eliminamos el plugin del listado
		final JList<AfirmaPlugin> list = this.view.getPluginsList();
		final DefaultListModel<AfirmaPlugin> listModel = (DefaultListModel<AfirmaPlugin>) list.getModel();
		listModel.removeElement(plugin);

		// Limpiamos el panel de informacion
		showPluginDetails(null);
	}


	/**
	 * Si es posible, abre una nueva instancia de la aplicaci&oacute;n y cierra la actual.
	 * Si no se encuentra un modo de arrancar la nueva instancia de la aplicaci&oacute;n,
	 * no se hace nada.
	 */
	private static void resetApplication() {

		File currentFile;
		try {
			currentFile = new File(PluginsManagementHandler.class.getProtectionDomain().getCodeSource().getLocation().toURI());
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se ha podido identificar el fichero ejecutable", e); //$NON-NLS-1$
			return;
		}

		// Compone el comando necesario para arrancar la aplicacion
		final List<String> command = getCommand(currentFile);

		// Ejecutamos una nueva instancia de la aplicacion
		if (command != null) {
			// Consultamos si se desea reiniciar la aplicacion
			final int option = JOptionPane.showConfirmDialog(
					null,
					SimpleAfirmaMessages.getString("PluginsManagementHandler.19"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("PluginsManagementHandler.18"), //$NON-NLS-1$
					JOptionPane.YES_NO_OPTION,
					JOptionPane.WARNING_MESSAGE);
			if (option == JOptionPane.YES_OPTION) {
				try {
					new ProcessBuilder(command).start();
				}
				catch (final Exception e) {
					LOGGER.log(Level.WARNING, "No se ha podido arrancar la nueva instancia de la aplicacion", e); //$NON-NLS-1$
				}

				// Salimos de la aplicacion antes de que se llegue a cargar la nueva instancia
				System.exit(0);
			}
		}
		// Pedimos al usuario que reinicie la aplicacion
		else {
			JOptionPane.showMessageDialog(
					null,
					SimpleAfirmaMessages.getString("PluginsManagementHandler.17"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("PluginsManagementHandler.18"), //$NON-NLS-1$
					JOptionPane.WARNING_MESSAGE);
		}
	}

	/**
	 * Devuelve el comando necesario para ejecutar la aplicaci&oacute;n o {@code null}
	 * si no hay una forma efectiva de ejecutarla
	 * @param currentFile Fichero o directorio con la aplicaci&oacute;n.
	 * @return Par&aacute;meros para la ejecuci&oacute;n de la aplicaci&oacute;n.
	 */
	private static List<String> getCommand(final File currentFile) {

		// La aplicacion se ejecutan las clases Java. No va a poder ejecutarse sin las
		// dependencias, por lo que se omite
		if (currentFile.isDirectory()) {
			return null;
		}

		// La aplicacion se ejecuta desde un JAR
		List<String> command;
		if (currentFile.getName().toLowerCase().endsWith(".jar")) { //$NON-NLS-1$

			// Si ese JAR forma parte de un ejecutable macOS, usamos el ejecutable
			final File appMac = MacUtils.getMacApp(currentFile);
			if (appMac != null && appMac.isFile()) {
				command = new ArrayList<>();
				command.add(appMac.getAbsolutePath());
			}
			else {
				final String java = System.getProperty("java.home") + File.separator + "bin" + File.separator + "java"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				command = new ArrayList<>();
				command.add(java);
				command.add("-jar"); //$NON-NLS-1$
				command.add(currentFile.getPath());
			}
		}
		// La aplicacion es un ejecutable de Windows
		else if (currentFile.getName().toLowerCase().endsWith(".exe")) { //$NON-NLS-1$
			command = new ArrayList<>();
			command.add(currentFile.getPath());
		}
		// En cualquier otro caso, no reiniciamos
		else {
			command = null;
		}

		return command;
	}

	void configPlugin() {

		final AfirmaPlugin plugin = this.view.getPluginsList().getSelectedValue();

		final PluginsPreferences preferences = PluginsPreferences.getInstance(plugin);

		PluginConfigurationDialog dialog;
		try {
			dialog = new PluginConfigurationDialog(
					SwingUtilities.getWindowAncestor(this.view),
					plugin);
		}
		catch (final PluginException e) {
			LOGGER.log(Level.SEVERE, "Error al cargar el dialogo de configuracion del plugin " + plugin.getInfo().getName(), e); //$NON-NLS-1$
			showError(SimpleAfirmaMessages.getString("PluginsManagementHandler.13"), e); //$NON-NLS-1$
			return;
		}
		dialog.init(preferences.recoverConfig());
		dialog.addWindowListener(new WindowAdapter() {
		    @Override
		    public void windowClosed(final WindowEvent e) {
				if (dialog.isAccepted()) {
					preferences.saveConfig(dialog.recoverConfig());
				}
		    }
		});
		dialog.setVisible(true);
	}

	private static void showError(final String message, final Throwable t) {
		AOUIFactory.showErrorMessage(message,
				SimpleAfirmaMessages.getString("PluginsManagementHandler.7"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE,
				t);
	}

	/**
	 * Muestra al usuario la informaci&oacute;n de un plugin.
	 * @param info Informaci&oacute;n del plugin.
	 */
	void showPluginDetails(final PluginInfo info) {

		final StringBuilder html = new StringBuilder();
		if (info != null) {
			html.append("<html>") //$NON-NLS-1$
				.append("<b>").append(SimpleAfirmaMessages.getString("PluginsManagementHandler.9")).append("</b><br>") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				.append("<span>&nbsp;&nbsp;").append(info.getVersion()).append("</span><br><br>"); //$NON-NLS-1$ //$NON-NLS-2$
			if (info.getAuthors() != null && info.getAuthors().length > 0) {
				html.append("<b>").append(SimpleAfirmaMessages.getString("PluginsManagementHandler.10")).append("</b><br>"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				for (final String author : info.getAuthors()) {
					html.append("<span>&nbsp;&nbsp;- ").append(author).append("</span><br>"); //$NON-NLS-1$ //$NON-NLS-2$
				}
				html.append("<br>"); //$NON-NLS-1$
			}
			if (info.getContacts() != null && info.getContacts().length > 0) {
				html.append("<b>").append(SimpleAfirmaMessages.getString("PluginsManagementHandler.11")).append("</b><br>"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				for (final String contact : info.getContacts()) {
					html.append("<span>&nbsp;&nbsp;").append(contact).append("</span><br>"); //$NON-NLS-1$ //$NON-NLS-2$
				}
				html.append("<br>"); //$NON-NLS-1$
			}
			html.append("<b>").append(SimpleAfirmaMessages.getString("PluginsManagementHandler.12")).append("</b><br>") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				.append("<span>").append(info.getDescription()).append("</span>") //$NON-NLS-1$ //$NON-NLS-2$
				.append("</html>"); //$NON-NLS-1$
		}

		this.view.getPluginInfoPane().setText(html.toString());
		this.view.getConfigButton().setVisible(info != null ? info.getConfigPanel() != null : false);
	}

	/**
	 * Carga la informaci&oacute;n actualmente configurada en la vista.
	 */
	void loadViewData() {
		try {
			this.pluginsList = this.pluginsManager.getPluginsLoadedList();
		} catch (final PluginException e) {
			LOGGER.severe("No se ha podido cargar la lista de plugins"); //$NON-NLS-1$
			showError(SimpleAfirmaMessages.getString("PluginsManagementHandler.8"), e); //$NON-NLS-1$
			return;
		}

		final JList<AfirmaPlugin> list = this.view.getPluginsList();
		final DefaultListModel<AfirmaPlugin> listModel = (DefaultListModel<AfirmaPlugin>) list.getModel();
		for (final AfirmaPlugin plugin : this.pluginsList) {
			listModel.addElement(plugin);
		}

		// Seleccionamos el primer elemento
		if (listModel.size() > 0) {
			list.setSelectedIndex(0);
			showPluginDetails(this.pluginsList.get(0).getInfo());
		}
	}

	@Override public void keyPressed(final KeyEvent e) { /* Vacio */ }
	@Override public void keyTyped(final KeyEvent e) { /* Vacio */ }

	@Override
	public void keyReleased(final KeyEvent ke) {
		// En Mac no cerramos los dialogos con Escape
		if (ke != null && ke.getKeyCode() == KeyEvent.VK_ESCAPE && !Platform.OS.MACOSX.equals(Platform.getOS())) {
			this.view.getParentWindow().dispose();
		}
	}

	@Override
	public void valueChanged(final ListSelectionEvent e) {

		// Cuando se selecciona un plugin del listado, se muestra su informacion en el
		// panel lateral
		if (e.getSource() instanceof JList<?>) {
			final Object plugin = ((JList<?>) e.getSource()).getSelectedValue();
			if (plugin instanceof AfirmaPlugin) {
				showPluginDetails(((AfirmaPlugin) plugin).getInfo());
			}
		}
	}
}
