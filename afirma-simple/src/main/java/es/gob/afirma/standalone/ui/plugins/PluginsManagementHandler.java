package es.gob.afirma.standalone.ui.plugins;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.DefaultListModel;
import javax.swing.JList;
import javax.swing.JOptionPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.plugins.PluginException;
import es.gob.afirma.standalone.plugins.PluginInfo;
import es.gob.afirma.standalone.plugins.PluginInstalledException;
import es.gob.afirma.standalone.plugins.PluginsManager;

/**
 * Manegador de eventos de PluginsManagementPanel.
 */
public class PluginsManagementHandler implements KeyListener {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final PluginsManagementPanel view;

	private List<PluginInfo> pluginsList;

	/**
	 * @param view
	 */
	public PluginsManagementHandler(final PluginsManagementPanel view) {
		this.view = view;
	}

	/**
	 * Establece el comportamiento sobre los componentes del panel.
	 */
	void registerComponents() {

		// Listado de plugins
		this.view.getPluginsList().addKeyListener(this);

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

		// Comprobamos que el plugin sea valido
		final PluginInfo pluginInfo = PluginsManager.checkPlugin(pluginFile);
		if (pluginInfo == null) {
			LOGGER.warning("El plugin no es valido y no se cargara"); //$NON-NLS-1$
			showError("El plugin no es valido y no se cargara");
			return;
		}

		// Copiamos el plugin al subdirectorio correspondiente dentro del
		// directorio de instalacion
		File importedPlugin;
		try {
			importedPlugin = PluginsManager.installPlugin(pluginFile, pluginInfo);
		}
		catch (final PluginInstalledException e) {
			LOGGER.log(Level.WARNING, "Ya existe una version instalada del plugin", e); //$NON-NLS-1$
			showError("Ya existe una versi\u00F3n instalada del plugin. Desinst\u00E1lela antes de continuar");
			return;
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "Ocurrio un error al instalar el plugin", e); //$NON-NLS-1$
			showError("Ocurri\u00F3 un error al instalar el plugin");
			return;
		}

		// Mostramos la informacion del plugin
		loadPluginInfo(pluginInfo);
	}

	/**
	 * Carga un fichero de plugin.
	 * @return Fichero de plugin.
	 */
	private File selectPluginFile() {
		final File[] files = AOUIFactory.getLoadFiles(
				"Cargar plugin",
				null,
				null,
				new String[] { PluginsManager.PLUGIN_EXTENSION },
				"Plugin de AutoFirma",
				false,
				false,
				AutoFirmaUtil.getDefaultDialogsIcon(),
				this.view);

		return files[0];
	}

	private void activePlugin(File file, PluginInfo info) {
		// TODO Auto-generated method stub
	}

	private void loadPluginInfo(PluginInfo pluginInfo) {

		// Insertamos el nombre del plugin en la lista
		final JList<PluginInfo> list = this.view.getPluginsList();
		final DefaultListModel<PluginInfo> listModel = (DefaultListModel<PluginInfo>) list.getModel();
		listModel.addElement(pluginInfo);

		// Seleccionamos el nuevo plugin
		list.setSelectedIndex(listModel.getSize() - 1);

		// Mostramos la informacion del plugin en el panel lateral
		showPluginInfo(pluginInfo);
	}

	void removePlugin() {

		// Obtenemos la informacion del plugin seleccionado
		final JList<PluginInfo> list = this.view.getPluginsList();
		final PluginInfo pluginInfo = list.getSelectedValue();
		if (pluginInfo == null) {
			return;
		}

		// Desinstalamos el plugin
		try {
			PluginsManager.uninstallPlugin(pluginInfo);
		} catch (final IOException e) {
			LOGGER.log(Level.SEVERE, "Ocurrio un error al desinstalar el plugin", e); //$NON-NLS-1$
			showError("Ocurri\u00F3 un error desinstalar el plugin"); //$NON-NLS-1$
			return;
		}

		// Eliminamos el plugin del listado
		final DefaultListModel<PluginInfo> listModel = (DefaultListModel<PluginInfo>) list.getModel();
		listModel.removeElement(pluginInfo);

		// Limpiamos el panel de informacion
		showPluginInfo(null);
	}

	void configPlugin() {
		// TODO Auto-generated method stub
	}

	private void showError(String message) {
		JOptionPane.showMessageDialog(this.view, message, "Error", JOptionPane.ERROR_MESSAGE);
	}

	/**
	 * Muestra al usuario la informaci&oacute;n de un plugin.
	 * @param info
	 */
	void showPluginInfo(PluginInfo info) {

		final StringBuilder html = new StringBuilder();
		if (info != null) {
			html.append("<html>") //$NON-NLS-1$
				.append("<b>").append(SimpleAfirmaMessages.getString("PluginsManagementPanel.0")).append("</b><br>") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				.append("<span>&nbsp;&nbsp;").append(info.getVersion()).append("</span><br><br>"); //$NON-NLS-1$ //$NON-NLS-2$
			if (info.getAuthors() != null && info.getAuthors().length > 0) {
				html.append("<b>").append(SimpleAfirmaMessages.getString("PluginsManagementPanel.1")).append("</b><br>"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				for (final String author : info.getAuthors()) {
					html.append("<span>&nbsp;&nbsp;- ").append(author).append("</span><br>"); //$NON-NLS-1$ //$NON-NLS-2$
				}
				html.append("<br>"); //$NON-NLS-1$
			}
			if (info.getContacts() != null && info.getContacts().length > 0) {
				html.append("<b>").append(SimpleAfirmaMessages.getString("PluginsManagementPanel.2")).append("</b><br>"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				for (final String contact : info.getContacts()) {
					html.append("<span>&nbsp;&nbsp;").append(contact).append("</span><br>"); //$NON-NLS-1$ //$NON-NLS-2$
				}
				html.append("<br>"); //$NON-NLS-1$
			}
			html.append("<b>").append(SimpleAfirmaMessages.getString("PluginsManagementPanel.3")).append("</b><br>") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				.append("<span>").append(info.getDescription()).append("</span>") //$NON-NLS-1$ //$NON-NLS-2$
				.append("</html>"); //$NON-NLS-1$
		}

		this.view.getPluginInfoPane().setText(html.toString());
		this.view.getConfigButton().setVisible(info != null ? info.isConfigurable() : false);
	}

	/**
	 * Carga la informaci&oacute;n actualmente configurada en la vista.
	 */
	void loadData() {

		List<PluginInfo> pluginsList;
		try {
			pluginsList = PluginsManager.getInstance().getPluginsLoadedList();
		} catch (final PluginException e) {
			LOGGER.severe("No se ha podido cargar la lista de plugins"); //$NON-NLS-1$
			showError("No se ha podido cargar la lista de plugins");
			return;
		}

		final JList<PluginInfo> list = this.view.getPluginsList();
		final DefaultListModel<PluginInfo> listModel = (DefaultListModel<PluginInfo>) list.getModel();
		for (final PluginInfo pluginInfo : pluginsList) {
			listModel.addElement(pluginInfo);
		}

		// Seleccionamos el primer elemento
		if (listModel.size() > 0) {
			list.setSelectedIndex(0);
			showPluginInfo(pluginsList.get(0));
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
}
