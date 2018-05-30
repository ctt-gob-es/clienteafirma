package es.gob.afirma.standalone.plugins;


/**
 * Informaci&oacute;n de un plugin.
 */
public class PluginInfo extends MinimalPluginInfo {

	private static final int DEFAULT_VERSION_CODE = 1;

	private static final String DEFAULT_VERSION = "1.0"; //$NON-NLS-1$

	private final String name;
	private String description;
	private String version;
	private String[] authors;
	private String[] contacts;

	private String configPanel;

	private GenericMenuOption menu;

	private PluginButton[] buttons;

	/**
	 * Crea la informaci&oacute;n b&aacute;sica de un plugin. El nombre interno del plugin,
	 * es el nombre con el cual se instalara, pero no se mostrar&aacute; al usuario. El nombre
	 * com&uacute; s&iacute; se podr&aacute; mostrar. El nombre interno debe
	 * permanecer invariable
	 * @param internalName Nombre interno del plugin.
	 * @param name Nombre legible del plugin.
	 */
	public PluginInfo(String internalName, String name) {
		super(internalName, DEFAULT_VERSION_CODE);

		if (name == null) {
			throw new NullPointerException("El nombre del plugin no puede ser nulo"); //$NON-NLS-1$
		}

		this.name = name;
		this.description = ""; //$NON-NLS-1$
		this.version = DEFAULT_VERSION;
		this.authors = null;
		this.contacts = null;
		this.configPanel = null;
	}

	/**
	 * Recupera el nombre legible del plugin.
	 * @return Nombre del plugin.
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * Recupera la descripci&oacute;n del plugin.
	 * @return Descripci&oacute;n del plugin.
	 */
	public String getDescription() {
		return this.description;
	}

	/**
	 * Estable la descripci&oacute;n del plugin.
	 * @param description Descripci&oacute;n del plugin.
	 */
	public void setDescription(String description) {
		this.description = description;
	}

	/**
	 * Establece la version de c&oacute;digo del plugin.
	 * @param versionCode Versi&oacute;n.
	 */
	public void setVersionCode(int versionCode) {
		this.versionCode = versionCode;
	}

	/**
	 * Recupera la cadena con la identificaci&oacute;n de la versi&oacute;n del plugin.
	 * @return Cadena con la versi&oacute;n.
	 */
	public String getVersion() {
		return this.version;
	}

	/**
	 * Estable el n&uacute;mero de versi&oacute;n del plugin.
	 * @param version Versi&oacute;n del plugin.
	 */
	public void setVersion(String version) {
		if (version != null) {
			this.version = version;
		}
	}

	/**
	 * Recupera el listado de autores del plugin.
	 * @return Listado de autores.
	 */
	public String[] getAuthors() {
		return this.authors;
	}

	/**
	 * Establece el listado de autores del plugin.
	 * @param authors Listado de autores.
	 */
	public void setAuthors(String[] authors) {
		this.authors = authors;
	}

	/**
	 * Recupera el listado de informaci&oacute;n de contacto del soporte del plugin.
	 * @return Listado contactos.
	 */
	public String[] getContacts() {
		return this.contacts;
	}

	/**
	 * Establece el listado de informaci&oacute;n de contacto del soporte del plugin.
	 * @param contacts Listado contactos.
	 */
	public void setContacts(String[] contacts) {
		this.contacts = contacts;
	}

	/**
	 * Comprueba si el plugin admite configuraci&oacute;n por parte del usuario.
	 * @return {@code true} si el plugin admite configuraci&oacute;n, {@code false}
	 * en caso contrario.
	 */
	public String getConfigPanel() {
		return this.configPanel;
	}

	/**
	 * Establece el nombre de la clase con el panel de configuraci&oacute;n del plugin. Esta clase
	 * deber&aacute; extender {@link es.gob.afirma.standalone.plugins.ConfiguratorPanel}.
	 * @param configPanel Nombre de la clase del panel de configuraci&oacute;n.
	 */
	public void setConfigPanel(String configPanel) {
		this.configPanel = configPanel;
	}

	/**
	 * Recupera el men&uacte; general del plugin.
	 * @return Men&uacute; general.
	 */
	public GenericMenuOption getMenu() {
		return this.menu;
	}

	/**
	 * Establece el men&uacte; general del plugin.
	 * @param menu Men&uacute; general.
	 */
	public void setMenu(GenericMenuOption menu) {
		this.menu = menu;
	}

	/**
	 * Recupera los botones para el uso de las acciones del plugin.
	 * @return Botones para el acceso a las funciones del plugin.
	 */
	public PluginButton[] getButtons() {
		return this.buttons;
	}

	/**
	 * Establece los botones para el uso de las acciones del plugin.
	 * @param buttons Botones para el acceso a las funciones del plugin.
	 */
	public void setButtons(PluginButton[] buttons) {
		this.buttons = buttons;
	}

	@Override
	public String toString() {
		return this.name;
	}
}
