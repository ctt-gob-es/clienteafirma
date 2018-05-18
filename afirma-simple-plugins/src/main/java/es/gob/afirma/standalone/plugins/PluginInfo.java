package es.gob.afirma.standalone.plugins;


/**
 * Informaci&oacute;n de un plugin.
 */
public class PluginInfo {

	private static final String DEFAULT_VERSION = "1.0"; //$NON-NLS-1$

	private final String name;
	private final String description;
	private String version;
	private String[] authors;
	private String[] contacts;

	/**
	 * Crea la informaci&oacute;n b&aacute;sica de un plugin.
	 * @param name
	 * @param description
	 */
	public PluginInfo(String name, String description) {
		this.name = name;
		this.description = description;
		this.version = DEFAULT_VERSION;
		this.authors = null;
		this.contacts = null;
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
		this.version = version;
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
}
