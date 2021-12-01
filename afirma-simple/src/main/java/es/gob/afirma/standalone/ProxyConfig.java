package es.gob.afirma.standalone;

/**
 * Configuraci&oacute;n de proxy establecida.
 */
public class ProxyConfig {

	private final ConfigType configType;

	private String host = null;

	private String port = null;

	private String username = null;

	private char[] password = null;

	private String excludedUrls = null;

	/**
	 * Crea la configuraci&oacute;n del proxy.
	 * @param type Configuraci&oacute;n base (sin proxy, heredada del sistema, etc).
	 */
	public ProxyConfig(final ConfigType type) {
		this.configType = type;
	}

	/**
	 * Recupera la configuraci&oacute;n base.
	 * @return Configuraci&oacute;n base (sin proxy, heredada del sistema, etc).
	 */
	public ConfigType getConfigType() {
		return this.configType;
	}

	/**
	 * Recupera la IP o nombre de dominio del proxy.
	 * @return IP o nombre de dominio del proxy.
	 */
	public String getHost() {
		return this.host;
	}

	/**
	 * Establece la IP o nombre de dominio del proxy.
	 * @param host IP o nombre de dominio del proxy.
	 */
	public void setHost(final String host) {
		this.host = host;
	}

	/**
	 * Recupera el puerto de acceso al proxy.
	 * @return Puerto de acceso al proxy.
	 */
	public String getPort() {
		return this.port;
	}

	/**
	 * Establece el puerto de acceso al proxy.
	 * @param port Puerto de acceso al proxy.
	 */
	public void setPort(final String port) {
		this.port = port;
	}

	/**
	 * Recupera el nombre de usuario de acceso al proxy.
	 * @return Nombre de usuario de acceso al proxy.
	 */
	public String getUsername() {
		return this.username;
	}

	/**
	 * Establece el nombre de usuario de acceso al proxy.
	 * @param username Nombre de usuario de acceso al proxy.
	 */
	public void setUsername(final String username) {
		this.username = username;
	}

	/**
	 * Recupera la contrase&ntilde;a de acceso al proxy.
	 * @return Contrase&ntilde;a de acceso al proxy.
	 */
	public char[] getPassword() {
		return this.password != null ? this.password.clone() : null;
	}

	/**
	 * Establece la contrase&ntilde;a de acceso al proxy.
	 * @param password Contrase&ntilde;a de acceso al proxy.
	 */
	public void setPassword(final char[] password) {
		this.password = password != null ? password.clone() : null;
	}

	/**
	 * Recupera el listado de URLs que deben excluirse del uso de proxy.
	 * @return Listado de URLs a las que no se acceder&aacute; a trav&eacute;s de proxy.
	 */
	public String getExcludedUrls() {
		return this.excludedUrls;
	}

	/**
	 * Establece el listado de URLs que deben excluirse del uso de proxy.
	 * @param urls Listado de URLs a las que no se acceder&aacute; a trav&eacute;s de proxy.
	 */
	public void setExcludedUrls(final String urls) {
		this.excludedUrls = urls;
	}

	/**
	 * Tipo de configuraci&oacute;n base (sin proxy, heredada del sistema, etc).
	 */
	public static enum ConfigType {
		/** No usar proxy. */
		NONE,
		/** Usar la configuraci&oacute;n del sistema. */
		SYSTEM,
		/** Usar configuraci&oacute;n establecida por el usuario. */
		CUSTOM
	}
}
