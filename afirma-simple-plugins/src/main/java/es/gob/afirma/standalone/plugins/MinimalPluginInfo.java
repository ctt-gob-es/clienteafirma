package es.gob.afirma.standalone.plugins;


/**
 * Informaci&oacute;n esencial de un plugin.
 */
public class MinimalPluginInfo {

	protected final String internalName;
	protected int versionCode;

	/**
	 * Crea la informaci&oacute;n esencial de un plugin.
	 * @param internalName Nombre interno del plugin.
	 * @param versionCode Versi&oacute;n de c&oacute;digo.
	 */
	public MinimalPluginInfo(String internalName, int versionCode) {
		if (internalName == null) {
			throw new NullPointerException("El nombre interno del plugin no puede ser nulo"); //$NON-NLS-1$
		}
		this.internalName = internalName;
		this.versionCode = versionCode;
	}

	/**
	 * Recupera el nombre interno del plugin.
	 * @return Nombre del plugin.
	 */
	public String getInternalName() {
		return this.internalName;
	}

	/**
	 * Recupera la version de c&oacute;digo del plugin.
	 * @return Versi&oacute;n.
	 */
	public int getVersionCode() {
		return this.versionCode;
	}
}
