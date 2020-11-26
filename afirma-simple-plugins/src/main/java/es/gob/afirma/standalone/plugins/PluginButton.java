package es.gob.afirma.standalone.plugins;

/**
 * Bot&oacute;n que un plugin puede agregar a alguna de las ventanas de la
 * aplicaci&oacute;n.
 */
public class PluginButton {

	private String title = null;
	private String icon = null;
	private String toolTip = null;
	private String accesibleDescription = null;
	private String afirmaWindow = null;
	private String actionClassName = null;
	private PluginAction action = null;

	/**
	 * Recupera el texto del bot&oacute;n.
	 * @return Texto del bot&oacute;n.
	 */
	public String getTitle() {
		return this.title;
	}
	/**
	 * Establece el texto del bot&oacute;n.
	 * @param title Texto del bot&oacute;n.
	 */
	public void setTitle(final String title) {
		this.title = title;
	}
	/**
	 * Recupera el icono del bot&oacute;n.
	 * @return Icono del bot&oacute;n.
	 */
	public String getIcon() {
		return this.icon;
	}
	/**
	 * Establece el icono del bot&oacute;n.
	 * @param icon Icono del bot&oacute;n.
	 */
	public void setIcon(final String icon) {
		this.icon = icon;
	}
	/**
	 * Recupera el texto de tooltip del bot&oacute;n.
	 * @return Texto del tooltip del bot&oacute;n.
	 */
	public String getToolTip() {
		return this.toolTip;
	}
	/**
	 * Establece el texto de tooltip del bot&oacute;n.
	 * @param toolTip Texto del tooltip del bot&oacute;n.
	 */
	public void setToolTip(final String toolTip) {
		this.toolTip = toolTip;
	}
	/**
	 * Recupera el texto de descripci&oacute;n accesible del bot&oacute;n.
	 * @return Descripci&oacute;n accesible  del bot&oacute;n.
	 */
	public String getAccesibleDescription() {
		return this.accesibleDescription;
	}
	/**
	 * Establece el texto de descripci&oacute;n accesible del bot&oacute;n.
	 * @param accesibleDescription Descripci&oacute;n accesible  del bot&oacute;n.
	 */
	public void setAccesibleDescription(final String accesibleDescription) {
		this.accesibleDescription = accesibleDescription;
	}
	/**
	 * Recupera el descriptor de la ventana de la aplicaci&oacute;n en la
	 * que debe aparecer el bot&oacute;n.
	 * @return Descriptor de ventana de la aplicaci&oacute;n.
	 */
	public String getWindow() {
		return this.afirmaWindow;
	}
	/**
	 * Establece el descriptor de la ventana de la aplicaci&oacute;n en la
	 * que debe aparecer el bot&oacute;n.
	 * @param window Descriptor de ventana de la aplicaci&oacute;n.
	 */
	public void setAfirmaWindow(final String window) {
		this.afirmaWindow = window;
	}
	/**
	 * Recupera el nombre de la clase de la acci&oacute;n asignada al bot&oacute;n.
	 * @return Nombre de la clase de acci&oacute;n.
	 */
	public String getActionClassName() {
		return this.actionClassName;
	}
	/**
	 * Establece el nombre de clase de la acci&oacute;n asignada al bot&oacute;n.
	 * @param actionClassName Nombre de la clase de acci&oacute;n.
	 */
	public void setActionClassName(final String actionClassName) {
		this.actionClassName = actionClassName;
	}

	/**
	 * Recupera la acci&oacute;n asignada al bot&oacute;n.
	 * @return Acci&oacute;n asignada al bot&oacute;n.
	 */
	public PluginAction getAction() {
		return this.action;
	}
	/**
	 * Establece la acci&oacute;n asignada al bot&oacute;n.
	 * @param action Acci&oacute;n asignada al bot&oacute;n.
	 */
	public void setAction(final PluginAction action) {
		this.action = action;
	}
}
