package es.gob.afirma.android.gui;

import android.graphics.drawable.Drawable;

/** Aplicaci&oacute;n Android. Usada para la descripci&oacute;n de las aplicaciones de solicitud de certificados
 * del sistema.
 * @author Astrid Idoate */
public final class AppProperties {

	private final CharSequence nameApp;
	private final String packageName;
	private final Drawable icon;
	private final String mainActivity;
	private final String description;
	private final String marketUrl;
	private final boolean installed;

	/** Crea una clase que representa una aplicaci&oacute;n Android.
	 * @param nameApp Nombre de la aplicaci&oacute;n
	 * @param packageName Paquete
	 * @param mainActivity Actividad principal
	 * @param description Descripci&oacute;n
	 * @param icon Icono
	 * @param marketUrl URL de descarga de la aplicaci&oacute;n en Google Play
	 * @param installed <code>true</code> si la aplicaci&oacute;n est&aacute; actualmente instalada, <code>false</code>
	 *                  en caso contrario */
	public AppProperties(final CharSequence nameApp,
			             final String packageName,
			             final String mainActivity,
			             final String description,
			             final Drawable icon,
			             final String marketUrl,
			             final boolean installed) {
		this.nameApp = nameApp;
		this.packageName = packageName;
		this.mainActivity = mainActivity;
		this.description = description;
		this.icon = icon;
		this.marketUrl = marketUrl;
		this.installed = installed;
	}

	/** Obtiene la actividad principal de la aplicaci&oacute;n.
	 * @return mainActivity Actividad principal de la aplicaci&oacute;n. */
	public String getMainActivity(){
		return this.mainActivity;
	}

	/** Obtiene la descripci&oacute;n de la aplicaci&oacute;n.
	 * @return Descripci&oacute;n de la aplicaci&oacute;n. */
	public String getDescription(){
		return this.description;
	}

	/** Obtiene el nombre de la aplicaci&oacute;n.
	 * @return nameApp Nombre de la aplicaci&oacute;n. */
	public CharSequence getNameApp(){
		return this.nameApp;
	}

	/** Obtiene el nombre del paquete de la aplicaci&ocute;n.
	 * @return packageName Nombre del paquete de la aplicaci&ocute;n. */
	public String getPackageName(){
		return this.packageName;
	}

	/** Obtiene el icono de la aplicaci&ocute;n.
	 * @return icon Identificador de <i>drawable</i> del icono. */
	public Drawable getIcon(){
		return this.icon;
	}

	/** Obtiene la URL de GooglePlay para la instalaci&oacute;n de la aplicaci&oacute;n.
	 * @return URL de GooglePlay para la instalaci&oacute;n de la aplicaci&oacute;n. */
	public String getMarketUrl() {
		return this.marketUrl;
	}

	/** Indica si la aplicaci&oacute;n est&aacute; instalada en el dispositivo.
	 * @return {@code true} si la aplicaci&oacute;n est&aacute; instalada, {@code false} en caso contrario. */
	public boolean isInstalled() {
		return this.installed;
	}


}
