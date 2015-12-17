package es.gob.afirma.signfolder.server.proxy;

import java.util.List;

/**
 * Configuracion de la aplicaci&oacute;n.
 * @author Carlos Gamuci
 */
public class AppConfiguration {

	private final List<String> appIdsList;
	private final List<String> appNamesList;

	/**
	 * Construye un objeto con el listado de aplicaciones datos de alta en el Portafirmas.
	 * @param appIdsList Listado de identificadores de aplicaci&oacute;n.
	 * @param appNamesList Listado de nombres de aplicaci&oacute;n.
	 */
	public AppConfiguration(final List<String> appIdsList, final List<String> appNamesList) {
		this.appIdsList = appIdsList;
		this.appNamesList = appNamesList;
	}

	/**
	 * Recupera el listado de identificadores de aplicaci&oacute;n.
	 * @return Listado de identificadores de aplicaci&oacute;n.
	 */
	public List<String> getAppIdsList() {
		return this.appIdsList;
	}

	/**
	 * Recupera el listado de nombres de aplicaci&oacute;n.
	 * @return Listado de nombres de aplicaci&oacute;n.
	 */
	public List<String> getAppNamesList() {
		return this.appNamesList;
	}
}
