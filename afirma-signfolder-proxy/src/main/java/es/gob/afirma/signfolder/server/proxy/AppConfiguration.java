package es.gob.afirma.signfolder.server.proxy;

import java.util.List;

/**
 * Configuracion de la aplicaci&oacute;n.
 * @author Carlos Gamuci
 */
public class AppConfiguration {

	private final List<String> appIdsList;
	private final List<String> appNamesList;
	
	public AppConfiguration(final List<String> appIdsList, final List<String> appNamesList) {
		this.appIdsList = appIdsList;
		this.appNamesList = appNamesList;
	}

	public List<String> getAppIdsList() {
		return this.appIdsList;
	}

	public List<String> getAppNamesList() {
		return this.appNamesList;
	}
}
