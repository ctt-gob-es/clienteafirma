package es.gob.afirma.android.signfolder.proxy;

import java.util.ArrayList;
import java.util.List;

/**
 * Configuracion de la aplicaci&oacute;n.
 * @author Carlos Gamuci
 */
public class AppConfiguration {

	private List<String> appIdsList;
	private List<String> appNamesList;
	
	public AppConfiguration() {
		this.appIdsList = new ArrayList<String>();
		this.appNamesList = new ArrayList<String>();
	}

	public List<String> getAppIdsList() {
		return this.appIdsList;
	}

	public void setAppIdsList(List<String> appIdsList) {
		this.appIdsList = appIdsList;
	}

	public List<String> getAppNamesList() {
		return this.appNamesList;
	}

	public void setAppNamesList(List<String> appNamesList) {
		this.appNamesList = appNamesList;
	}
}
