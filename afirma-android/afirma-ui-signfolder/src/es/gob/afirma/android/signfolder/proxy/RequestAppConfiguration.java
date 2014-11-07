package es.gob.afirma.android.signfolder.proxy;

import java.util.ArrayList;
import java.util.List;

/** Configuracion de la aplicaci&oacute;n.
 * @author Carlos Gamuci */
public final class RequestAppConfiguration {

	private List<String> appIdsList;
	private List<String> appNamesList;

	RequestAppConfiguration() {
		this.appIdsList = new ArrayList<String>();
		this.appNamesList = new ArrayList<String>();
	}

	public List<String> getAppIdsList() {
		return this.appIdsList;
	}

	void setAppIdsList(final List<String> appIdsList) {
		this.appIdsList = appIdsList;
	}

	public List<String> getAppNamesList() {
		return this.appNamesList;
	}

	void setAppNamesList(final List<String> appNamesList) {
		this.appNamesList = appNamesList;
	}
}
