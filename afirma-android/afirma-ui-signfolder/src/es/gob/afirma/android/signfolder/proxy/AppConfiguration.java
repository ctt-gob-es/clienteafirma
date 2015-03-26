package es.gob.afirma.android.signfolder.proxy;

import java.util.ArrayList;
import java.util.List;

/** Configuracion de la aplicaci&oacute;n.
 * @author Carlos Gamuci. */
public class AppConfiguration {

	private List<String> appIdsList;
	private List<String> appNamesList;

	AppConfiguration() {
		this.appIdsList = new ArrayList<String>();
		this.appNamesList = new ArrayList<String>();
	}

	/** Obtiene la lista de identificadores de la aplicaci&oacute;n.
	 * @return Lista de identificadores de la aplicaci&oacute;n. */
	public List<String> getAppIdsList() {
		return this.appIdsList;
	}

	void setAppIdsList(final List<String> appIdsList) {
		this.appIdsList = appIdsList;
	}

	/** Obtiene la lista de nombres de la aplicaci&oacute;n.
	 * @return Lista de nombres de la aplicaci&oacute;n. */
	public List<String> getAppNamesList() {
		return this.appNamesList;
	}

	void setAppNamesList(final List<String> appNamesList) {
		this.appNamesList = appNamesList;
	}
}
