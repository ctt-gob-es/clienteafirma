package es.gob.afirma.standalone.ui.preferences;

import es.gob.afirma.keystores.AOKeyStore;

public class RegisteredKeystore {

	private String name;
	private String lib;
	private String providerName;

	RegisteredKeystore() {
		// Constructor vacio
	}

	RegisteredKeystore(final AOKeyStore aoks) {
		this.name = aoks.getName();
		this.providerName = aoks.getProviderName();
		this.lib = null;
	}

	public String getName() {
		return this.name;
	}
	public void setName(final String name) {
		this.name = name;
	}
	public String getLib() {
		return this.lib;
	}
	public void setLib(final String lib) {
		this.lib = lib;
	}
	public String getProviderName() {
		return this.providerName;
	}
	public void setProviderName(final String providerName) {
		this.providerName = providerName;
	}

	@Override
	public String toString() {
		return getName();
	}

}
