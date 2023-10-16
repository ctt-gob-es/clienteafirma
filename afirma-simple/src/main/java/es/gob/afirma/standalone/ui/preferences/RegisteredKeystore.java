package es.gob.afirma.standalone.ui.preferences;

import es.gob.afirma.keystores.AOKeyStore;

public class RegisteredKeystore {

	private String name;
	private String lib;
	private String providerName;
	private final boolean isSystemSmartCard;

	RegisteredKeystore() {
		this.isSystemSmartCard = false;
	}

	RegisteredKeystore(final AOKeyStore aoks) {
		this(aoks, false);
	}

	RegisteredKeystore(final AOKeyStore aoks, final boolean isSystemSmartCard) {
		this.name = aoks.getName();
		this.providerName = aoks.getProviderName();
		this.lib = null;
		this.isSystemSmartCard = isSystemSmartCard;
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
	public boolean isSystemSmartCard() {
		return this.isSystemSmartCard;
	}

	@Override
	public String toString() {
		return getName();
	}

}
