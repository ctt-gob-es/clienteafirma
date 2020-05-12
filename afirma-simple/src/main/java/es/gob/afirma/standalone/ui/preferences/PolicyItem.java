package es.gob.afirma.standalone.ui.preferences;

import es.gob.afirma.core.signers.AdESPolicy;

/** Pol&iacute;tica de firma definida por los valores que deben establecerse para la misma y un nombre
 * que la define de cara al usuario. */
final class PolicyItem {

	private final String name;

	private AdESPolicy policy;

	/** Construye el elemento con nombre y configuraci&oacute;n de pol&iacute;tica.
	 * @param name Nombre de la configuraci&oacute;n.
	 * @param policy Configuraci&oacute;n. */
	PolicyItem(final String name, final AdESPolicy policy) {
		this.name = name;
		this.policy = policy;
	}

	/** Recupera la configuraci&oacute;n de pol&iacute;tica.
	 * @return Configuraci&oacute;n. */
	public AdESPolicy getPolicy() {
		return this.policy;
	}

	/** Establece la configuraci&oacute;n de pol&iacute;tica.
	 * @param policy Configuraci&oacute;n. */
	void setPolicy(final AdESPolicy policy) {
		this.policy = policy;
	}

	@Override
	public boolean equals(final Object obj) {

		if (obj instanceof AdESPolicy) {
			return this.policy != null && this.policy.equals(obj);
		}
		else if (obj instanceof PolicyItem) {
			final PolicyItem item = (PolicyItem) obj;
			return this.name.equals(item.name) &&
					(this.policy == null && item.getPolicy() == null ||
							this.policy.equals(item.getPolicy()));
		}
		return false;
	}

	@Override
	public String toString() {
		return this.name;
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}

	public String getName() {
		return this.name;
	}
}
