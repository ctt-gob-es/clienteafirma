package es.gob.afirma.standalone.signdetails;

import es.gob.afirma.core.signers.AdESPolicy;

public class SignaturePolicy {

	String name;
	AdESPolicy policy;

	public SignaturePolicy(final String name, final AdESPolicy policy) {
		this.name = name;
		this.policy = policy;
	}
	public String getName() {
		return this.name;
	}
	public void setName(final String name) {
		this.name = name;
	}
	public AdESPolicy getPolicy() {
		return this.policy;
	}
	public void setPolicy(final AdESPolicy policy) {
		this.policy = policy;
	}

}
