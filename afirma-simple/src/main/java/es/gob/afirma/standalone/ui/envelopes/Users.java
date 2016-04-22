package es.gob.afirma.standalone.ui.envelopes;

public class Users {

	private final String cn;
	private final String email;
	private final String uid;

	public String getCn() {
		return this.cn;
	}
	public String getEmail() {
		return this.email;
	}
	public String getUid() {
		return this.uid;
	}

	public Users(final String cn, final String email, final String uid) {
		this.cn = cn;
		this.email = email;
		this.uid = uid;
	}

	@Override
	public String toString() {
		return "cn=" + this.cn + ", email=" + this.email + ", UID=" + this.uid; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}
}
