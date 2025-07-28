package es.gob.afirma.keystores.jmulticard.ui;

import java.util.Arrays;

/** Resultado del di&aacute;logo de solicitud de contrase&ntilde;a. */
final class PasswordResult {

	private transient char[] password;

	private transient boolean useCache;

	PasswordResult(final char[] passwd) {
		this.password = passwd != null ? passwd.clone() : null;
		this.useCache = false;
	}

	PasswordResult(final char[] passwd, final boolean cachePassword) {
		this.password = passwd != null ? passwd.clone() : null;
		this.useCache = cachePassword;
	}

	char[] getPassword() {
		return this.password != null ? this.password.clone() : null;
	}

	boolean isUseCache() {
		return this.useCache;
	}

	void clear() {
		if (this.password != null) {
			Arrays.fill(this.password, '\0');
			this.password = null;
		}
		this.useCache = false;
	}
}
