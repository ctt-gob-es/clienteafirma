package es.gob.afirma.core.misc.http;

public class ConnectionConfig {

	private int readTimeout = -1;

	private UrlHttpManager defaultConnection = null;

	public int getReadTimeout() {
		return this.readTimeout;
	}

	public void setReadTimeout(final int readTimeout) {
		this.readTimeout = readTimeout;
	}

	public UrlHttpManager getDefaultConnection() {
		return this.defaultConnection;
	}

	public void setDefaultConnection(final UrlHttpManager defaultConnection) {
		this.defaultConnection = defaultConnection;
	}

	public void apply(final UrlHttpManager urlManager) {

		if (urlManager == null) {
			return;
		}

		if (this.readTimeout != UrlHttpManager.DEFAULT_TIMEOUT) {
			urlManager.setReadTimeout(this.readTimeout);
		}
	}
}
