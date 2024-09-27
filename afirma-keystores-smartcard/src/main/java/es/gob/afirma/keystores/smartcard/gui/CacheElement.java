package es.gob.afirma.keystores.smartcard.gui;

import java.util.Arrays;
import java.util.Timer;
import java.util.TimerTask;

public class CacheElement {

	private static final long CACHE_TIMEOUT = 3600 * 1000;	// 1 hora

	private boolean cacheEnabled = true;

	private transient char[] password = null;

	private transient Timer timer = null;

	void setPassword(final char[] password) {
		this.password = password;
		// Si no se ha hecho ya, programamos una tarea para el borrado de la contrasena cacheada para
		// que se ejecute en un tiempo determinado
		if (this.timer == null) {
			this.timer = new Timer();
			this.timer.schedule(new ResetCacheTimerTask(this), CACHE_TIMEOUT);
		}
	}

	char[] getPassword() {
		return this.password;
	}

	void setCacheEnabled(final boolean enable) {
		this.cacheEnabled = enable;
	}

	boolean isCacheEnabled() {
		return this.cacheEnabled;
	}

	public void reset() {
		if (this.password != null) {
			Arrays.fill(this.password, '\0');
			this.password = null;
			if (this.timer != null) {
				this.timer.cancel();
				this.timer.purge();
				this.timer = null;
			}
		}
	}

	/** Tarea para el borrado de los datos <i>cacheados</i> por un elemento. */
	final class ResetCacheTimerTask extends TimerTask {

		private transient final CacheElement element;

		ResetCacheTimerTask(final CacheElement cacheElement) {
			this.element = cacheElement;
		}

		@Override
		public void run() {
			if (this.element != null) {
				this.element.reset();
			}
		}
	}
}
