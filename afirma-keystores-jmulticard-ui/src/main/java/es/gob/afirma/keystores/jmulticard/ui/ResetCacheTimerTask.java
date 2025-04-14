package es.gob.afirma.keystores.jmulticard.ui;

import java.util.TimerTask;


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
