package es.gob.afirma.keystores.jmulticard.ui;

import java.util.Timer;
import java.util.logging.Logger;
import java.util.prefs.Preferences;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.UnsupportedCallbackException;

/** <code>CallbackHandler</code> que gestiona los <code>Callbacks</code> de petici&oacute;n de
 * informaci&oacute;n al usuario cuando utiliza una tarjeta inteligente.
 * Esta clase cachea las respuestas de confirmaci&oacute;n y contrase&ntilde;a del usuario de
 * tal forma que no requerir&aacute;a que las vuelva a introducir.
 * La cach&eacute; se borra autom&aacute;ticamente pasado un tiempo determinado. */
public final class SmartcardCacheCallbackHandler implements CallbackHandler, CacheElement {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final long CACHE_TIMEOUT = 3600 * 1000;	// 1 hora

	private static final String PREFERENCE_KEY_USE_CACHE = "useCacheCeres"; //$NON-NLS-1$

	private transient char[] cachedPassword = null;

	private transient Timer timer = null;

	@Override
	public void handle(final Callback[] callbacks) throws UnsupportedCallbackException {

		if (callbacks != null) {
			for (final Callback cb : callbacks) {
				if (cb != null) {
					if (!(cb instanceof PasswordCallback)) {
						throw new UnsupportedCallbackException(cb);
					}
					synchronized (LOGGER) {
						char[] pin;
						if (this.cachedPassword == null) {

							// Comprobamos si anteriormente se activo la opcion de usar cache para
							// poner este valor por defecto
							final boolean useCacheDefaultValue = loadUseCachePreference();

							final CommonPasswordCallback uip = new CommonPasswordCallback(
								((PasswordCallback)cb).getPrompt(),
								Messages.getString("CommonPasswordCallback.2"), //$NON-NLS-1$
								false,
								true,
								useCacheDefaultValue
							);

							pin = uip.getPassword();

							// Si se encuentra marcada la opcion de usar cache, guardamos el PIN
							final boolean newUseCacheDefaultValue = uip.isUseCacheChecked();
							if (newUseCacheDefaultValue) {
								LOGGER.info("Guardamos en cache la contrasena de la tarjeta"); //$NON-NLS-1$
								this.cachedPassword = pin;
							}
							// Si se ha cambiado el valor de la opcion de usar cache, guardamos este valor
							if (useCacheDefaultValue != newUseCacheDefaultValue) {
								setUseCachePreference(newUseCacheDefaultValue);
							}
						}
						else {
							pin = this.cachedPassword;
						}
						((PasswordCallback)cb).setPassword(pin);
					}

					 // Si no se ha hecho ya, programamos una tarea para el borrado de la contrasena cacheada para
					// que se ejecute en un tiempo determinado
					if (this.timer == null) {
						this.timer = new Timer();
						this.timer.schedule(new ResetCacheTimerTask(this), CACHE_TIMEOUT);
					}
				}
			}
		}
		else {
			LOGGER.warning("Se ha recibido un array de Callbacks nulo"); //$NON-NLS-1$
		}
	}

	@Override
	public void reset() {

		LOGGER.info("Eliminamos de cache la contrasena de la tarjeta"); //$NON-NLS-1$

		synchronized (LOGGER) {
			this.cachedPassword = null;
		}

		if (this.timer != null) {
			this.timer.cancel();
			this.timer.purge();
			this.timer = null;
		}
	}

	private static boolean loadUseCachePreference() {
		try {
			return Preferences.userNodeForPackage(SmartcardCacheCallbackHandler.class).getBoolean(PREFERENCE_KEY_USE_CACHE, false);
		}
		catch (final Exception e) {
			LOGGER.warning("No se puede acceder a la configuracion del cacheo del PIN de la tarjeta: " + e); //$NON-NLS-1$
			return false;
		}
	}

	private static void setUseCachePreference(final boolean useCache) {
		try {
			Preferences.userNodeForPackage(SmartcardCacheCallbackHandler.class).putBoolean(PREFERENCE_KEY_USE_CACHE, useCache);
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo guardar la configuracion del cacheo del PIN de la tarjeta: " + e); //$NON-NLS-1$
		}
	}
}
