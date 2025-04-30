package es.gob.afirma.keystores.jmulticard.ui;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Timer;
import java.util.logging.Logger;
import java.util.prefs.Preferences;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.UnsupportedCallbackException;


/** <code>CallbackHandler</code> que gestiona los <code>Callbacks</code> de petici&oacute;n de
 * informaci&oacute;n al usuario cuando utiliza un DNIe. Esta clase <i>cachea</i> las respuestas
 * de confirmaci&oacute;n y contrase&ntilde;a del usuario de tal forma que no requerir&aacute;a
 * que las vuelva a introducir. La cach&eacute; se borra autom&aacute;ticamente pasado un tiempo
 * determinado. */
public final class DnieCacheCallbackHandler implements CallbackHandler, CacheElement {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final long CACHE_TIMEOUT = 3600 * 1000;	// 1 hora

	private static final String PREFERENCE_KEY_USE_CACHE = "useCacheDni"; //$NON-NLS-1$

	private transient char[] cachedPassword = null;

	private transient boolean confirmed = false;

	private transient Timer timer = null;

	@Override
	public void handle(final Callback[] callbacks) throws UnsupportedCallbackException {

		if (callbacks != null) {
			for (final Callback cb : callbacks) {
				if (cb != null) {
					if (
						"es.gob.jmulticard.callback.CustomTextInputCallback".equals(cb.getClass().getName()) || //$NON-NLS-1$
						"javax.security.auth.callback.TextInputCallback".equals(cb.getClass().getName()) //$NON-NLS-1$
					) {
						LOGGER.info("Solicitamos codigo CAN del DNIe"); //$NON-NLS-1$
						try {
							final Method m = cb.getClass().getMethod("setText", String.class); //$NON-NLS-1$
							final UIPasswordCallbackCan uip = new UIPasswordCallbackCan(
								Messages.getString("CanPasswordCallback.0"), //$NON-NLS-1$
								null,
								Messages.getString("CanPasswordCallback.0"), //$NON-NLS-1$
								Messages.getString("CanPasswordCallback.2") //$NON-NLS-1$
							);
							m.invoke(cb, new String(uip.getPassword()));
						}
						catch (final NoSuchMethodException    |
							         SecurityException        |
							         IllegalAccessException   |
							         IllegalArgumentException |
							         InvocationTargetException e) {
							throw new UnsupportedCallbackException(
								cb,
								"No se ha podido invocar al metodo 'setText' de la callback: " + e //$NON-NLS-1$
							);
						}
					}
					else if (cb instanceof CustomAuthorizeCallback) {
						if (!this.confirmed) {
							LOGGER.info("Solicitamos autorizacion para el uso del DNIe"); //$NON-NLS-1$
								DialogBuilder.showSignatureConfirmDialog((CustomAuthorizeCallback) cb);
							this.confirmed = ((CustomAuthorizeCallback) cb).isAuthorized();
						}
						((CustomAuthorizeCallback) cb).setAuthorized(this.confirmed);
					}
					else if (cb instanceof PasswordCallback) {
						synchronized (LOGGER) {
							final char[] pin;
							if (this.cachedPassword == null) {
								LOGGER.info("Solicitamos codigo PIN del DNIe"); //$NON-NLS-1$

								// Comprobamos si anteriormente se activo la opcion de usar cache para
								// poner este valor por defecto
								final boolean useCacheDefaultValue = loadUseCachePreference();

								final CommonPasswordCallback uip = new CommonPasswordCallback(
									((PasswordCallback)cb).getPrompt(),
									Messages.getString("CommonPasswordCallback.1"), //$NON-NLS-1$
									true,
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

						// Si no se ha hecho ya, programamos una tarea para el borrado de la
						// contrasena cacheada para que se ejecute en un tiempo determinado
						if (this.timer == null) {
							this.timer = new Timer();
							this.timer.schedule(new ResetCacheTimerTask(this), CACHE_TIMEOUT);
						}
					}
					else {
						throw new UnsupportedCallbackException(cb);
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
			this.confirmed = false;
		}

		if (this.timer != null) {
			this.timer.cancel();
			this.timer.purge();
			this.timer = null;
		}
	}

	private static boolean loadUseCachePreference() {
		try {
			return Preferences.userNodeForPackage(
				DnieCacheCallbackHandler.class).getBoolean(PREFERENCE_KEY_USE_CACHE, false
			);
		}
		catch (final Exception e) {
			LOGGER.warning(
				"No se puede acceder a la configuracion del cacheo del PIN de la tarjeta: " + e //$NON-NLS-1$
			);
			return false;
		}
	}

	private static void setUseCachePreference(final boolean useCache) {
		try {
			Preferences.userNodeForPackage(
				DnieCacheCallbackHandler.class).putBoolean(PREFERENCE_KEY_USE_CACHE, useCache
			);
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo guardar la configuracion del cacheo del PIN de la tarjeta: " + e); //$NON-NLS-1$
		}
	}
}
