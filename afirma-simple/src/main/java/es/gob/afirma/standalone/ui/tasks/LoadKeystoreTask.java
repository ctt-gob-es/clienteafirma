package es.gob.afirma.standalone.ui.tasks;

import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.prefs.KeyStorePreferencesManager;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.standalone.SimpleKeyStoreManager;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;

public class LoadKeystoreTask extends Thread{

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private AOKeyStoreManager keyStoreManager;

	private AOKeyStore keystore;

	private Exception exception;

	/**
	 * Construye la tarea seleccionando ya cu&aacute;l es el almac&eacute;n por defecto que
	 * se cargar&aacute;.
	 */
	public LoadKeystoreTask() {

		// Comprobamos cual fue el ultimo almacen utilizado y si habia alguno establecido por defecto
		final String lastSelectedKeyStore = KeyStorePreferencesManager.getLastSelectedKeystore();
		final boolean useDefaultStore = PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_USE_DEFAULT_STORE_IN_BROWSER_CALLS);

		// Si hay marcado un almacen como el ultimo seleccionado, lo usamos (este es el caso en el que se llaman
		// varias operaciones de firma dentro de la misma invocacion a la aplicacion)
		if (lastSelectedKeyStore != null && !lastSelectedKeyStore.isEmpty()) {
			this.keystore = SimpleKeyStoreManager.getLastSelectedKeystore();
		}
		// Si no, si el usuario definio un almacen por defecto para usarlo en las llamadas a la aplicacion, lo usamos
		else if (useDefaultStore) {
			final String defaultStore = PreferencesManager.get(PreferencesManager.PREFERENCE_KEYSTORE_DEFAULT_STORE);
			if (!PreferencesManager.VALUE_KEYSTORE_DEFAULT.equals(defaultStore)
				&& !AOKeyStore.PKCS12.getName().equals(defaultStore)
				&& !AOKeyStore.PKCS11.getName().equals(defaultStore)) {
				this.keystore = SimpleKeyStoreManager.getKeyStore(defaultStore, true);
			}
		}

		// Si aun no se ha definido el almacen, se usara el por defecto para el sistema operativo
		if (this.keystore == null || !isOsKeyStore(this.keystore)) {
			this.keystore = AOKeyStore.getDefaultKeyStoreTypeByOs(Platform.getOS());
		}
	}

    @Override
    public void run() {

    	LOGGER.info("Iniciando hilo para la carga de almacen: " + this.keystore.getName()); //$NON-NLS-1$

    	try {
    		final PasswordCallback pwc = this.keystore.getStorePasswordCallback(null);

    		this.keyStoreManager = AOKeyStoreManagerFactory.getAOKeyStoreManager(this.keystore, // Store
					null, // Lib
					null, // Description
					pwc, // PasswordCallback
					null // Parent
					);
    	} catch (final Exception e) {
    		LOGGER.severe("Error al cargar almacen de claves en segundo plano: " + e); //$NON-NLS-1$
    		this.exception = e;
    	}

    }

    /**
     * Devuelve el gestor de almac&eacute;n configurado una vez se ha cargado. Este m&eacute;todo
     * devolver&aacute; {@code null} si el hilo no se ha ejecutado o no ha terminado.
     * @return Gestor de almac&eacute;n o {@code null} si no se ha cargado a&uacute;n.
     */
	public AOKeyStoreManager getKeyStoreManager() {
		return this.keyStoreManager;
	}

	/**
     * Devuelve el tipo de almac&eacute;n configurado.
     * @return Tipo de almac&eacute;n.
     */
	public AOKeyStore getAOKeyStore() {
		return this.keystore;
	}

	/**
	 * Devuelve la excepci&oacute;n producida al cargarse el gestor de almac&eacute;n de claves
	 * en caso de producirse.
	 * @return Excepci&oacute;n producida durante la carga del almac&eacute;n o {@code null} si
	 * a&uacute;n no ha terminado la cargar del almac&eacute;n o si se produjo alguna al cargarse.
	 */
	public Exception getException() {
		return this.exception;
	}

	/**
	 * Devuelve true si es un almac&eacute;n que pertenece a alg&uacute;n sistema operativo.
	 * @param aoks Almac&eacute;n a comprobar.
	 * @return true en caso de que pertenezca a un sistema operativo, false en caso contrario.
	 */
    public static boolean isOsKeyStore(final AOKeyStore aoks) {
		if (AOKeyStore.WINDOWS.equals(aoks)
			|| AOKeyStore.APPLE.equals(aoks)
			|| AOKeyStore.SHARED_NSS.equals(aoks)
			|| AOKeyStore.MOZ_UNI.equals(aoks)) {
			return true;
		}
		return false;
    }

}
