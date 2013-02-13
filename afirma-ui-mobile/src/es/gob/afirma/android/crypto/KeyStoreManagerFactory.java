package es.gob.afirma.android.crypto;

import android.app.Activity;

/** Facrtor&iacute;a de gestores de contrase&ntuilde;as y claves para Android. */
public final class KeyStoreManagerFactory {

	private KeyStoreManagerFactory() {
		// Se prohibe crear instancias
	}

	/** Obtiene el gestor de contrase&ntuilde;as y claves m&aacute;s apropiado seg&uacute;n el entorno
	 * operativo y el hardware encontrado.
	 * @param activity Actividad padre
	 * @return Gestor de contrase&ntuilde;as y claves */
	public static MobileKeyStoreManager getKeyStoreManager(final Activity activity) {
		return new Android4KeyStoreManager(activity);
	}

}
