package es.gob.afirma.android.crypto;

import android.app.Activity;

public final class KeyStoreManagerFactory {

	private KeyStoreManagerFactory() {
		// Se prohibe crear instancias
	}

	public static MobileKeyStoreManager getKeyStoreManager(final Activity activity) {
		return new Android4KeyStoreManager(activity);
	}

}
