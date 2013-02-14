package es.gob.afirma.android.crypto;

import android.app.Activity;

/** Facrtor&iacute;a de gestores de contrase&ntuilde;as y claves para Android. */
public final class KeyStoreManagerFactory {

	private KeyStoreManagerFactory() {
		// Se prohibe crear instancias
	}

	private static final String ES_GOB_AFIRMA = "es.gob.afirma"; //$NON-NLS-1$
	private static final String AET_PKCS11_STORE = "PKCS11KeyStore"; //$NON-NLS-1$

	/** Obtiene el gestor de contrase&ntuilde;as y claves m&aacute;s apropiado seg&uacute;n el entorno
	 * operativo y el hardware encontrado.
	 * @param activity Actividad padre
	 * @return Gestor de contrase&ntuilde;as y claves */
	public static MobileKeyStoreManager getKeyStoreManager(final Activity activity) {
//		// Primero intentamos la instanciacion de un PKCS#11 MSC AET
//		try {
//			final Properties providerConfiguration = new Properties();
//			providerConfiguration.put(ContextManager.CONTEXT_MANAGER_TYPE, ContextManagerType.MICROSD.getType());
//			providerConfiguration.put(AETProvider.PROVIDER_TYPE, AETProviderType.JAVA.getType());
//			final Provider provider = AETProvider.getInstance(providerConfiguration, activity.getApplicationContext());
//			Security.addProvider(provider);
//			final KeyStore ks = KeyStore.getInstance(AET_PKCS11_STORE, provider);
//			ks.load(null, "pin".toCharArray());
//			final Enumeration<String> aliases = ks.aliases();
//			while (aliases.hasMoreElements()) {
//				Log.i(ES_GOB_AFIRMA, aliases.nextElement());
//			}
//		}
//		catch (final Exception e) {
//			Log.w(ES_GOB_AFIRMA, "No se ha podido obtener el proveedor PKCS#11 AET: " + e); //$NON-NLS-1$
//		}
		return new Android4KeyStoreManager(activity);
	}

}
