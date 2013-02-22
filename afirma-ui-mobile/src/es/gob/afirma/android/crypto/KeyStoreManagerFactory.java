package es.gob.afirma.android.crypto;

import android.app.Activity;

/** Facrtor&iacute;a de gestores de contrase&ntuilde;as y claves para Android. */
public final class KeyStoreManagerFactory {

	private KeyStoreManagerFactory() {
		// Se prohibe crear instancias
	}

//	private static final String ES_GOB_AFIRMA = "es.gob.afirma"; //$NON-NLS-1$
//	private static final String AET_PKCS11_STORE = "PKCS11KeyStore"; //$NON-NLS-1$

	/** Obtiene el gestor de contrase&ntuilde;as y claves m&aacute;s apropiado seg&uacute;n el entorno
	 * operativo y el hardware encontrado.
	 * @param activity Actividad padre
	 * @return Gestor de contrase&ntuilde;as y claves */
	public static MobileKeyStoreManager getKeyStoreManager(final Activity activity) {
//		// Primero intentamos la instanciacion de un PKCS#11 MSC AET
//		try {
//
//			// Todo el proceso se hace por reflexion, porque el entorno de ejecucion es completamente opcional
//			final Properties providerConfiguration = new Properties();
//
//			final Class<?> contextManagerClass = Class.forName("com.aet.android.javaprovider.context.ContextManager"); //$NON-NLS-1$
//			final Field contextManagerTypeFiled = contextManagerClass.getDeclaredField("CONTEXT_MANAGER_TYPE"); //$NON-NLS-1$
//
//			final Class<?> contextManagerTypeClass = Class.forName("com.aet.android.javaprovider.context.ContextManagerType"); //$NON-NLS-1$
//			final Field microsdField = contextManagerTypeClass.getDeclaredField("MICROSD"); //$NON-NLS-1$
//			final Object microsdObject = microsdField.get(null);
//			final Method getTypeMethod = contextManagerTypeClass.getDeclaredMethod("getType"); //$NON-NLS-1$
//			final Object typeValue = getTypeMethod.invoke(microsdObject);
//
//			providerConfiguration.put(
//					contextManagerTypeFiled.get(null),
//					typeValue
//			);
//
//
//			final Class<?> aetProviderClass = Class.forName("com.aet.android.javaprovider.AETProvider"); //$NON-NLS-1$
//			final Field aetProviderTypeField = aetProviderClass.getDeclaredField("PROVIDER_TYPE"); //$NON-NLS-1$
//			final Object aetProviderTypeObject = aetProviderTypeField.get(null);
//
//			final Class<?> aetProviderTypeClass = Class.forName("com.aet.android.javaprovider.AETProviderType"); //$NON-NLS-1$
//			final Field javaField = aetProviderTypeClass.getDeclaredField("JAVA"); //$NON-NLS-1$
//			final Object javaObject = javaField.get(null);
//			final Method aetProviderGetTypeMethod = aetProviderTypeClass.getDeclaredMethod("getType"); //$NON-NLS-1$
//			final Object javaTypeValue = aetProviderGetTypeMethod.invoke(javaObject);
//
//			providerConfiguration.put(
//				aetProviderTypeObject,
//				javaTypeValue
//			);
//
//			final Method aetProviderGetInstanceMethod = aetProviderClass.getDeclaredMethod("getInstance", Properties.class, Context.class); //$NON-NLS-1$
//
//			final Provider provider = (Provider) aetProviderGetInstanceMethod.invoke(
//				null, providerConfiguration, activity.getApplicationContext()
//			);
//			Security.addProvider(provider);
//			final KeyStore ks = KeyStore.getInstance(AET_PKCS11_STORE, provider);
//			ks.load(null, "pin".toCharArray());
//			final Enumeration<String> aliases = ks.aliases();
//			if (aliases.hasMoreElements()) {
//				return new AndroidJcaKeyStoreManager(ks, "".toCharArray()); //$NON-NLS-1$
//			}
//		}
//		catch (final Exception e) {
//			Log.w(ES_GOB_AFIRMA, "No se ha podido obtener el proveedor PKCS#11 AET: " + e); //$NON-NLS-1$
//		}
		return new Android4KeyStoreManager(activity);
	}

}
