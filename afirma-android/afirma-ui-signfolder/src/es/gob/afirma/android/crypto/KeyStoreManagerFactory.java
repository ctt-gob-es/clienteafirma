package es.gob.afirma.android.crypto;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.Provider;
import java.security.Security;
import java.util.Properties;

import android.app.Activity;
import android.content.Context;
import android.hardware.usb.UsbDevice;
import android.hardware.usb.UsbManager;
import android.util.Log;
import es.gob.afirma.android.crypto.LoadKeyStoreManagerTask.KeystoreManagerListener;
import es.gob.afirma.android.gui.PinDialog;
import es.gob.afirma.android.signfolder.SFConstants;

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
	 * @param ksfl Clase donde hay que establecer el almac&eacute;n
	 * @param ksml Clase a la que hay que notificar la finalizaci&oacute;n de la
	 *             carga e inicializaci&oacute;n del gestor de claves y certificados
	 * @param usbDevice Dispositivo USB en el caso de almacenes de claves externos
	 * @param usbManager Gestor de dispositivos USB en el caso de almacenes de claves externos */
	public static void initKeyStoreManager(final Activity activity,
			final LoadKeyStoreManagerTask ksfl,
			final KeystoreManagerListener ksml,
			final UsbDevice usbDevice,
			final UsbManager usbManager) {

		// Primero buscamos un DNIe en el CCID USB
		if (usbDevice != null && usbManager != null) {
			try {
				final Class<?> androidCCIDConnectionClass = Class.forName("es.inteco.labs.android.usb.AndroidCCIDConnection"); //$NON-NLS-1$
				final Object androidCCIDConnectionObject = androidCCIDConnectionClass.getConstructor(
						UsbManager.class, UsbDevice.class).newInstance(usbManager, usbDevice);

				final Class<?> dnieProviderClass = Class.forName("es.gob.jmulticard.jse.provider.DnieProvider"); //$NON-NLS-1$
				final Provider p = (Provider) dnieProviderClass.getConstructor(androidCCIDConnectionClass).newInstance(androidCCIDConnectionObject);

				Security.addProvider(p);
				// Obtenemos el almacen unicamente para ver si falla
				KeyStore.getInstance("DNI", p); //$NON-NLS-1$

				// Si llegamos hasta aqui preguntamos el PIN al usuario
				// KeyStore: "DNI", Proveedor: "DNIeJCAProvider"
				final PinDialog pinDialog = PinDialog.newInstance("DNIeJCAProvider", "DNI", ksml); //$NON-NLS-1$ //$NON-NLS-2$
				pinDialog.setLoadKeyStoreManagerTask(ksfl);
				pinDialog.show(activity.getFragmentManager(), "PinDialog"); //$NON-NLS-1$

				return;
			}
			catch (final ClassNotFoundException e) {
				Log.w(ES_GOB_AFIRMA, "No se encuentran las bibliotecas de acceso al DNIe: " + e.toString()); //$NON-NLS-1$
			}
			catch (final NoSuchMethodException e) {
				Log.w(ES_GOB_AFIRMA, "No se encuentran las bibliotecas de acceso al DNIe: " + e.toString()); //$NON-NLS-1$
			}
			catch (final KeyStoreException e) {
				Log.w(ES_GOB_AFIRMA, "Se ha encontrado un CCID USB, pero no un DNIe en el: " + e); //$NON-NLS-1$
			}
			catch (final Exception e) {
				Log.w(ES_GOB_AFIRMA, "No se ha podido instanciar el controlador del DNIe: " + e); //$NON-NLS-1$
			}
		}


		// Despues intentamos la instanciacion de un PKCS#11 MSC AET
		try {
			// Todo el proceso se hace por reflexion, porque el entorno de ejecucion es completamente opcional
			// Si falla en este primer bloque devolvemos el almacen de Android, de forma transparente para el usuario
			final Properties providerConfiguration = new Properties();
			final Class<?> contextManagerClass = Class.forName("com.aet.android.javaprovider.context.ContextManager"); //$NON-NLS-1$
			final Field contextManagerTypeFiled = contextManagerClass.getDeclaredField("CONTEXT_MANAGER_TYPE"); //$NON-NLS-1$
			final Class<?> contextManagerTypeClass = Class.forName("com.aet.android.javaprovider.context.ContextManagerType"); //$NON-NLS-1$
			final Field microsdField = contextManagerTypeClass.getDeclaredField("MICROSD"); //$NON-NLS-1$
			final Object microsdObject = microsdField.get(null);
			final Method getTypeMethod = contextManagerTypeClass.getDeclaredMethod("getType"); //$NON-NLS-1$
			final Object typeValue = getTypeMethod.invoke(microsdObject);

			providerConfiguration.put(
					contextManagerTypeFiled.get(null),
					typeValue
					);

			final Class<?> aetProviderClass = Class.forName("com.aet.android.javaprovider.AETProvider"); //$NON-NLS-1$
			final Field aetProviderTypeField = aetProviderClass.getDeclaredField("PROVIDER_TYPE"); //$NON-NLS-1$
			final Object aetProviderTypeObject = aetProviderTypeField.get(null);
			final Class<?> aetProviderTypeClass = Class.forName("com.aet.android.javaprovider.AETProviderType"); //$NON-NLS-1$
			final Field javaField = aetProviderTypeClass.getDeclaredField("JAVA"); //$NON-NLS-1$
			final Object javaObject = javaField.get(null);
			final Method aetProviderGetTypeMethod = aetProviderTypeClass.getDeclaredMethod("getType"); //$NON-NLS-1$
			final Object javaTypeValue = aetProviderGetTypeMethod.invoke(javaObject);

			providerConfiguration.put(
					aetProviderTypeObject,
					javaTypeValue
					);

			final Method aetProviderGetInstanceMethod = aetProviderClass.getDeclaredMethod("getInstance", Properties.class, Context.class); //$NON-NLS-1$
			final Provider provider = (Provider) aetProviderGetInstanceMethod.invoke(
				null,
				providerConfiguration,
				activity.getApplicationContext()
			);

			Security.addProvider(provider);
			Log.i(SFConstants.LOG_TAG, "Anadido el proveedor AET: " + provider.getName());  //$NON-NLS-1$

			// Obtenemos el almacen unicamente para ver si falla
			KeyStore.getInstance(AET_PKCS11_STORE, provider);

			Log.i(ES_GOB_AFIRMA, "Se ha instanciado correctamente el proveedor AET");  //$NON-NLS-1$

			// A partir de este punto, si falla, terminamos con error y no devolvemos el almacen de Android, mostrando
			// un dialogo de error al usuario

			// Si llegamos hasta aqui sin errores preguntamos el PIN al usuario
			// KeyStore: "PKCS11KeyStore", Proveedor: "AETProvider"
			final PinDialog pinDialog = PinDialog.newInstance("AETProvider", "PKCS11KeyStore", ksml); //$NON-NLS-1$ //$NON-NLS-2$
			pinDialog.setLoadKeyStoreManagerTask(ksfl);
			pinDialog.show(activity.getFragmentManager(), "PinDialog"); //$NON-NLS-1$

			return;
		}
		catch (final Exception e) {
			Log.w(ES_GOB_AFIRMA, "No se ha detectado una MSC: " + e); //$NON-NLS-1$
		}

		ksfl.setKeyStore(new Android4KeyStoreManager(activity));
	}

}