package es.gob.afirma.ui.utils;

import java.util.ArrayList;
import java.util.List;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.KeyStoreConfiguration;

/** Clase para la recuperaci&oacute;n de los distintos almacenes del sistema
 * disponibles para realizar distintas operaciones. */
public final class KeyStoreLoader {

	private KeyStoreLoader() {
		// No permitimos la instanciacion
	}

    /** Recupera los almacenes compatibles con el sistema y preparados
     * para contener certificados de firma.
     * @return Listado de almacenes. */
    public static KeyStoreConfiguration[] getKeyStoresToSign() {

        final List<KeyStoreConfiguration> stores = new ArrayList<>();

        if (Platform.getOS().equals(Platform.OS.WINDOWS)) {
            stores.add(new KeyStoreConfiguration(AOKeyStore.WINDOWS, null, null));
        }
        if (Platform.getOS().equals(Platform.OS.MACOSX)) {
            stores.add(new KeyStoreConfiguration(AOKeyStore.APPLE, null, null));
        }

        stores.add(new KeyStoreConfiguration(AOKeyStore.MOZ_UNI, null, null));
        stores.add(new KeyStoreConfiguration(AOKeyStore.DNIEJAVA, null, null));
        stores.add(new KeyStoreConfiguration(AOKeyStore.PKCS12, null, null));

        return stores.toArray(new KeyStoreConfiguration[0]);
    }

    /** Recupera los almacenes compatibles con el sistema y preparados
     * para contener certificados para envoltura de datos.
     * @return Listado de almacenes. */
    public static KeyStoreConfiguration[] getKeyStoresToWrap() {

        final List<KeyStoreConfiguration> stores = new ArrayList<>();

        stores.add(new KeyStoreConfiguration(AOKeyStore.SINGLE, null, null));
        stores.add(new KeyStoreConfiguration(AOKeyStore.PKCS12, null, null));
        stores.add(new KeyStoreConfiguration(AOKeyStore.PKCS11, null, null));

        if (Platform.getOS().equals(Platform.OS.WINDOWS)) {
            stores.add(new KeyStoreConfiguration(AOKeyStore.WINADDRESSBOOK, null, null));
        }

        return stores.toArray(new KeyStoreConfiguration[0]);
    }

    /** Recupera los almacenes compatibles con el sistema y preparados
     * para contener certificados para desenvoltura de datos.
     * @return Listado de almacenes. */
    public static KeyStoreConfiguration[] getKeyStoresToUnWrap() {

        final List<KeyStoreConfiguration> stores = new ArrayList<>();

        if (Platform.getOS().equals(Platform.OS.WINDOWS)) {
            stores.add(new KeyStoreConfiguration(AOKeyStore.WINDOWS, null, null));
        }
        if (Platform.getOS().equals(Platform.OS.MACOSX)) {
            stores.add(new KeyStoreConfiguration(AOKeyStore.APPLE, null, null));
        }

        stores.add(new KeyStoreConfiguration(AOKeyStore.MOZ_UNI, null, null));
        stores.add(new KeyStoreConfiguration(AOKeyStore.SINGLE, null, null));
        stores.add(new KeyStoreConfiguration(AOKeyStore.PKCS12, null, null));
        stores.add(new KeyStoreConfiguration(AOKeyStore.PKCS11, null, null));

        return stores.toArray(new KeyStoreConfiguration[0]);
    }
}
