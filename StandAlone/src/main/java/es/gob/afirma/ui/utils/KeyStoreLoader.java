package es.gob.afirma.ui.utils;

import java.util.Vector;

import es.gob.afirma.keystores.KeyStoreConfiguration;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.Platform;

/**
 * Clase para la recuperaci&oacute;n de los distintos almacenes del sistema
 * disponibles para realizar distintas operaciones.
 */
public class KeyStoreLoader {

	/**
	 * Recupera los almacenes compatibles con el sistema y preparados
	 * para contener certificados de firma.
	 * @return Listado de almacenes.
	 */
	public static KeyStoreConfiguration[] getKeyStoresToSign() {
		
		Vector<KeyStoreConfiguration> stores = new Vector<KeyStoreConfiguration>();

		if (Platform.getOS().equals(Platform.OS.WINDOWS)) 
			stores.add(new KeyStoreConfiguration(AOConstants.AOKeyStore.WINDOWS, null, null)); //$NON-NLS-1$ //$NON-NLS-2$
		if (Platform.getOS().equals(Platform.OS.MACOSX)) 
			stores.add(new KeyStoreConfiguration(AOConstants.AOKeyStore.APPLE, null, null)); //$NON-NLS-1$ //$NON-NLS-2$
		
//		try {
//			MozillaKeyStoreUtilities.getSystemNSSLibDir();
			stores.add(new KeyStoreConfiguration(AOConstants.AOKeyStore.MOZ_UNI, null, null));
//		} catch (Exception e) {}
		
		stores.add(new KeyStoreConfiguration(AOConstants.AOKeyStore.PKCS12, null, null));
		
		return stores.toArray(new KeyStoreConfiguration[0]);
	}
	
	/**
	 * Recupera los almacenes compatibles con el sistema y preparados
	 * para contener certificados para envoltura de datos.
	 * @return Listado de almacenes.
	 */
	public static KeyStoreConfiguration[] getKeyStoresToWrap() {
		
		Vector<KeyStoreConfiguration> stores = new Vector<KeyStoreConfiguration>();

		stores.add(new KeyStoreConfiguration(AOConstants.AOKeyStore.SINGLE, null, null));
		stores.add(new KeyStoreConfiguration(AOConstants.AOKeyStore.PKCS12, null, null));
		
		if (Platform.getOS().equals(Platform.OS.WINDOWS)) {
			stores.add(new KeyStoreConfiguration(AOConstants.AOKeyStore.WINADDRESSBOOK, null, null));
		}
		
		return stores.toArray(new KeyStoreConfiguration[0]);
	}
}
