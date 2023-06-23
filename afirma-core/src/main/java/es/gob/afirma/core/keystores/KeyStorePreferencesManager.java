package es.gob.afirma.core.keystores;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

public final class KeyStorePreferencesManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final Preferences PREFERENCES;

	/** Indica cual fue el &uacute;ltimo almac&eacute;n de claves seleccionado por el usuario. */
	public static final String PREFERENCE_LAST_KEYSTORE_SELECTED= "lastKeystoreSelected"; //$NON-NLS-1$

	/** Indica cual fue la libreria del &uacute;ltimo almac&eacute;n de claves seleccionado por el usuario. */
	public static final String PREFERENCE_LAST_KEYSTORE_LIB_SELECTED= "lastKeystoreLibSelected"; //$NON-NLS-1$

	/** Indica si omitir o no el certificado de autenticaci&oacute;n para DNIe. */
	public static final String PREFERENCE_SKIP_AUTH_CERT_DNIE= "skipAuthCertDnie"; //$NON-NLS-1$

	static {
		PREFERENCES = Preferences.userNodeForPackage(KeyStorePreferencesManager.class);
	}

	private KeyStorePreferencesManager() {
		// No permitimos la instanciacion
	}

	/**
	 * Obtiene todos los registros de almacenes de claves de tarjetas inteligentes
	 * @return Mapa con pares de clave-valor donde la clave es el nombre de la tarjeta y
	 * el valor es la ruta hacia el controlador de la misma.
	 */
	public static Map<String, String> getSmartCardsRegistered() {
		final Map<String, String> result = new HashMap<String, String>();
		try {
			final String[] childNames = PREFERENCES.node("/es/gob/afirma/core/keystores").childrenNames(); //$NON-NLS-1$
			if (childNames != null && childNames.length > 0) {
				for (int i = 0 ; i < childNames.length ; i++) {
						final String cardName = PREFERENCES.node("/es/gob/afirma/core/keystores/" + childNames[i]).keys()[0]; //$NON-NLS-1$
						final String lib = PREFERENCES.node("/es/gob/afirma/core/keystores/" + childNames[i]).get(cardName, null); //$NON-NLS-1$
						result.put(cardName, lib);
				}
			}
		} catch (final BackingStoreException e) {
				LOGGER.severe("No se han podido obtener los registros sobre tarjetas inteligentes " + e); //$NON-NLS-1$
		}

		return result;
	}


	/**
	 * Agrega el registro de la tarjeta inteligente pasada por par&aacute;metro
	 * @param smartCardName Nombre de la tarjeta
	 * @param libPath Ruta del controlador PKCS11
	 * @return true si se ha agregado correctamente
	 */
	public static boolean addSmartCardToRec(final String smartCardName, final String libPath) {

		boolean regAdded = false;
		boolean noExistRec = true;
		int cont = 1;

		while (noExistRec) {

			try {
				if (!PREFERENCES.nodeExists("/es/gob/afirma/core/keystores/" + cont)) { //$NON-NLS-1$
					PREFERENCES.node("/es/gob/afirma/core/keystores/" + cont).put(smartCardName, libPath); //$NON-NLS-1$
					regAdded = true;
					noExistRec = false;
				}
			} catch (final BackingStoreException e) {
				LOGGER.severe("No se ha podido agregar la siguiente tarjeta inteligente al registro:" + smartCardName + " " + e); //$NON-NLS-1$ //$NON-NLS-2$
				noExistRec = false;
			}

			cont++;
		}

		return regAdded;
	}

	/**
	 * Elimina del registro el almac&eacute;n indicado
	 * @param name Nombre del almac&eacute;n de la tarjeta a eliminar
	 * @return devuelve true en caso de que se haya eliminado correctamente
	 */
	public static boolean deleteSmartCardRec(final String name) {

		boolean deletedRec = false;

		try {
			final String[] childNames = PREFERENCES.node("/es/gob/afirma/core/keystores").childrenNames(); //$NON-NLS-1$
			if (childNames != null && childNames.length > 0) {
				for (int i = 0 ; i < childNames.length ; i++) {
						final String smartCardName = PREFERENCES.node("/es/gob/afirma/core/keystores/" + childNames[i]).keys()[0]; //$NON-NLS-1$
						if (name.equals(smartCardName)) {
							PREFERENCES.node("/es/gob/afirma/core/keystores/" + childNames[i]).removeNode(); //$NON-NLS-1$
							deletedRec = true;
							break;
						}
				}
			}
		} catch (final BackingStoreException e) {
				LOGGER.severe("No se ha podido eliminar la tarjeta inteligente: " + name + " " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}

		return deletedRec;
	}

	/**
	 * Asigna el &uacute;ltimo almac&eacute;n de claves seleccionado por el usuario
	 * @param ksName Nombre del almac&eacute;n
	 */
	public static void setLastSelectedKeystore(final String ksName) {
		PREFERENCES.put(PREFERENCE_LAST_KEYSTORE_SELECTED, ksName);
	}

	/**
	 * Devuelve el &uacute;ltimo almac&eacute;n de claves seleccionado por el usuario
	 * @return Nombre del &uacute;ltimo almac&eacute;n seleccionado por el usuario
	 */
	public static String getLastSelectedKeystore() {
		return PREFERENCES.get(PREFERENCE_LAST_KEYSTORE_SELECTED, ""); //$NON-NLS-1$
	}

	/**
	 * Asigna la librer&iacute;a del &uacute;ltimo almac&eacute;n de claves seleccionado por el usuario
	 * @param lib Nombre de libreria
	 */
	public static void setLastSelectedKeystoreLib(final String lib) {
		PREFERENCES.put(PREFERENCE_LAST_KEYSTORE_LIB_SELECTED, lib);
	}

	/**
	 * Devuelve la librer&iacute;a del &uacute;ltimo almac&eacute;n de claves seleccionado por el usuario
	 * @return Nombre de la librer&iacute;a del &uacute;ltimo almac&eacute;n seleccionado por el usuario
	 */
	public static String getLastSelectedKeystoreLib() {
		return PREFERENCES.get(PREFERENCE_LAST_KEYSTORE_LIB_SELECTED, ""); //$NON-NLS-1$
	}

	/**
	 * Asigna la preferencia para omitir o no el cerificado de autenticaci&oacute;n para DNIe
	 * @param skipCert Valor para la preferencia
	 */
	public static void setSkipAuthCertDNIe(final boolean skipCert) {
		PREFERENCES.putBoolean(PREFERENCE_SKIP_AUTH_CERT_DNIE, skipCert);
	}

	/**
	 * Obtiene la preferencia para omitir o no el cerificado de autenticaci&oacute;n para DNIe
	 * @return true si se va a omitir el certificado
	 */
	public static boolean getSkipAuthCertDNIe() {
		return PREFERENCES.getBoolean(PREFERENCE_SKIP_AUTH_CERT_DNIE, false);
	}

}
