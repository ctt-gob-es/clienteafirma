package es.gob.afirma.core.keystores;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

public final class KeyStorePreferencesManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final Preferences USER_PREFERENCES;
	private static final Preferences SYSTEM_PREFERENCES;
	private static final Properties PROPERTIES;

	private static final String TRUE_VALUE = "true"; //$NON-NLS-1$
	private static final String FALSE_VALUE = "false"; //$NON-NLS-1$

	/** Indica cual fue el &uacute;ltimo almac&eacute;n de claves seleccionado por el usuario. */
	public static final String PREFERENCE_LAST_KEYSTORE_SELECTED = "lastKeystoreSelected"; //$NON-NLS-1$

	/** Indica cual fue la libreria del &uacute;ltimo almac&eacute;n de claves seleccionado por el usuario. */
	public static final String PREFERENCE_LAST_KEYSTORE_LIB_SELECTED = "lastKeystoreLibSelected"; //$NON-NLS-1$

	/** Indica si omitir o no el certificado de autenticaci&oacute;n para DNIe. */
	public static final String PREFERENCE_SKIP_AUTH_CERT_DNIE = "skipAuthCertDnie"; //$NON-NLS-1$

	static {
		USER_PREFERENCES = Preferences.userNodeForPackage(KeyStorePreferencesManager.class);
		SYSTEM_PREFERENCES = Preferences.systemNodeForPackage(KeyStorePreferencesManager.class);
		PROPERTIES = new Properties();
		try {
			PROPERTIES.load(KeyStorePreferencesManager.class.getResourceAsStream("/properties/preferences.properties")); //$NON-NLS-1$
		}
		catch (final Exception e) {
			LOGGER.severe(
				"No han podido cargarse los valores por defecto del fichero de configuracion de preferencias, se usaran los valores por defecto: " //$NON-NLS-1$
					+ e
			);
		}
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
		final Map<String, String> result = new HashMap<>();
		try {
			final String[] childNames = USER_PREFERENCES.childrenNames();
			if (childNames != null && childNames.length > 0) {
				for (int i = 0 ; i < childNames.length ; i++) {
						final String cardName = USER_PREFERENCES.node(childNames[i]).keys()[0];
						final String lib = USER_PREFERENCES.node(childNames[i]).get(cardName, null);
						result.put(cardName, lib);
				}
			}
		} catch (final BackingStoreException e) {
				LOGGER.severe("No se han podido obtener los registros sobre tarjetas inteligentes " + e); //$NON-NLS-1$
		}

		return result;
	}

	/**
	 * Obtiene todos los registros de almacenes de claves de tarjetas inteligentes en forma de mapa
	 * @return Mapa que a su vez contiene mapas con pares de clave-valor donde la clave es el nombre de la tarjeta y
	 * el valor es la ruta hacia el controlador de la misma.
	 */
	public static Map<String, Object> getSmartCardsMap() {
		final Map<String, Object> result = new HashMap<>();
		try {
			final String[] childNames = USER_PREFERENCES.childrenNames();
			if (childNames != null && childNames.length > 0) {
				for (int i = 0 ; i < childNames.length ; i++) {
						final String cardName = USER_PREFERENCES.node(childNames[i]).keys()[0];
						final String lib = USER_PREFERENCES.node(childNames[i]).get(cardName, null);
						final Map<String, String> smartCardRecord = new HashMap<>();
						smartCardRecord.put(cardName, lib);
						result.put(childNames[i], smartCardRecord);
				}
			}
		} catch (final BackingStoreException e) {
				LOGGER.severe("No se han podido obtener los registros sobre tarjetas inteligentes " + e); //$NON-NLS-1$
		}

		return result;
	}

	/**
	 * Obtiene todos los registros de almacenes de claves de tarjetas inteligentes en forma de mapa
	 * @return Mapa con pares de clave-valor donde la clave es el nombre de la tarjeta y
	 * el valor es la ruta hacia el controlador de la misma.
	 */
	public static Map<String, String> getSmartCardNameControllerMap() {
		final Map<String, String> result = new HashMap<>();
		try {
			final String[] childNames = USER_PREFERENCES.childrenNames();
			if (childNames != null && childNames.length > 0) {
				for (int i = 0 ; i < childNames.length ; i++) {
						final String cardName = USER_PREFERENCES.node(childNames[i]).keys()[0];
						final String lib = USER_PREFERENCES.node(childNames[i]).get(cardName, null);
						result.put(cardName, lib);
				}
			}
		} catch (final BackingStoreException e) {
				LOGGER.severe("No se han podido obtener los registros sobre tarjetas inteligentes " + e); //$NON-NLS-1$
		}

		return result;
	}

	/**
	 * Registra los almacenes de claves de tarjetas inteligentes
	 */
	public static void putSmartCardsMap(final Map<String, Object> smartCards) {
		for(final String smartCard : smartCards.keySet()) {
			final Map<String, String> smartCardKeyValue = (Map<String, String>) smartCards.get(smartCard);
			for(final String smartCardKey : smartCardKeyValue.keySet()) {
					final Map<String, String> smartCardsRegistered = getSmartCardNameControllerMap();
					final boolean existController = checkExistsController(smartCardsRegistered, smartCardKeyValue.get(smartCardKey));
					if (!existController) {
						final String smartCardNameChecked = checkCorrectName(smartCardsRegistered, smartCardKey);
						addSmartCardToRec(smartCardNameChecked, smartCardKeyValue.get(smartCardKey));
					}
			}
		}
	}

	/**
	 * Comprueba si existe una tarjeta inteligente con la misma ruta de controlador en el registro.
	 * @param smartCardsRegistered Mapa de tarjetas inteligentes registradas.
	 * @param newController Almac&eacute;n a comprobar si existe o no.
	 * @return Devuelve true en caso de que ya exista, false en caso contrario.
	 */
	private static boolean checkExistsController(final Map<String, String> smartCardsRegistered, final String newController) {
		for(final String smartCardName : smartCardsRegistered.keySet()) {
			final String controllerName = smartCardsRegistered.get(smartCardName);
			if (controllerName.equals(newController)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Comprueba si ya existe una tarjeta inteligente con el nombre indicado por par&aacute;metro, y si es as&iacute;,
	 * genera uno nuevo.
	 * @param smartCardsRegistered Mapa de tarjetas inteligentes registradas.
	 * @param newSmartCardName Nombre de tarjeta inteligente a comprobar.
	 * @return Devuelve el nuevo nombre a registrar
	 */
	private static String checkCorrectName(final Map<String, String> smartCardsRegistered, final String newSmartCardName) {
		String result = newSmartCardName;
		if (smartCardsRegistered.containsKey(newSmartCardName)) {
			boolean existCardName = true;
			int cont = 1;
			while (existCardName) {
				final String newName = result + "-" + cont; //$NON-NLS-1$
				if (!smartCardsRegistered.containsKey(newName)) {
					result = newName;
					existCardName = false;
				} else {
					cont++;
				}
			}
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
				if (!USER_PREFERENCES.nodeExists("/es/gob/afirma/core/keystores/" + cont)) { //$NON-NLS-1$
					USER_PREFERENCES.node("/es/gob/afirma/core/keystores/" + cont).put(smartCardName, libPath); //$NON-NLS-1$
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
			final String[] childNames = USER_PREFERENCES.node("/es/gob/afirma/core/keystores").childrenNames(); //$NON-NLS-1$
			if (childNames != null && childNames.length > 0) {
				for (int i = 0 ; i < childNames.length ; i++) {
						final String smartCardName = USER_PREFERENCES.node("/es/gob/afirma/core/keystores/" + childNames[i]).keys()[0]; //$NON-NLS-1$
						if (name.equals(smartCardName)) {
							USER_PREFERENCES.node("/es/gob/afirma/core/keystores/" + childNames[i]).removeNode(); //$NON-NLS-1$
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
		USER_PREFERENCES.put(PREFERENCE_LAST_KEYSTORE_SELECTED, ksName);
	}

	/**
	 * Devuelve el &uacute;ltimo almac&eacute;n de claves seleccionado por el usuario
	 * @return Nombre del &uacute;ltimo almac&eacute;n seleccionado por el usuario
	 */
	public static String getLastSelectedKeystore() {
		return USER_PREFERENCES.get(PREFERENCE_LAST_KEYSTORE_SELECTED, ""); //$NON-NLS-1$
	}

	/**
	 * Asigna la librer&iacute;a del &uacute;ltimo almac&eacute;n de claves seleccionado por el usuario
	 * @param lib Nombre de libreria
	 */
	public static void setLastSelectedKeystoreLib(final String lib) {
		USER_PREFERENCES.put(PREFERENCE_LAST_KEYSTORE_LIB_SELECTED, lib);
	}

	/**
	 * Devuelve la librer&iacute;a del &uacute;ltimo almac&eacute;n de claves seleccionado por el usuario
	 * @return Nombre de la librer&iacute;a del &uacute;ltimo almac&eacute;n seleccionado por el usuario
	 */
	public static String getLastSelectedKeystoreLib() {
		return USER_PREFERENCES.get(PREFERENCE_LAST_KEYSTORE_LIB_SELECTED, ""); //$NON-NLS-1$
	}

	/**
	 * Asigna la preferencia para omitir o no el cerificado de autenticaci&oacute;n para DNIe
	 * @param skipCert Valor para la preferencia
	 */
	public static void setSkipAuthCertDNIe(final boolean skipCert) {
		// Si el valor que se le va a asignar a la propiedad es el mismo que el del sistema,
		// se elimina en el registro del usuario y permanece la del sistema
		final boolean systemValue = getBooleanSystemPreference(PREFERENCE_SKIP_AUTH_CERT_DNIE);
		if (skipCert == systemValue) {
			USER_PREFERENCES.remove(PREFERENCE_SKIP_AUTH_CERT_DNIE);
		} else if (skipCert != getSkipAuthCertDNIe()) {
			// Si la propiedad ha cambiado con respecto a la configurada en el sistema o por defecto, se guardara
			USER_PREFERENCES.putBoolean(PREFERENCE_SKIP_AUTH_CERT_DNIE, skipCert);
		}
	}

	/**
	 * Obtiene la preferencia para omitir o no el cerificado de autenticaci&oacute;n para DNIe
	 * @return true si se va a omitir el certificado
	 */
	public static boolean getSkipAuthCertDNIe() {
		return USER_PREFERENCES.getBoolean(PREFERENCE_SKIP_AUTH_CERT_DNIE, getBooleanSystemPreference(PREFERENCE_SKIP_AUTH_CERT_DNIE));
	}

	/**
	 * Recupera el valor de una cadena de texto almacenada en las propiedades del sistema.
	 * @param key Clave del valor que queremos recuperar.
	 * @return La preferencia almacenada o la que se encuentra configurada por defecto si no se encontr&oacute;. */
	public static boolean getBooleanSystemPreference(final String key) {
		return SYSTEM_PREFERENCES.getBoolean(key, getBooleanDefaultPreference(key));
	}

	/**
	 * Recupera el valor de una cadena de texto almacenada en un fichero de propiedades.
	 *  @param key Clave del valor que queremos recuperar.
	 * @return La preferencia almacenada o {@code def} si no se encontr&oacute;. */
	public static boolean getBooleanDefaultPreference(final String key) {
		return Boolean.parseBoolean(PROPERTIES.getProperty(key));
	}

	/**
	 * Se obtienen las preferencias a exportar que se hayan registrado en el sistema y en el usuario.
	 * Si la preferencia existe en usuario y sistema, tendr&aacute; prioridad la del usuario.
	 * @return Mapa con las claves y valores del sistema.
	 */
	public static Map<String, Object> getPrefsToExport() {

		final Map<String, Object> result = new HashMap<String, Object>();
		try {
			final String[] systemKeys = SYSTEM_PREFERENCES.keys();
			for (int i = 0 ; i < systemKeys.length ; i++) {
				final String value = SYSTEM_PREFERENCES.get(systemKeys[i], null);
				if (value != null && (value.equals(TRUE_VALUE) || value.equals(FALSE_VALUE))) {
					result.put(systemKeys[i], Boolean.parseBoolean(value));
				} else {
					result.put(systemKeys[i], value);
				}
			}
			final String[] userKeys = USER_PREFERENCES.keys();
			for (int i = 0 ; i < userKeys.length ; i++) {
				final String value = USER_PREFERENCES.get(userKeys[i], null);
				if (value != null && (value.equals(TRUE_VALUE) || value.equals(FALSE_VALUE))) {
					result.put(userKeys[i], Boolean.parseBoolean(value));
				} else {
					result.put(userKeys[i], value);
				}
			}
		} catch (final BackingStoreException e) {
			LOGGER.severe(
					"Error al obtener preferencias configuradas por el usuario" //$NON-NLS-1$
						+ e
				);
		}
		return result;
	}

}
