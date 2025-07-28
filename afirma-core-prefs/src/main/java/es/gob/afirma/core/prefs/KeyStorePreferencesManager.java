package es.gob.afirma.core.prefs;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

public final class KeyStorePreferencesManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final Preferences USER_PREFERENCES;
	private static Preferences SYSTEM_PREFERENCES;

	/** Indica cual fue el &uacute;ltimo almac&eacute;n de claves seleccionado por el usuario. */
	public static String lastSelectedKeystore = null;

	/** Indica cual fue la libreria del &uacute;ltimo almac&eacute;n de claves seleccionado por el usuario. */
	public static String lastSelectedKeystoreLib = null;

	private static final String KEYSTORES_NODE = "/es/gob/afirma/core/keystores"; //$NON-NLS-1$
	private static final String SYSTEM_UPDATED_KEYSTORES_NODE = "/es/gob/afirma/core/systemkeystores"; //$NON-NLS-1$

	private static final String DUMMY = "dummy"; //$NON-NLS-1$

	static {

		final Preferences userRootPreferences = Preferences.userRoot();

		// Cargamos las preferencias de usuario
		USER_PREFERENCES = userRootPreferences.node(KEYSTORES_NODE);

		// Si existen preferencias del sistema actualizadas y esta permitido el que se actualicen
		// las preferencias, usaremos esas. Si no, cargaremos las del sistema por defecto. Si no,
		// no cargaremos ninguna
		Preferences systemPreferences;
		try {
			if (userRootPreferences.nodeExists(SYSTEM_UPDATED_KEYSTORES_NODE) && isAutommaticUpdateConfigAllowed()) {
				systemPreferences = userRootPreferences.node(SYSTEM_UPDATED_KEYSTORES_NODE);
			}
			else if (Preferences.systemRoot().nodeExists(KEYSTORES_NODE)){
				systemPreferences = Preferences.systemRoot().node(KEYSTORES_NODE);
			}
			else {
				systemPreferences = null;
			}
		}
		catch (final Exception e) {
			LOGGER.warning("No se pueden cargar las preferencias establecidas a nivel de sistema: " + e); //$NON-NLS-1$
			systemPreferences = null;
		}
		SYSTEM_PREFERENCES = systemPreferences;
	}

	private KeyStorePreferencesManager() {
		// No permitimos la instanciacion
	}

	/**
	 * Obtiene todos los registros de almacenes de claves de tarjetas inteligentes del sistema.
	 * @return Mapa con pares de clave-valor donde la clave es el nombre de la tarjeta y
	 * el valor es la ruta hacia el controlador de la misma.
	 */
	public static Map<String, String> getSystemSmartCardsRegistered() {
		final Map<String, String> result = new HashMap<>();
		if (SYSTEM_PREFERENCES != null) {
			try {
				final String[] childNamesSystem = SYSTEM_PREFERENCES.childrenNames();
				if (childNamesSystem != null && childNamesSystem.length > 0) {
					for (int i = 0 ; i < childNamesSystem.length ; i++) {
						final String cardName = SYSTEM_PREFERENCES.node(childNamesSystem[i]).keys()[0];
						final String lib = SYSTEM_PREFERENCES.node(childNamesSystem[i]).get(cardName, null);
						result.put(cardName, lib);
					}
				}
			} catch (final BackingStoreException e) {
				LOGGER.log(Level.SEVERE, "No se han podido obtener los registros sobre tarjetas inteligentes del sistema", e); //$NON-NLS-1$
			}
		}

		return result;
	}


	/**
	 * Obtiene todos los registros de almacenes de claves de tarjetas inteligentes del usuario.
	 * @return Mapa con pares de clave-valor donde la clave es el nombre de la tarjeta y
	 * el valor es la ruta hacia el controlador de la misma.
	 */
	public static Map<String, String> getUserSmartCardsRegistered() {
		final Map<String, String> result = new HashMap<>();
		if (USER_PREFERENCES != null) {
			try {
				final String[] childNamesUser = USER_PREFERENCES.childrenNames();
				if (childNamesUser != null && childNamesUser.length > 0) {
					for (int i = 0 ; i < childNamesUser.length ; i++) {
						final String cardName = USER_PREFERENCES.node(childNamesUser[i]).keys()[0];
						final String lib = USER_PREFERENCES.node(childNamesUser[i]).get(cardName, null);
						result.put(cardName, lib);
					}
				}
			} catch (final BackingStoreException e) {
				LOGGER.severe("No se han podido obtener los registros sobre tarjetas inteligentes del usuario: " + e); //$NON-NLS-1$
			}
		}

		return result;
	}

	/**
	 * Obtiene todos los registros de almacenes de claves de tarjetas inteligentes en forma de mapa
	 * @return Mapa que a su vez contiene mapas con pares de clave-valor donde la clave es el nombre de la tarjeta y
	 * el valor es la ruta hacia el controlador de la misma.
	 */
	public static Map<String, String> getAllSmartCardsMap() {
		final Map<String, String> result = new HashMap<>();
		final Map<String, String> userSmartCards = getUserSmartCardsRegistered();
		final Map<String, String> systemSmartCards = getSystemSmartCardsRegistered();
		result.putAll(userSmartCards);
		result.putAll(systemSmartCards);
		return result;
	}

	/**
	 * Registra los almacenes de claves de tarjetas inteligentes para el usuario, ignorando
	 * aquellos que ya estuviesen definidos.
	 * @param smartCards Mapa de tarjetas configuradas con su nombre y la ruta de la biblioteca
	 * PKCS#11.
	 */
	public static void putUserSmartCardsMap(final Map<String, Object> smartCards) {

		final Map<String, String> smartCardsRegistered = getAllSmartCardsMap();

		for (final String smartCard : smartCards.keySet()) {
			// Las tarjetas que agregue el usuario no pueden configurar un PKCS#11 que ya este registrado
			final boolean existController = checkExistsController(smartCardsRegistered, (String) smartCards.get(smartCard));
			if (!existController) {
				// Nos aseguramos de que el nombre no coincida con ningun otro
				final String smartCardNameChecked = checkCorrectName(smartCardsRegistered, smartCard);
				addSmartCardToUserRec(smartCardNameChecked, (String) smartCards.get(smartCard));
				// Actualizamos el listado para que al agregar el resto de tarjetas no se pueda repetir el nombre
				smartCardsRegistered.put(smartCardNameChecked, (String) smartCards.get(smartCard));
			}
		}
	}

	/**
	 * Registra los almacenes de claves de tarjetas inteligentes para el sistema.
	 * @param smartCards Mapa de tarjetas configuradas con su nombre y la ruta de la biblioteca
	 * PKCS#11.
	 */
	public static void putSystemSmartCardsMap(final Map<String, Object> smartCards) {

		if (SYSTEM_PREFERENCES == null) {
			createSystemPrefs();
		}

		// Comprobamos si tenemos permisos para guardar en las preferencias del sistema
		setSystemPermissions();

		// Eliminamos las tarjetas que hubiese establecidas a nivel de sistema
		try {
			for (final String smartCardRegistry : SYSTEM_PREFERENCES.childrenNames()) {
				SYSTEM_PREFERENCES.node(smartCardRegistry).removeNode();
			}
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se pudo borrar una de las tarjetas del sistema anteriormente registrada", e); //$NON-NLS-1$
		}

		// Agregamos las nuevas tarjetas con cuidado de no pisar los nombres de las que ya existan
		// (incluimos las del sistema por si no se hubiesen borrado correctamente)
		final Map<String, String> smartCardsRegistered = getAllSmartCardsMap();

		for (final String smartCard : smartCards.keySet()) {
			// Nos aseguramos de que el nombre no coincida con ningun otro
			final String smartCardNameChecked = checkCorrectName(smartCardsRegistered, smartCard);
			addSmartCardToSystemRec(smartCardNameChecked, (String) smartCards.get(smartCard));
			// Actualizamos el listado para que al agregar el resto de tarjetas no se pueda repetir el nombre
			smartCardsRegistered.put(smartCardNameChecked, (String) smartCards.get(smartCard));
		}
	}

	/**
	 * Crea las preferencias de sistema y comprueba que sean editables.
	 */
	private static void createSystemPrefs() {
		try {
			SYSTEM_PREFERENCES = Preferences.systemNodeForPackage(KeyStorePreferencesManager.class);
		}
		catch (final Exception e) {
			unlockSystemPreferences();
		}

		setSystemPermissions();
	}

	/**
	 * Realiza una operacion y, si falla, cambia las preferencias al nodo de usuario para permitir
	 * su edici&oacute;n.
	 */
	private static void setSystemPermissions() {
		try {
			SYSTEM_PREFERENCES.putBoolean(DUMMY, true);
			SYSTEM_PREFERENCES.remove(DUMMY);
		}
		catch (final Exception e) {
			if (!SYSTEM_PREFERENCES.isUserNode()) {
				unlockSystemPreferences();
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
		for (final String smartCardName : smartCardsRegistered.keySet()) {
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
	 * @param systemSmartCardsRegistered Mapa de tarjetas inteligentes registradas en el sistema.
	 * @param userSmartCardsRegistered Mapa de tarjetas inteligentes registradas por el usuario.
	 * @param newSmartCardName Nombre de tarjeta inteligente a comprobar.
	 * @return Devuelve el nuevo nombre a registrar
	 */
	private static String checkCorrectName(final Map<String, String> smartCardsRegistered, final String newSmartCardName) {

		int i = 1;
		String result = newSmartCardName;
		while (smartCardsRegistered.containsKey(result)) {
			result = newSmartCardName + "-" + i++; //$NON-NLS-1$
		}
		return result;
	}

	/**
	 * Agrega al registro del usuario la tarjeta inteligente pasada por par&aacute;metro.
	 * @param smartCardName Nombre de la tarjeta.
	 * @param libPath Ruta del controlador PKCS11.
	 * @return true si se ha agregado correctamente.
	 */
	public static boolean addSmartCardToUserRec(final String smartCardName, final String libPath) {

		boolean regAdded = false;
		boolean noExistRec = true;
		int cont = 1;

		while (noExistRec) {

			try {
				if (!USER_PREFERENCES.nodeExists(Integer.toString(cont))) {
					USER_PREFERENCES.node(Integer.toString(cont)).put(smartCardName, libPath);
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
	 * Agrega al registro del sistema la tarjeta inteligente pasada por par&aacute;metro.
	 * @param smartCardName Nombre de la tarjeta.
	 * @param libPath Ruta del controlador PKCS#11.
	 * @return {@code true} si se ha agregado correctamente.
	 */
	private static boolean addSmartCardToSystemRec(final String smartCardName, final String libPath) {

		boolean regAdded = false;
		boolean noExistRec = true;
		int cont = 1;

		while (noExistRec) {

			try {
				if (!SYSTEM_PREFERENCES.nodeExists(Integer.toString(cont))) {
					final Preferences ksNode = SYSTEM_PREFERENCES.node(Integer.toString(cont));
					ksNode.put(smartCardName, libPath);
					regAdded = true;
					noExistRec = false;
				}
			} catch (final Exception e) {
				if (!SYSTEM_PREFERENCES.isUserNode()) {
					unlockSystemPreferences();
					return addSmartCardToSystemRec(smartCardName, libPath);
				}
				LOGGER.severe("No se ha podido agregar la tarjeta inteligente " + smartCardName + " al registro del sistema: " + e); //$NON-NLS-1$ //$NON-NLS-2$
				noExistRec = false;
			}

			cont++;
		}

		try {
			SYSTEM_PREFERENCES.flush();
		} catch (final Exception e) {
			LOGGER.severe("No se han podido guardar las tarjetas inteligentes en el registro del sistema: " + e); //$NON-NLS-1$
			return false;
		}

		return regAdded;
	}


	/**
	 * Desbloquea el guardado en las preferencias del sistema cambiando el nodo
	 * de guardado a uno en el nodo de preferencias de usuario.
	 */
	private static void unlockSystemPreferences() {
		SYSTEM_PREFERENCES = Preferences.userRoot().node(SYSTEM_UPDATED_KEYSTORES_NODE);
	}

	/**
	 * Elimina del registro el almac&eacute;n indicado
	 * @param name Nombre del almac&eacute;n de la tarjeta a eliminar
	 * @return devuelve true en caso de que se haya eliminado correctamente
	 */
	public static boolean deleteSmartCardRec(final String name) {

		boolean deletedRec = false;

		try {
			final String[] childNames = USER_PREFERENCES.childrenNames();
			if (childNames != null && childNames.length > 0) {
				for (int i = 0 ; i < childNames.length ; i++) {
						final String smartCardName = USER_PREFERENCES.node(childNames[i]).keys()[0];
						if (name.equals(smartCardName)) {
							USER_PREFERENCES.node(childNames[i]).removeNode();
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
		lastSelectedKeystore = ksName;
	}

	/**
	 * Devuelve el &uacute;ltimo almac&eacute;n de claves seleccionado por el usuario
	 * @return Nombre del &uacute;ltimo almac&eacute;n seleccionado por el usuario
	 */
	public static String getLastSelectedKeystore() {
		return lastSelectedKeystore;
	}

	/**
	 * Asigna la librer&iacute;a del &uacute;ltimo almac&eacute;n de claves seleccionado por el usuario
	 * @param lib Nombre de libreria
	 */
	public static void setLastSelectedKeystoreLib(final String lib) {
		lastSelectedKeystoreLib = lib;
	}

	/**
	 * Devuelve la librer&iacute;a del &uacute;ltimo almac&eacute;n de claves seleccionado por el usuario
	 * @return Nombre de la librer&iacute;a del &uacute;ltimo almac&eacute;n seleccionado por el usuario
	 */
	public static String getLastSelectedKeystoreLib() {
		return lastSelectedKeystoreLib;
	}

	/**
	 * Elimina todas preferencias del usuario de la aplicaci&oacute;n referentes a almacenes sin
	 * incluir las tarjetas inteligentes.
	 * @throws BackingStoreException Si ocurre un error eliminando las preferencias.
	 */
	public static void clearKeyStorePrefs() throws BackingStoreException {
		for (final String key : USER_PREFERENCES.keys()) {
			USER_PREFERENCES.remove(key);
		}
		USER_PREFERENCES.flush();
	}

	/**
	 * Elimina todas las preferencias de sistema de los almacenes.
	 * @throws BackingStoreException Cuando no se pueden eliminar las preferencias.
	 */
	public static void removeSystemPrefs() throws BackingStoreException {
		if (Preferences.systemRoot().nodeExists(KEYSTORES_NODE)){
			final Preferences node = Preferences.systemRoot().node(KEYSTORES_NODE);
			final Preferences parent = node.parent();
			node.removeNode();
			removeEmptyTree(parent);
		}
	}

	private static void removeEmptyTree(final Preferences node) throws BackingStoreException {
		Preferences parent = node;
		while (!parent.name().isEmpty() && parent.childrenNames().length == 0 && parent.keys().length == 0) {
			final Preferences newParent = parent.parent();
			parent.removeNode();
			parent = newParent;
		}
	}

	//TODO: Se deberia crear un objeto general en el core para gestionar todas las preferencias del cliente y
	// que cada modulo guarde las suyas. Aqui se agregan preferencias gestionadas por el PreferenceManager
	// del modulo afirma-ui-simple-configurator-common porque son las que identifican si hay que actualizar
	// las preferencias o no.

	private static final String INTERNAL_SYSTEM_PREFERENCE_NODE = "/es/gob/afirma/standalone/ui/internalpreferences"; //$NON-NLS-1$
	private static final String SYSTEM_PREFERENCE_ALLOW_UPDATE_CONFIG = "allowUpdateConfig"; //$NON-NLS-1$

	private static boolean isAutommaticUpdateConfigAllowed() {
		try {
			final Preferences systemRoot = Preferences.systemRoot();
			return systemRoot.nodeExists(INTERNAL_SYSTEM_PREFERENCE_NODE)
					&& systemRoot.node(INTERNAL_SYSTEM_PREFERENCE_NODE).getBoolean(SYSTEM_PREFERENCE_ALLOW_UPDATE_CONFIG, false);
		}
		catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se ha podido comprobar si esta activa la actualizacion automatica de la configuracion", e); //$NON-NLS-1$
			return false;
		}
	}

}