package es.gob.afirma.ui.utils;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Logger;
import java.util.prefs.BackingStoreException;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.ui.principal.AccessibilityOptionsPane;
import es.gob.afirma.ui.principal.ContextOptionsPane;
import es.gob.afirma.ui.principal.Main;
import es.gob.afirma.ui.principal.MainOptionsPane;
import es.gob.afirma.ui.principal.UserProfile;

/** Gestor de perfiles de usuario.
 * @author Carlos Gamuci */
public final class ProfileManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private ProfileManager() {
		// No permitimos la instanciacion
	}

    /** Nombre del perfil por defecto del sistema. */
    public static final String DEFAULT_PROFILE_NAME = '<' + "Predeterminado" + '>'; //$NON-NLS-1$

    private static final String IDS_SEPARATOR = ","; //$NON-NLS-1$

    private static final int MAX_NUM_PROFILES_ALLOWED = 100;

    private static final String KEY_LAST_PROFILE_NAME = "ultimo.perfil.cargado"; //$NON-NLS-1$

    private static final String PREFIX_KEY = "options."; //$NON-NLS-1$

    private static final String KEY_PROFILES = PREFIX_KEY + "profiles"; //$NON-NLS-1$

    private static final String KEY_DEFAULT_PROFILE_NAME = ".default"; //$NON-NLS-1$

    private static final String KEY_PROFILE_NAME = ".name"; //$NON-NLS-1$

    private static final String GENERAL_PREFIX_KEY = ".general."; //$NON-NLS-1$

    private static final String KEY_GENERAL_ADVANCED_VIEW = GENERAL_PREFIX_KEY + "advanced"; //$NON-NLS-1$

    private static final String KEY_GENERAL_ALGORITHM_NAME = GENERAL_PREFIX_KEY + "algo.name"; //$NON-NLS-1$

    private static final String KEY_GENERAL_USE_ALGO_XML = GENERAL_PREFIX_KEY + "algo.useXML"; //$NON-NLS-1$

    private static final String KEY_GENERAL_USE_POLICY = GENERAL_PREFIX_KEY + "policy"; //$NON-NLS-1$

    private static final String KEY_GENERAL_POLICY_OID = GENERAL_PREFIX_KEY + "policy.oid"; //$NON-NLS-1$

    private static final String KEY_GENERAL_POLICY_URL = GENERAL_PREFIX_KEY + "policy.url"; //$NON-NLS-1$

    private static final String KEY_GENERAL_POLICY_HASH = GENERAL_PREFIX_KEY + "policy.hash"; //$NON-NLS-1$

    private static final String CONTEXT_PREFIX_KEY = ".context."; //$NON-NLS-1$

    private static final String KEY_CONTEXT_SUBJECT = CONTEXT_PREFIX_KEY + "subject"; //$NON-NLS-1$

    private static final String KEY_CONTEXT_PLACE = CONTEXT_PREFIX_KEY + "place"; //$NON-NLS-1$

    private static final String KEY_CONTEXT_CONTACT = CONTEXT_PREFIX_KEY + "contact"; //$NON-NLS-1$

    private static final String KEY_CONTEXT_PADES_FORMAT = CONTEXT_PREFIX_KEY + "pades.format"; //$NON-NLS-1$

    private static final String ACCESIBILITY_PREFIX_KEY = ".accesibility."; //$NON-NLS-1$

    private static final String KEY_ACCESIBILITY_FONT_BIG = ACCESIBILITY_PREFIX_KEY + "fontBig"; //$NON-NLS-1$

    private static final String KEY_ACCESIBILITY_FONT_STYLE = ACCESIBILITY_PREFIX_KEY + "fontStyle"; //$NON-NLS-1$

    private static final String KEY_ACCESIBILITY_HIGH_CONTRAST = ACCESIBILITY_PREFIX_KEY + "highContrast"; //$NON-NLS-1$

    private static final String KEY_ACCESIBILITY_FOCUS = ACCESIBILITY_PREFIX_KEY + "focus"; //$NON-NLS-1$

    private static final String KEY_ACCESIBILITY_MAXIMIZED = ACCESIBILITY_PREFIX_KEY + "maximized"; //$NON-NLS-1$

    private static final String KEY_ACCESIBILITY_CURSOR = ACCESIBILITY_PREFIX_KEY + "cursor"; //$NON-NLS-1$

    /** Listado de relaciones con los siguientes 3 valores:
     * <ul>
     * <li>Nombre de opci&oacute;n de configuraci&oacute;n que se almacena en las preferencias del sistema.</li>
     * <li>Nombre interno de variable utilizado en la configuraci&oacute;n de la herramienta.</li>
     * <li>Valor por defecto</li>
     * </ul> */
    private static final String[][] CONVERSE_VALUES = new String[][] {
            {
                    KEY_GENERAL_ADVANCED_VIEW, MainOptionsPane.MAIN_ADVANCED_VIEW, "false"}, //$NON-NLS-1$
            {
                    KEY_GENERAL_ALGORITHM_NAME, MainOptionsPane.MAIN_DEFAULT_ALGORITHM, MainOptionsPane.DEFAULT_DEFAULT_ALGORITHM },
            {
                    KEY_GENERAL_USE_ALGO_XML, MainOptionsPane.MAIN_ALGORITHM_XML, "false"}, //$NON-NLS-1$
            {
                    KEY_GENERAL_USE_POLICY, MainOptionsPane.MAIN_POLICY_ESTABLISHED, "false"}, //$NON-NLS-1$
            {
                    KEY_GENERAL_POLICY_OID, MainOptionsPane.MAIN_POLICY_IDENTIFIER, ""}, //$NON-NLS-1$
            {
                    KEY_GENERAL_POLICY_URL, MainOptionsPane.MAIN_POLICY_QUALIFIER, ""}, //$NON-NLS-1$
            {
                    KEY_GENERAL_POLICY_HASH, MainOptionsPane.MAIN_POLICY_HASH, ""}, //$NON-NLS-1$
            {
                    KEY_CONTEXT_SUBJECT, ContextOptionsPane.KEY_SUBJECT, ""}, //$NON-NLS-1$
            {
                    KEY_CONTEXT_PLACE, ContextOptionsPane.KEY_PRODUCTION_PLACE, ""}, //$NON-NLS-1$
            {
                    KEY_CONTEXT_CONTACT, ContextOptionsPane.KEY_CONTACT_INFO, ""}, //$NON-NLS-1$
            {
                    KEY_CONTEXT_PADES_FORMAT, ContextOptionsPane.KEY_PADES_FORMAT, AOSignConstants.PADES_SUBFILTER_BASIC},
            {
                    KEY_ACCESIBILITY_CURSOR, AccessibilityOptionsPane.MAIN_CURSOR_SIZE, "false"}, //$NON-NLS-1$
            {
                    KEY_ACCESIBILITY_FOCUS, AccessibilityOptionsPane.MAIN_FOCUS_VISIBLE, "false"}, //$NON-NLS-1$
            {
                    KEY_ACCESIBILITY_FONT_BIG, AccessibilityOptionsPane.MAIN_FONT_SIZE, "false"}, //$NON-NLS-1$
            {
                    KEY_ACCESIBILITY_FONT_STYLE, AccessibilityOptionsPane.MAIN_FONT_STYLE, "false"}, //$NON-NLS-1$
            {
                    KEY_ACCESIBILITY_HIGH_CONTRAST, AccessibilityOptionsPane.MAIN_HIGHT_CONTRAST, "false"}, //$NON-NLS-1$
            {
                    KEY_ACCESIBILITY_MAXIMIZED, AccessibilityOptionsPane.MAIN_WINDOWS_SIZE, "false"} //$NON-NLS-1$
            };

    private static String getPreference(final String preferenceKey) {
        return getPreference(preferenceKey, null);
    }

    private static String getPreference(final String preferenceKey, final String defaultValue) {
        return Main.getPreferences().get(preferenceKey, defaultValue);
    }

    /** Obtiene los identificadores de perfiles creados en el sistema y los devuelve
     * en un conjunto.
     * @return Conjunto de identificadores. */
    private static Set<String> getProfileIdsSet() {

        final String profiles = Main.getPreferences().get(KEY_PROFILES, null);
        final HashSet<String> profilesSet = new HashSet<>();
        if (profiles != null && profiles.trim().length() > 0) {
            for (final String profileId : profiles.split(IDS_SEPARATOR)) {
                profilesSet.add(profileId);
            }
        }
        return profilesSet;
    }

    /** Devuelve un identificador de perfil que no este siendo utilizado. Si se
     * alcanza el l&iacute;mite de identificadores permitidos, se devolver&aacute; {@code null}.
     * @return Identificador disponible para su uso. */
    public static String getFreeProfileId() {
        final Set<String> createdProfiles = getProfileIdsSet();
        if (createdProfiles.isEmpty()) {
            return "0"; //$NON-NLS-1$
        }

        for (int i = 0; i < MAX_NUM_PROFILES_ALLOWED; i++) {
            if (!createdProfiles.contains(Integer.toString(i))) {
                return Integer.toString(i);
            }
        }

        return null;
    }

    /** Consulta si existe un perfil definido con un determinado nombre.
     * @param profileName Nombre del perfil que se est&aacute; buscando.
     * @return {@code true} si existe un perfil con ese nombre, {@code false} en
     *         caso contrario. */
    public static boolean existProfileName(final String profileName) {

        final Iterator<String> i = getProfileIdsSet().iterator();
        while (i.hasNext()) {
            final String id = i.next();
            final String name = getPreference(PREFIX_KEY + id + KEY_PROFILE_NAME);
            if (profileName.equals(name)) {
                return true;
            }
        }

        return false;
    }

    /** Recupera el nombre de un perfil a partir de su identificador. Si no se
     * encuentra un perfil con el nombre se devuelve {@code null}.
     * @param profileName Nombre del perfil.
     * @return Identificador del perfil. */
    public static String getProfileIdByName(final String profileName) {
        final Iterator<String> i = getProfileIdsSet().iterator();
        while (i.hasNext()) {
            final String id = i.next();
            final String name = getPreference(PREFIX_KEY + id + KEY_PROFILE_NAME);
            if (profileName.equals(name)) {
                return id;
            }
        }

        return null;
    }

    /** Almacena la configuraci&oacute;n de la herramienta para el perfil indicado
     * mediante su identificador.
     * @param id Identificador con el que almacenar la configuraci&oacute;n.
     * @param name Nombre asignado al perfil.
     * @param config Configuraci&oacute;n que se desea almacenar.
     * @throws IllegalArgumentException Cuando se indique un nombre de fichero no v&aacute;lido. */
    public static void saveConfiguration(final String id, final String name, final Properties config) {
        if (id == null) {
            return;
        }
        if (name == null || name.indexOf('<') != -1 || name.indexOf('>') != -1) {
            throw new IllegalArgumentException();
        }
        final String profilePrefix = PREFIX_KEY + id;
        for (final String[] element : CONVERSE_VALUES) {
            Main.getPreferences().put(profilePrefix + element[0], config.getProperty(element[1], element[2]));
        }
        Main.getPreferences().put(profilePrefix + KEY_PROFILE_NAME, name.trim());
        addNewId(id);
	    try {
			Main.getPreferences().flush();
		}
	    catch (final BackingStoreException e) {
			LOGGER.warning(
				"No se han podido guardar las preferencias de configuracion de la aplicacion: " + e //$NON-NLS-1$
			);
		}
    }

    /** Guarda una preferencia. Si se indica un valor nulo para la preferencia la borra.
     * @param key Clave de la preferencia a asignar.
     * @param value Valor a asignar a la preferencia o nulo para borrarla.
     * @throws IllegalArgumentException Cuando se indica una clave nula. */
    public static void setDinamicPreference(final String key, final String value) {

    	if (key == null) {
            throw new IllegalArgumentException("No se ha indicado la clave de la preferencia"); //$NON-NLS-1$
        }

    	String completeKey = PREFIX_KEY;
    	final String id = UserProfile.getCurrentProfileId();
        if (id != null) {
            completeKey += id + "."; //$NON-NLS-1$
        }
        completeKey += key;

        if (value != null) {
        	Main.getPreferences().put(completeKey, value);
        } else {
        	Main.getPreferences().remove(completeKey);
		}

	    try {
			Main.getPreferences().flush();
		}
	    catch (final BackingStoreException e) {
			LOGGER.warning(
				"No se han podido guardar las preferencias de configuracion de la aplicacion: " + e //$NON-NLS-1$
			);
		}
    }

    /** Recupera una preferencia.
     * @param key Clave de la preferencia a asignar.
     * @return Valor de la preferencia o {@code null} si no est&aacute; definida.
     * @throws IllegalArgumentException Cuando se indica una clave nula. */
    public static String getDinamicPreference(final String key) {

    	if (key == null) {
            throw new IllegalArgumentException("No se ha indicado la clave de la preferencia"); //$NON-NLS-1$
        }

    	String completeKey = PREFIX_KEY;
    	final String id = UserProfile.getCurrentProfileId();
        if (id != null) {
            completeKey += id + "."; //$NON-NLS-1$
        }
        completeKey += key;

        return Main.getPreferences().get(completeKey, null);
    }

    private static void addNewId(final String id) {

        final Set<String> idsSet = getProfileIdsSet();
        idsSet.add(id);

        String idsString = ""; //$NON-NLS-1$
        final String[] ids = idsSet.toArray(new String[0]);
        for (int i = 0; i < ids.length - 1; i++) {
            idsString += ids[i] + IDS_SEPARATOR;
        }

        if (ids.length > 0) {
            idsString += ids[ids.length - 1];
        }

        Main.getPreferences().put(KEY_PROFILES, idsString);
    }

    /** Elimina un perfil previamente creado.
     * @param id Identificador del perfil que se desea eliminar. */
    public static void removeConfiguration(final String id) {
        if (id == null) {
            return;
        }
        final String profilePrefix = PREFIX_KEY + id;
        for (final String[] element : CONVERSE_VALUES) {
            Main.getPreferences().remove(profilePrefix + element[0]);
        }
        Main.getPreferences().remove(profilePrefix + KEY_PROFILE_NAME);
        removeId(id);
	    try {
			Main.getPreferences().flush();
		}
	    catch (final BackingStoreException e) {
			LOGGER.warning(
				"No se han podido guardar las preferencias de configuracion de la aplicacion: " + e //$NON-NLS-1$
			);
		}
    }

    private static void removeId(final String id) {
        final Set<String> idsSet = getProfileIdsSet();
        idsSet.remove(id);

        String idsString = ""; //$NON-NLS-1$
        final String[] ids = idsSet.toArray(new String[0]);
        for (int i = 0; i < ids.length - 1; i++) {
            idsString += ids[i] + IDS_SEPARATOR;
        }

        if (ids.length > 0) {
            idsString += ids[ids.length - 1];
        }

        Main.getPreferences().put(KEY_PROFILES, idsString);
    }

    /** Recupera la configuraci&oacute;n correspondiente a un perfil. Si se utiliza
     * el nombre del perfil por defecto, se devolver&aacute; la configuraci&oacute;n
     * por defecto.
     * @param name Nombre del perfil.
     * @return Configuraci&oacute;n almacenada para ese perfil. */
    public static Properties getConfiguration(final String name) {

        if (DEFAULT_PROFILE_NAME.equals(name)) {
            return getDefaultConfiguration();
        }

        final String id = getProfileIdByName(name);
        final String profilePrefix = PREFIX_KEY + id;

        final Properties config = new Properties();
        for (final String[] element : CONVERSE_VALUES) {
            config.setProperty(element[1], Main.getPreferences().get(profilePrefix + element[0], element[2]));
        }

        return config;
    }

    /** Recupera la configuraci&oacute;n del perfil por defecto de la aplicaci&oacute;n.
     * @return Configuraci&oacute;n por defecto. */
    public static Properties getDefaultConfiguration() {
        final Properties config = new Properties();
        for (final String[] element : CONVERSE_VALUES) {
            config.setProperty(element[1], element[2]);
        }
        return config;
    }

    /** Recupera la configuraci&oacute;n del perfil por defecto tal como la haya configurado
     * el usuario.
     * @return Configuraci&oacute;n por defecto modificada. */
    public static Properties getDefaultConfigurationModified() {

    	final Properties config = new Properties();
    	final Properties defaultConfig = getDefaultConfiguration();

    	saveDefaultModifiedValue(config, MainOptionsPane.MAIN_ADVANCED_VIEW, defaultConfig, "true"); //$NON-NLS-1$
    	saveDefaultModifiedValue(config, MainOptionsPane.MAIN_DEFAULT_ALGORITHM, defaultConfig, AOSignConstants.DEFAULT_SIGN_ALGO);
    	saveDefaultModifiedValue(config, MainOptionsPane.MAIN_ALGORITHM_XML, defaultConfig, "false"); //$NON-NLS-1$
    	saveDefaultModifiedValue(config, MainOptionsPane.MAIN_POLICY_ESTABLISHED, defaultConfig, "false"); //$NON-NLS-1$
    	saveDefaultModifiedValue(config, MainOptionsPane.MAIN_POLICY_IDENTIFIER, defaultConfig, ""); //$NON-NLS-1$
    	saveDefaultModifiedValue(config, MainOptionsPane.MAIN_POLICY_HASH, defaultConfig, ""); //$NON-NLS-1$
    	saveDefaultModifiedValue(config, MainOptionsPane.MAIN_POLICY_QUALIFIER, defaultConfig, ""); //$NON-NLS-1$
    	saveDefaultModifiedValue(config, ContextOptionsPane.KEY_SUBJECT, defaultConfig, ""); //$NON-NLS-1$
    	saveDefaultModifiedValue(config, ContextOptionsPane.KEY_PRODUCTION_PLACE, defaultConfig, ""); //$NON-NLS-1$
    	saveDefaultModifiedValue(config, ContextOptionsPane.KEY_CONTACT_INFO, defaultConfig, ""); //$NON-NLS-1$
    	saveDefaultModifiedValue(config, ContextOptionsPane.KEY_PADES_FORMAT, defaultConfig, AOSignConstants.PADES_SUBFILTER_BASIC);
    	saveDefaultModifiedValue(config, AccessibilityOptionsPane.MAIN_FONT_SIZE, defaultConfig, "false"); //$NON-NLS-1$
    	saveDefaultModifiedValue(config, AccessibilityOptionsPane.MAIN_FOCUS_VISIBLE, defaultConfig, "false"); //$NON-NLS-1$
    	saveDefaultModifiedValue(config, AccessibilityOptionsPane.MAIN_FONT_STYLE, defaultConfig, "false"); //$NON-NLS-1$
    	saveDefaultModifiedValue(config, AccessibilityOptionsPane.MAIN_CURSOR_SIZE, defaultConfig, "false"); //$NON-NLS-1$
    	saveDefaultModifiedValue(config, AccessibilityOptionsPane.MAIN_HIGHT_CONTRAST, defaultConfig, "false"); //$NON-NLS-1$
    	saveDefaultModifiedValue(config, AccessibilityOptionsPane.MAIN_WINDOWS_SIZE, defaultConfig, "false"); //$NON-NLS-1$

        return config;
    }

    private static void saveDefaultModifiedValue(final Properties config, final String key, final Properties defaultConfig, final String defaultValue) {
    	config.setProperty(key,
    			Main.getPreferences().get(KEY_PROFILES + KEY_DEFAULT_PROFILE_NAME + '.' + key,
    					defaultConfig.getProperty(key, defaultValue)));
    }

    /** recupera el nombre de perfil asociado a un ID.
     * @param id identificador del perfil
     * @return Nombre del perfil o {@code null} si no existe un perfil con ese identificador. */
    public static String getProfileName(final String id) {
        if (id == null) {
            return DEFAULT_PROFILE_NAME;
        }
        return getPreference(PREFIX_KEY + id + KEY_PROFILE_NAME);
    }

    /** Recupera el listado de nombres de perfiles registrados en la aplicaci&oacute;n.
     * @return Nombres de los perfiles. */
    public static String[] getProfilesNames() {
        final List<String> names = new ArrayList<>();
        final Iterator<String> i = getProfileIdsSet().iterator();
        while (i.hasNext()) {
            names.add(getPreference(PREFIX_KEY + i.next() + KEY_PROFILE_NAME));
        }
        return names.toArray(new String[0]);
    }

    /** Establece cu&aacute;l fue el nombre del &uacute;ltimo perfil cargado por la herramienta.
     * @param name Nombre de perfil. */
    public static void setLastProfileName(final String name) {
        if (name == null) {
            Main.getPreferences().remove(KEY_LAST_PROFILE_NAME);
        }
        else {
            Main.getPreferences().put(KEY_LAST_PROFILE_NAME, name);
        }
	    try {
			Main.getPreferences().flush();
		}
	    catch (final BackingStoreException e) {
			LOGGER.warning(
				"No se ha podido guardar el nombre del ultimo perfil creado: " + e //$NON-NLS-1$
			);
		}
    }

    /** Recupera el nombre del &uacute;ltimo perfil cargado por la herramienta. Si se estableci&oacute;
     * el perfil por defecto devuelve {@code null}.
     * @return Nombre de perfil. */
    public static String getLastProfileName() {
        return Main.getPreferences().get(KEY_LAST_PROFILE_NAME, null);
    }

    /** Recupera el valor de una opci&oacute;n de accesibilidad.
     * @param option Nombre de la opci&oacute;n de accesibilidad.
     * @param name Nombre del perfil.
     * @return Boolean indicando el valor de la opci&oacute;n. */
    public static String getAccessibilityOptionValue(final String option, final String name) {
        final Properties properties = getConfiguration(name);
        return properties.getProperty(option, null);
    }

    /**
     * Almacena en el perfil por defecto la configuraci&oacute;n indicada.
     * @param config Configuraci&oacute;n a almacenar.
     */
    public static void savePreferencesOnDefaultProfile(final Properties config) {

    	savePreferenceOnDefaultProfile(config, MainOptionsPane.MAIN_ADVANCED_VIEW, "true"); //$NON-NLS-1$
    	savePreferenceOnDefaultProfile(config, MainOptionsPane.MAIN_DEFAULT_ALGORITHM, AOSignConstants.DEFAULT_SIGN_ALGO);
    	savePreferenceOnDefaultProfile(config, MainOptionsPane.MAIN_ALGORITHM_XML, "false"); //$NON-NLS-1$
    	savePreferenceOnDefaultProfile(config, MainOptionsPane.MAIN_POLICY_ESTABLISHED, "false"); //$NON-NLS-1$
    	savePreferenceOnDefaultProfile(config, MainOptionsPane.MAIN_POLICY_IDENTIFIER, ""); //$NON-NLS-1$
    	savePreferenceOnDefaultProfile(config, MainOptionsPane.MAIN_POLICY_HASH, ""); //$NON-NLS-1$
    	savePreferenceOnDefaultProfile(config, MainOptionsPane.MAIN_POLICY_QUALIFIER, ""); //$NON-NLS-1$
    	savePreferenceOnDefaultProfile(config, ContextOptionsPane.KEY_SUBJECT, ""); //$NON-NLS-1$
    	savePreferenceOnDefaultProfile(config, ContextOptionsPane.KEY_PRODUCTION_PLACE, ""); //$NON-NLS-1$
    	savePreferenceOnDefaultProfile(config, ContextOptionsPane.KEY_CONTACT_INFO, ""); //$NON-NLS-1$
    	savePreferenceOnDefaultProfile(config, ContextOptionsPane.KEY_PADES_FORMAT, AOSignConstants.PADES_SUBFILTER_BASIC);
    	savePreferenceOnDefaultProfile(config, AccessibilityOptionsPane.MAIN_FONT_SIZE, "false"); //$NON-NLS-1$
    	savePreferenceOnDefaultProfile(config, AccessibilityOptionsPane.MAIN_FOCUS_VISIBLE, "false"); //$NON-NLS-1$
    	savePreferenceOnDefaultProfile(config, AccessibilityOptionsPane.MAIN_FONT_STYLE, "false"); //$NON-NLS-1$
    	savePreferenceOnDefaultProfile(config, AccessibilityOptionsPane.MAIN_CURSOR_SIZE, "false"); //$NON-NLS-1$
    	savePreferenceOnDefaultProfile(config, AccessibilityOptionsPane.MAIN_HIGHT_CONTRAST, "false"); //$NON-NLS-1$
    	savePreferenceOnDefaultProfile(config, AccessibilityOptionsPane.MAIN_WINDOWS_SIZE, "false"); //$NON-NLS-1$
    }

    /** Guardas las preferencias en el perfil actual.
     * @param config Configuraci&oacute;n que contiene el par valor-clave de configuraci&oacute;n.
     * @param key Clave de configuraci&oacute;n.
     * @param value Valor por defecto de la clave de configuraci&oacute;n. */
    public static void savePreferenceOnDefaultProfile(final Properties config, final String key, final String value) {
    	Main.getPreferences().put(KEY_PROFILES + KEY_DEFAULT_PROFILE_NAME + '.' + key, config.getProperty(key, value));

    }
}
