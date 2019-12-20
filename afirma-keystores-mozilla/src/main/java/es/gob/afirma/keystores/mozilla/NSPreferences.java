/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.mozilla;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.mozilla.ProfilesIni.FirefoxProfile;
import es.gob.afirma.keystores.mozilla.ProfilesIni.StateInfo;

/** M&eacute;todos de utilidad para Mozilla Firefox y Nestcape.
 * Inspirada en la clase com.sun.deploy.net.proxy.NSPreferences de Sun
 * Microsystems. */
final class NSPreferences {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** N&uacute;mero m&iacute;nimo de ficheros que debe haber en un perfil de Mozilla para que
	 * sea v&aacute;lido. */
	private static final int MIN_FIREFOX_FILES_ON_PROFILE = 10;


    /** Cachea los directorios de perfil activos. Normalmente, s&oacute;lo habr&aacute;
     * uno. */
    private static Map<String, String> activeProfilesDir = null;


    private NSPreferences() {
        // No permitimos la instanciacion
    }

    /** Devuelve el directorio del perfil activo de Firefox. Si no hubiese perfil
     * activo, devolver&iacute;a el directorio del perfil por defecto y si
     * tampoco lo hubiese, el del primer perfil encontrado. Si no hubiese
     * perfiles configurados, devolver&iacute;a {@code null}. Este metodo cachea
     * la ruta del directorio de perfil segun el fichero "profiles.ini" indicado
     * para evitar que deba cargarse y analizarse m&uacute;ltiples veces.
     * @param iniFile Fichero con la informaci&oacute;n de los perfiles de Firefox.
     * @return Directorio con la informaci&oacute;n del perfil.
     * @throws IOException Cuando ocurre un error abriendo o leyendo el fichero. */
    static String getFireFoxUserProfileDirectory(final File iniFile) throws IOException {

        if (iniFile == null) {
            throw new IllegalArgumentException(
        		"El fichero de perfiles es nulo y no se podra determinar el directorio del usuario de Firefox" //$NON-NLS-1$
    		);
        }

        if (activeProfilesDir == null) {
        	activeProfilesDir = new HashMap<>();
        }

        // Comprobamos si ya lo hemos mapeado previamente
        final String iniFilePath = iniFile.getCanonicalPath();
        if (activeProfilesDir.containsKey(iniFilePath)) {
        	return activeProfilesDir.get(iniFilePath);
        }

        if (!iniFile.isFile()) {
            throw new IOException(
        		"No se ha encontrado el fichero con los perfiles de Firefox en: " + iniFile //$NON-NLS-1$
    		);
        }

        String currentProfilePath = null;

        // Leemos el fichero con la informacion de los perfiles y buscamos el
        // activo(el que esta bloqueado)
        final ProfilesIni profilesIni = loadProfilesIni(iniFile);

        final FirefoxProfile activeProfile = getActiveProfile(profilesIni);

        // Comprobamos si se ha encontrado un perfil activo y si este es valido, en cuyo caso, lo devolvemos
        if (activeProfile != null && !isDummyProfile(activeProfile)) {
        	currentProfilePath = activeProfile.getAbsolutePath();
        	LOGGER.info("Se utilizara el perfil activo de Mozilla: '" + //$NON-NLS-1$
        			getCleanPath(currentProfilePath));
        }

        // Si no hay perfil activo o no es valido, tomamos el por defecto
        if (currentProfilePath == null) {
        	final FirefoxProfile[] profiles = profilesIni.getProfilesList().toArray(new FirefoxProfile[0]);
        	for (final FirefoxProfile profile : profiles) {
        		if (isDummyProfile(profile)) {
        			continue;
        		}
        		if (profile.isDefault()) {
        			currentProfilePath = profile.getAbsolutePath();
        			LOGGER.info("Se utilizara el perfil por defecto para Mozilla : " + getCleanPath(currentProfilePath)); //$NON-NLS-1$
        			break;
        		}
        	}
        }

        // Si no hay ninguno actualmente activo y el perfil por defecto esta bloqueado, se elige el que haya sufrido una ultima modificacion mas reciente
        // Esto se debe a problemas con los perfiles en versiones de Mozilla Firefox a partir de la 69
        if (currentProfilePath == null) {
        	long lastModified = 0;
        	final FirefoxProfile[] profiles = profilesIni.getProfilesList().toArray(new FirefoxProfile[0]);
		    for (final FirefoxProfile profile : profiles) {
		    	if (isDummyProfile(profile)) {
		    		continue;
		    	}
		    	if (new File(profile.getAbsolutePath()).lastModified() > lastModified) {
		    		lastModified = new File(profile.getAbsolutePath()).lastModified();
		    		currentProfilePath = profile.getAbsolutePath();
		    		LOGGER.info("Se usara el ultimo perfil modificado de Mozilla: " +  getCleanPath(currentProfilePath)); //$NON-NLS-1$
		    	}
		    }
        }

        // Si no hay ninguno por defecto, se toma el primero
        if (currentProfilePath == null && !profilesIni.getProfilesList().isEmpty()) {
            currentProfilePath = profilesIni.getProfilesList().get(0).getAbsolutePath();
            LOGGER.info("Se utilizara el primer perfil encontrado de Mozilla: " +  getCleanPath(currentProfilePath)); //$NON-NLS-1$
        }

        activeProfilesDir.put(iniFilePath, currentProfilePath);

        return currentProfilePath;
    }

    /**
     * Omite el directorio del usuario del path del perfil de FireFox.
     * @param profilePath Ruta del directorio de perfil.
     * @return Ruta ofuscada.
     */
    private static String getCleanPath(final String profilePath) {
    	return profilePath.replace(Platform.getUserHome(), "USERHOME"); //$NON-NLS-1$
    }

    /**
     * Obtiene cual deberia ser el perfil de Mozilla activo seg&uacute;n se declara en el fichero
     * de perfiles.
     * @param profilesIni Informaci&oacute;n cargada del fichero de perfiles.
     * @return Perfil activo o {@code null} si no se identific&oacute;.
     */
    private static FirefoxProfile getActiveProfile(final ProfilesIni profilesIni) {

    	FirefoxProfile activeProfile = null;

    	// Version 1 del fichero "profiles.ini"
        if (profilesIni.getGeneralInfo().getVersion() == 1) {
            for (final FirefoxProfile profile : profilesIni.getProfilesList()) {
            	if (isDummyProfile(profile)) {
            		continue;
            	}
            	if (profile.isLocked()) {
            		activeProfile = profile;
            		LOGGER.info("Se toma como perfil activo de Mozilla el primer perfil valido bloqueado"); //$NON-NLS-1$
            		break;
            	}
            }
        }
        // Version 2 y cualquiera que se defina en el futuro del fichero "profiles.ini"
        else {
        	final StateInfo stateInfo = profilesIni.getStateInfo();
        	if (stateInfo != null) {
        		final String profilePath = stateInfo.getDefaultProfilePath();
        		if (profilePath != null) {
        			for (final FirefoxProfile profile : profilesIni.getProfilesList()) {
        				if (profilePath.equals(profile.getPath())) {
        					activeProfile = profile;
        					LOGGER.info("Se toma como perfil activo de Mozilla el indicado en el 'profiles.ini' v2 o sup."); //$NON-NLS-1$
        					break;
        				}
        			}
        		}
        	}
        }

        if (activeProfile == null) {
        	LOGGER.info("No se encontro el perfil activo de Mozilla"); //$NON-NLS-1$
        }

		return activeProfile;
	}

	private static boolean isDummyProfile(final FirefoxProfile profile) {
    	// Si el perfil tiene menos de 10 ficheros o no se puede leer ninguno de los ficheros .db
		// del perfil, damos el perfil por invalido

    	if (new File(profile.getAbsolutePath()).list().length < MIN_FIREFOX_FILES_ON_PROFILE) {
    		LOGGER.fine(
    			"Se descarta el perfil '" + getCleanPath(profile.getAbsolutePath()) + //$NON-NLS-1$
    			"' por no alcanzar el numero de archivos de un perfil valido" //$NON-NLS-1$
    		);
    		return true;
    	}
    	if (!new File(profile.getAbsolutePath(), "key4.db").canRead() && //$NON-NLS-1$
    			!new File(profile.getAbsolutePath(), "key3.db").canRead()) { //$NON-NLS-1$
    		LOGGER.fine(
    			"Se descarta el perfil '" + getCleanPath(profile.getAbsolutePath()) + //$NON-NLS-1$
    			"' por no tener un almacen de claves legible" //$NON-NLS-1$
    		);
        	return true;
    	}
    	return false;
    }

    /** Analiza la informaci&oacute;n de los perfiles declarada en el fichero
     * "profiles.ini". Para identificar correctamente los perfiles es necesario
     * que haya al menos una l&iacute;nea de separaci&oacute;n entre los bloques
     * de informaci&oacute;n de cada perfil.
     * @param iniFile Fichero con la informaci&oacute;n de los perfiles.
     * @return Listado de perfiles completos encontrados.
     * @throws IOException Cuando se produce un error durante la lectura de la
     *                     configuraci&oacute;n. */
    private static ProfilesIni loadProfilesIni(final File iniFile) throws IOException {

    	ProfilesIni profilesIni;
    	try {
    		profilesIni = new ProfilesIni(iniFile);
    	}
    	catch (final Exception e) {
    		profilesIni = null;
    		throw new IOException("No se pudo cargar el fichero de perfiles de Firefox", e); //$NON-NLS-1$
    	}

    	return profilesIni;
    }
}
