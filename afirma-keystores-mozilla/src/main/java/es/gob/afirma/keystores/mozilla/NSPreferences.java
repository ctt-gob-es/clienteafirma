/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.mozilla;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.core.misc.Platform;

/** M&eacute;todos de utilidad para Mozilla Firefox y Nestcape.
 * Inspirada en la clase com.sun.deploy.net.proxy.NSPreferences de Sun
 * Microsystems. */
final class NSPreferences {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** N&uacute;mero m&iacute;nimo de ficheros que debe haber en un perfil de Mozilla para que
	 * sea v&aacute;lido. */
	private static final int MIN_FIREFOX_FILES_ON_PROFILE = 10;

    private NSPreferences() {
        // No permitimos la instanciacion
    }

    /** Devuelve el directorio del perfil activo de Firefox. Si no hubiese perfil
     * activo, devolver&iacute;a el directorio del perfil por defecto y si
     * tampoco lo hubiese, el del primer perfil encontrado. Si no hubiese
     * perfiles configurados, devolver&iacute;a {@code null}.
     * @param iniFile Fichero con la informaci&oacute;n de los perfiles de Firefox.
     * @return Directorio con la informaci&oacute;n del perfil.
     * @throws IOException Cuando ocurre un error abriendo o leyendo el fichero. */
    static String getFireFoxUserProfileDirectory(final File iniFile) throws IOException {

        if (iniFile == null) {
            throw new IllegalArgumentException(
        		"El fichero INI es nulo y no se podra determinar el directorio del usuario de Firefox" //$NON-NLS-1$
    		);
        }

        if (!iniFile.isFile()) {
            throw new IOException(
        		"No se ha encontrado el fichero con los perfiles de Firefox en: " + iniFile //$NON-NLS-1$
    		);
        }

        String currentProfilePath = null;

        // Leemos el fichero con la informacion de los perfiles y buscamos el
        // activo(el que esta bloqueado)
        final FirefoxProfile[] profiles = readProfiles(iniFile);
        for (final FirefoxProfile profile : profiles) {
        	if (isDummyProfile(profile)) {
	    		continue;
	    	}
            if (profile.isLocked()) {
                currentProfilePath = profile.getAbsolutePath();
                LOGGER.info("Se utilizara el perfil bloqueado para Mozilla: '" + profile.getAbsolutePath().replace(Platform.getUserHome(), "USERHOME") + "'"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                break;
            }
        }

        // Si no hay ninguno actualmente activo, tomamos el por defecto
        if (currentProfilePath == null) {
            for (final FirefoxProfile profile : profiles) {
            	if (isDummyProfile(profile)) {
		    		continue;
    		    }
                if (profile.isDefault()) {
                    currentProfilePath = profile.getAbsolutePath();
                    LOGGER.info("Se utilizara el perfil por defecto para Mozilla"); //$NON-NLS-1$
                    break;
                }
            }
        }

        // Si no hay ninguno actualmente activo y el perfil por defecto esta bloqueado, se elige el que haya sufrido una ultima modificacion mas reciente
        // Esto se debe a problemas con los perfiles en versiones de Mozilla Firefox a partir de la 69
        if (currentProfilePath == null) {
        	long lastModified = 0;
		    for (final FirefoxProfile profile : profiles) {
		    	if (isDummyProfile(profile)) {
		    		continue;
		    	}
		    	if (new File(profile.getAbsolutePath()).lastModified() > lastModified) {
		    		lastModified = new File(profile.getAbsolutePath()).lastModified();
		    		currentProfilePath = profile.getAbsolutePath();
		    		LOGGER.info(
		    			"Ultima modificacion del perfil '" + profile.getAbsolutePath().replace(Platform.getUserHome(), "USERHOME") + "': " //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	    					+ new File(profile.getAbsolutePath()).lastModified()
		    		);
		    	}
		    }
        }

        // Si no hay ninguno por defecto, se toma el primero
        if (currentProfilePath == null && profiles.length > 0) {
            currentProfilePath = profiles[0].getAbsolutePath();
            LOGGER.info("Se utilizara el primer perfil encontrado"); //$NON-NLS-1$
        }

        return currentProfilePath;
    }

    private static boolean isDummyProfile(final FirefoxProfile profile) {
    	// Si el perfil tiene menos de 10 ficheros o no se puede leer ninguno de los ficheros .db del perfil, damos el perfil por invalido
    	if (new File(profile.getAbsolutePath()).list().length < MIN_FIREFOX_FILES_ON_PROFILE ||
    		!new File(profile.getAbsolutePath(), "key4.db").canRead() && !new File(profile.getAbsolutePath(), "key3.db").canRead()) { //$NON-NLS-1$ //$NON-NLS-2$
    		LOGGER.info(
    			"Se descarta el perfil '" + profile.getAbsolutePath().replace(Platform.getUserHome(), "USERHOME") + "' con " + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					new File(profile.getAbsolutePath()).list().length + " archivos por no pertenecer al usuario activo o ser invalido" //$NON-NLS-1$
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
    private static FirefoxProfile[] readProfiles(final File iniFile) throws IOException {

        final String nameAtr = "name="; //$NON-NLS-1$
        final String isRelativeAtr = "isrelative="; //$NON-NLS-1$
        final String pathProfilesAtr = "path="; //$NON-NLS-1$
        final String isDefaultAtr = "default="; //$NON-NLS-1$

        String line = null;
        final List<FirefoxProfile> profiles = new ArrayList<>();

        try (
	        final BufferedReader in = new BoundedBufferedReader(
	    		new FileReader(iniFile),
				1024, // Maximo 1024 lineas
				4096 // Maximo 4KB por linea
			);
		) {

	        while ((line = in.readLine()) != null) {

	            // Buscamos un nuevo bloque de perfil
	            if (!line.trim().toLowerCase().startsWith("[profile")) { //$NON-NLS-1$
	                continue;
	            }

	            final FirefoxProfile profile = new FirefoxProfile();
	            while ((line = in.readLine()) != null && line.trim().length() > 0 && !line.trim().toLowerCase().startsWith("[profile")) { //$NON-NLS-1$
	                if (line.trim().toLowerCase().startsWith(nameAtr)) {
	                    profile.setName(line.trim().substring(nameAtr.length()));
	                }
	                else if (line.trim().toLowerCase().startsWith(isRelativeAtr)) {
	                    profile.setRelative(
                    		line.trim().substring(isRelativeAtr.length()).equals("1") //$NON-NLS-1$
	                    );
	                }
	                else if (line.trim().toLowerCase().startsWith(pathProfilesAtr)) {
	                    profile.setPath(
                    		line.trim().substring(pathProfilesAtr.length())
	                    );
	                }
	                else if (line.trim().toLowerCase().startsWith(isDefaultAtr)) {
	                    profile.setDefault(
                    		line.trim().substring(isDefaultAtr.length()).equals("1") //$NON-NLS-1$
	                    );
	                }
	                else {
	                    break;
	                }
	            }

	            // Debemos encontrar al menos el nombre y la ruta del perfil
	            if (profile.getName() != null || profile.getPath() != null) {
	                profile.setAbsolutePath(profile.isRelative() ?
	            		new File(iniFile.getParent(), profile.getPath()).toString() :
	            			profile.getPath());

	                profiles.add(profile);
	            }

	            profile.setLocked(
            		new File(profile.getAbsolutePath(), "lock").exists() || // En UNIX //$NON-NLS-1$
            			Files.isSymbolicLink(new File(profile.getAbsolutePath(), "lock").toPath()) || // En UNIX y Firefox 69 o superiores //$NON-NLS-1$
            				new File(profile.getAbsolutePath(), "parent.lock").exists() // En Windows //$NON-NLS-1$
        		);

	        }
        }
        return profiles.toArray(new FirefoxProfile[profiles.size()]);
    }

    /** Almacena la configuraci&oacute;n para la identificaci&oacute;n de un
     * perfil de Mozilla Firefox. */
    static final class FirefoxProfile {

        private String name = null;

        String getName() {
            return this.name;
        }

        void setName(final String n) {
            this.name = n;
        }

        private boolean relative = true;

        boolean isRelative() {
            return this.relative;
        }

        void setRelative(final boolean r) {
            this.relative = r;
        }

        private String path = null;

        String getPath() {
            return this.path;
        }

        void setPath(final String p) {
            this.path = p;
        }

        private String absolutePath = null;

        String getAbsolutePath() {
            return this.absolutePath;
        }

        void setAbsolutePath(final String ap) {
            this.absolutePath = ap;
        }

        private boolean def = false;

        boolean isDefault() {
            return this.def;
        }

        void setDefault(final boolean d) {
            this.def = d;
        }

        private boolean locked = false;

        boolean isLocked() {
        	return this.locked;
        }

        void setLocked(final boolean lock) {
        	this.locked = lock;
        }

        @Override
		public String toString() {
        	return "Perfil de Firefox" + //$NON-NLS-1$
    			(this.locked ? " bloqueado" : " no bloqueado") + //$NON-NLS-1$ //$NON-NLS-2$
    			(this.def ? " y por defecto " : "") + //$NON-NLS-1$ //$NON-NLS-2$
    			(this.absolutePath != null ? " situado en: " + this.absolutePath : ""); //$NON-NLS-1$ //$NON-NLS-2$
        }
    }
}
