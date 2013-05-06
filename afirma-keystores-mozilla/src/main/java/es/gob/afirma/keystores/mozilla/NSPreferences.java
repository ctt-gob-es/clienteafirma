/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.keystores.mozilla;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/** M&eacute;todos de utilidad para Mozilla Firefox y Nestcape.
 * Inspirada en la clase com.sun.deploy.net.proxy.NSPreferences de Sun
 * Microsystems. */
final class NSPreferences {

    private NSPreferences() {
        // No permitimos la instanciacion
    }

    /** Devuelve el directorio del perfil activo de Firefox. Si no hubiese perfil
     * activo, devolver&iacute;a el directorio del perfil por defecto y si
     * tampoco lo hubiese el del primer perfil encontrado. Si no hubiese
     * perfiles configurados, devolver&iacute;a {@code null}.
     * @param iniFile
     *        Fichero con la informaci&oacute;n de los perfiles de Firefox.
     * @return Directorio con la informaci&oacute;n del perfil.
     * @throws IOException
     *         Cuando ocurre un error abriendo o leyendo el fichero. */
    static String getFireFoxUserProfileDirectory(final File iniFile) throws IOException {

        if (iniFile == null) {
            throw new IllegalArgumentException("El fichero INI es nulo y no se podra determinar el directorio del usuario de firefox"); //$NON-NLS-1$
        }

        if (!iniFile.exists() || !iniFile.isFile()) {
            throw new IOException("No se ha encontrado el fichero con los perfiles de firefox"); //$NON-NLS-1$
        }

        String currentProfilePath = null;

        // Leemos el fichero con la informacion de los perfiles y buscamos el
        // activo(el que esta bloqueado)
        final FirefoxProfile[] profiles = readProfiles(iniFile);
        for (final FirefoxProfile profile : profiles) {
            if (isProfileLocked(profile)) {
                currentProfilePath = profile.getAbsolutePath();
                break;
            }
        }

        // Si no hay ninguno actualmente activo, tomamos el por defecto
        if (currentProfilePath == null) {
            for (final FirefoxProfile profile : profiles) {
                if (profile.isDefault()) {
                    currentProfilePath = profile.getAbsolutePath();
                    break;
                }
            }
        }

        // Si no hay ninguno por defecto, se toma el primero
        if (profiles.length > 0) {
            currentProfilePath = profiles[0].getAbsolutePath();
        }

        return currentProfilePath;
    }

    /** Parsea la informacion de los perfiles declarada en el fichero
     * "profiles.ini". Para identificar correctamente los perfiles es necesario
     * que haya al menos una l&iacute;nea de separaci&oacute;n entre los bloques
     * de informaci&oacute;n de cada perfil.
     * @param iniFile
     *        Fichero con lainformaci&oacute;n de los perfiles.
     * @return Listado de perfiles completos encontrados.
     * @throws IOException
     *         Cuando se produce un error durante la lectura de la
     *         configuraci&oacute;n. */
    private static FirefoxProfile[] readProfiles(final File iniFile) throws IOException {

        final String nameAtr = "name="; //$NON-NLS-1$
        final String isRelativeAtr = "isrelative="; //$NON-NLS-1$
        final String pathProfilesAtr = "path="; //$NON-NLS-1$
        final String isDefaultAtr = "default="; //$NON-NLS-1$

        String line = null;
        final List<FirefoxProfile> profiles = new ArrayList<FirefoxProfile>();
        final BufferedReader in = new BufferedReader(new FileReader(iniFile));

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
                profile.setAbsolutePath(profile.isRelative() ? new File(iniFile.getParent(), profile.getPath()).toString() : profile.getPath());

                profiles.add(profile);
            }
        }
        in.close();

        return profiles.toArray(new FirefoxProfile[profiles.size()]);
    }

    /** Comprueba que un perfil de Firefox est&eacute; bloqueado. Un perfil esta
     * bloqueado cuando en su directorio se encuentra el fichero "parent.lock".
     * @param profile
     *        Informaci&oacute;n del perfil de Firefox.
     * @return Devuelve {@code true} si el perfil esta bloqueado, {@code false} en caso contrario. */
    private static boolean isProfileLocked(final FirefoxProfile profile) {
        return new File(profile.getAbsolutePath(), "parent.lock").exists() || // En Windows //$NON-NLS-1$
               new File(profile.getAbsolutePath(), "lock").exists(); // En UNIX //$NON-NLS-1$
    }

    /** Clase que almacena la configuraci&oacute;n para la identificacion de un
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
    }
}
