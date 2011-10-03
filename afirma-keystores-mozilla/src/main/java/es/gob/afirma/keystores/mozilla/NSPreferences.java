/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.keystores.mozilla;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Vector;

/** M&eacute;todos de utilidad para Mozilla Firefox y Nestcape.
 * Inspirada en la clase com.sun.deploy.net.proxy.NSPreferences de Sun
 * Microsystems. */
final class NSPreferences {

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
                currentProfilePath = profile.absolutePath;
                break;
            }
        }

        // Si no hay ninguno actualmente activo, tomamos el por defecto
        if (currentProfilePath == null) {
            for (final FirefoxProfile profile : profiles) {
                if (profile.isDefault) {
                    currentProfilePath = profile.absolutePath;
                    break;
                }
            }
        }

        // Si no hay ninguno por defecto, se toma el primero
        if (profiles.length > 0) {
            currentProfilePath = profiles[0].absolutePath;
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

        final String NAME_ATR = "name="; //$NON-NLS-1$
        final String IS_RELATIVE_ATR = "isrelative="; //$NON-NLS-1$
        final String PATH_PROFILES_ATR = "path="; //$NON-NLS-1$
        final String IS_DEFAULT_ATR = "default="; //$NON-NLS-1$

        String line = null;
        final Vector<FirefoxProfile> profiles = new Vector<FirefoxProfile>();
        final BufferedReader in = new BufferedReader(new FileReader(iniFile));
        try {
            while ((line = in.readLine()) != null) {

                // Buscamos un nuevo bloque de perfil
                if (!line.trim().toLowerCase().startsWith("[profile")) { //$NON-NLS-1$
                    continue;
                }

                final FirefoxProfile profile = new FirefoxProfile();
                while ((line = in.readLine()) != null && line.trim().length() > 0 && !line.trim().toLowerCase().startsWith("[profile")) { //$NON-NLS-1$
                    if (line.trim().toLowerCase().startsWith(NAME_ATR)) {
                        profile.name = line.trim().substring(NAME_ATR.length());
                    }
                    else if (line.trim().toLowerCase().startsWith(IS_RELATIVE_ATR)) {
                        profile.isRelative =
                                line.trim().substring(IS_RELATIVE_ATR.length()).equals("1"); //$NON-NLS-1$
                    }
                    else if (line.trim().toLowerCase().startsWith(PATH_PROFILES_ATR)) {
                        profile.path =
                                line.trim().substring(PATH_PROFILES_ATR.length());
                    }
                    else if (line.trim().toLowerCase().startsWith(IS_DEFAULT_ATR)) {
                        profile.isDefault =
                                line.trim().substring(IS_DEFAULT_ATR.length()).equals("1"); //$NON-NLS-1$
                    }
                    else {
                        break;
                    }
                }

                // Debemos encontrar al menos el nombre y la ruta del perfil
                if (profile.name != null || profile.path != null) {
                    profile.absolutePath = profile.isRelative ? new File(iniFile.getParent(), profile.path).toString() : profile.path;

                    profiles.add(profile);
                }
            }
        }
        catch (final Exception e) {
            throw new IOException("Error al leer la configuracion de los perfiles de Firefox: " + e); //$NON-NLS-1$
        }
        finally {
            try {
                in.close();
            }
            catch (final Exception e) {
             // Ignoramos los errores en el cierre
            }
        }

        return profiles.toArray(new FirefoxProfile[profiles.size()]);
    }

    /** Comprueba que un perfil de Firefox est&eacute; bloqueado. Un perfil esta
     * bloqueado cuando en su directorio se encuentra el fichero "parent.lock".
     * @param profile
     *        Informaci&oacute;n del perfil de Firefox.
     * @return Devuelve {@code true} si el perfil esta bloqueado, {@code false} en caso contrario. */
    private static boolean isProfileLocked(final FirefoxProfile profile) {
        return new File(profile.absolutePath, "parent.lock").exists() || // En //$NON-NLS-1$
                                                                         // Windows
               new File(profile.absolutePath, "lock").exists(); // En UNIX //$NON-NLS-1$
    }

    /** Clase que almacena la configuraci&oacute;n para la identificacion de un
     * perfil de Mozilla Firefox. */
    static final class FirefoxProfile {
        String name = null;
        boolean isRelative = true;
        String path = null;
        String absolutePath = null;
        boolean isDefault = false;
    }
}
