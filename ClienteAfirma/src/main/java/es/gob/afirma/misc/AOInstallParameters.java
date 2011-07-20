/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.misc;

import java.io.File;
import java.security.AccessController;
import java.util.logging.Logger;

/** Constantes de utilidad en toda la aplicaci&oacute;n.
 * @version 0.4. */
public final class AOInstallParameters {

    /** Directorio de usuario. */
    public static final String USER_HOME;
    static {
        USER_HOME = AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {
                try {
                    return Platform.getUserHome() + File.separator;
                }
                catch (final Exception e) {
                    Logger.getLogger("es.gob.afirma").severe("No ha podido determinarse el directorio de usuario para la configuracion del cliente");
                    return "";
                }
            }
        });
    }

    // ************************************************************
    // ************* DIRECTORIOS DE INSTALACION *******************
    // ************************************************************

    /** Recupera el directorio del usuario activo (terminado en el car&acute;cter
     * separador).
     * @return Directorio del usuario activo del sistema. */
    public static final String getUserHome() {
        return USER_HOME;
    }
}
