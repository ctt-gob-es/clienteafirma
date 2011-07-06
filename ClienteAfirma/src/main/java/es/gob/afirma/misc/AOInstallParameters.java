/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
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

    /** Preguntar al usuario sobre la acci&oacute;n a realizar. */
    public final static int ACTION_ASK = 1;

    /** Respetar instalaciones antiguas del cliente. */
    public final static int ACTION_RESPECT = 2;

    /** Eliminar instalaciones antiguas del cliente. */
    public final static int ACTION_DELETE = 3;

    /** Acci&oacute;n a realizar con respecto a las versiones antiguas
     * encontradas del cliente. */
    public int oldVersionsAction = ACTION_ASK;

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
