/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.standalone;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/** Gesti&oacute;n de mensajes del aplicativo. */
public class Messages {

    private static final String BUNDLE_NAME = "es.gob.afirma.standalone.messages"; //$NON-NLS-1$

    private static ResourceBundle bundle = ResourceBundle.getBundle(BUNDLE_NAME, Locale.getDefault());

    private Messages() { /* No permitimos la instanciacion */ }

    /** Obtiene un mensaje.
     * @param key Clave del mensaje
     * @return Mensaje correspondiente a la clave */
    public static String getString(final String key) {
        try {
            return bundle.getString(key);
        }
        catch (final MissingResourceException e) {
            return '!' + key + '!';
        }
    }

    /** Cambia la localizaci&oacute;n a la establecida por defecto. */
    static void changeLocale() {
        bundle = ResourceBundle.getBundle(BUNDLE_NAME, Locale.getDefault());
    }
}
