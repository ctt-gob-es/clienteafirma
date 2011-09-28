/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.keystores.common;

import java.util.Locale;
import java.util.ResourceBundle;

import es.gob.afirma.core.misc.AOUtil;

/** Clase para la obtencion de los recursos textuales del UI. */
final class KeyStoreMessages {

    private static final String BUNDLE_NAME = "keystoremessages"; //$NON-NLS-1$
    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME, Locale.getDefault(), AOUtil.getCleanClassLoader());

    private KeyStoreMessages() {
        // No permitimos la instanciacion
    }

    /** Recupera el texto identificado con la clave proporcionada.
     * @param key
     *        Clave del texto.
     * @return Recuerso textual. */
    static String getString(final String key) {
        try {
            return RESOURCE_BUNDLE.getString(key);
        }
        catch (final Exception e) {
            return '!' + key + '!';
        }
    }

    /** Recupera el texto identificado con la clave proporcionada y sustituye la
     * subcadenas "%0" por el texto proporcionado.
     * @param key
     *        Clave del texto.
     * @param text
     *        Texto que se desea insertar.
     * @return Recuerso textual con la subcadena sustituida. */
    static String getString(final String key, final String text) {
        try {
            return RESOURCE_BUNDLE.getString(key).replace("%0", text); //$NON-NLS-1$
        }
        catch (final Exception e) {
            return '!' + key + '!';
        }
    }
    
}
