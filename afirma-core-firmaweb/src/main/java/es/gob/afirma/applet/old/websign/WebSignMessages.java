/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.applet.old.websign;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import es.gob.afirma.core.misc.AOUtil;

class WebSignMessages {
    private static final String BUNDLE_NAME = "websignmessages"; //$NON-NLS-1$

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME, Locale.getDefault(), AOUtil.getCleanClassLoader());

    private WebSignMessages() {
        // No permitimos la instanciacion
    }

    static String getString(String key) {
        try {
            return RESOURCE_BUNDLE.getString(key);
        }
        catch (MissingResourceException e) {
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
