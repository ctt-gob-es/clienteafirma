/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma;

import java.net.URL;
import java.net.URLClassLoader;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Vector;
import java.util.logging.Logger;

/** Gestor de mensajes del BootLoader. */
public final class BootLoaderMessages {

    private static final String BUNDLE_NAME = "messages"; //$NON-NLS-1$

    private static ResourceBundle RESOURCE_BUNDLE;

    static {
        try {            
            ClassLoader classLoader = BootLoaderMessages.class.getClassLoader();
            if (classLoader instanceof URLClassLoader) {
                Vector<URL> urls = new Vector<URL>();
                for (URL url : ((URLClassLoader)classLoader).getURLs()) {
                    if (url.toString().endsWith(".jar")) { //$NON-NLS-1$
                        urls.add(url);
                    }
                    classLoader = new URLClassLoader(urls.toArray(new URL[0]));
                }
            }
            RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME, Locale.getDefault(), classLoader);
        }
        catch (final Exception e) {
            try {
                RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);
            }
            catch (final Exception e1) {
                Logger.getLogger("es.gob.afirma").severe("No ha podido cargarse el fichero de mensajes localizados: " + e1);  //$NON-NLS-1$//$NON-NLS-2$
            }
        }
    }

    private BootLoaderMessages() {
        // No permitimos la instanciacion
    }

    /** Obtiene un mensaje de usuario.
     * @param key Clave del mensaje a obtener
     * @return Mensaje obtenido */
    public static String getString(final String key) {
        try {
            return RESOURCE_BUNDLE.getString(key);
        }
        catch (final MissingResourceException e) {
            return '!' + key + '!';
        }
    }
}
