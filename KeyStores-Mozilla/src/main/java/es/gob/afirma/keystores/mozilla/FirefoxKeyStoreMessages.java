package es.gob.afirma.keystores.mozilla;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Vector;
import java.util.logging.Logger;

class FirefoxKeyStoreMessages {
    
    private static final String BUNDLE_NAME = "firefoxmessages"; //$NON-NLS-1$
    private static ResourceBundle RESOURCE_BUNDLE;

    static {
        try {            
            ClassLoader classLoader = FirefoxKeyStoreMessages.class.getClassLoader();
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
    
    private FirefoxKeyStoreMessages() {
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
}
