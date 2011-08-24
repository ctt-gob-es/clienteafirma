package es.gob.afirma.keystores.mozilla;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

class FirefoxKeyStoreMessages {
    private static final String BUNDLE_NAME = "es.gob.afirma.keystores.mozilla.firefoxmessages"; //$NON-NLS-1$

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

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
