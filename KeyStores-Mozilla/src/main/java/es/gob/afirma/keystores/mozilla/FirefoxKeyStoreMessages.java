package es.gob.afirma.keystores.mozilla;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

class FirefoxKeyStoreMessages {
    private static final String BUNDLE_NAME = "firefoxmessages"; //$NON-NLS-1$

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

    static {
        ClassLoader classLoader = FirefoxKeyStoreMessages.class.getClassLoader();
        System.out.println("Tipo de classloader para los mensajes de almacenes Firefox: " + classLoader.getClass().toString());
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
