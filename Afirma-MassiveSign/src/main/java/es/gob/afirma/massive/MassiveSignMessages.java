package es.gob.afirma.massive;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

class MassiveSignMessages {
    private static final String BUNDLE_NAME = "massivesignmessages"; //$NON-NLS-1$

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

    private MassiveSignMessages() {
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
