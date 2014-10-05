package es.gob.afirma.crypto.handwritten;

import java.util.ResourceBundle;

final class HandwrittenMessages {

	private static final String BUNDLE_NAME = "handwrittenmessages"; //$NON-NLS-1$

	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

	private HandwrittenMessages() {
		// No instanciable
	}

	static String getString(final String key) {
		try {
			return RESOURCE_BUNDLE.getString(key);
		}
		catch (final Exception e) {
			return '!' + key + '!';
		}
	}

    /** Obtiene un mensaje.
     * @param key Clave del mensaje.
     * @param params Part&iacute;culas de texto que se agregan al mensaje.
     * @return Mensaje correspondiente a la clave. */
    static String getString(final String key, final String... params) {

    	String text;
        try {
            text = RESOURCE_BUNDLE.getString(key);
        }
        catch (final Exception e) {
            return '!' + key + '!';
        }

        if (params != null) {
            for (int i = 0; i < params.length; i++) {
            	if (params[i] != null) {
            		text = text.replace("%" + i, params[i]); //$NON-NLS-1$
            	}
            }
        }

        return text;
    }
}
