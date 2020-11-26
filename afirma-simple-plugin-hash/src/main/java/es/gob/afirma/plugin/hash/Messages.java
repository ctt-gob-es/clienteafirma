package es.gob.afirma.plugin.hash;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * Clase para la carga de mensajes.
 */
public class Messages {
	private static final String BUNDLE_NAME = "es.gob.afirma.plugin.hash.messages"; //$NON-NLS-1$

	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

	private Messages() {
	}

	/**
	 * Obtiene un mensaje del fichero de recursos.
	 * @param key Clave del mensaje a recuperar.
	 * @return Mensaje legible.
	 */
	public static String getString(final String key) {
		try {
			return RESOURCE_BUNDLE.getString(key);
		} catch (final MissingResourceException e) {
			return '!' + key + '!';
		}
	}

    /** Obtiene un mensaje.
     * @param key Clave del mensaje
     * @param params Particulas de texto que se agregan al mensaje.
     * @return Mensaje correspondiente a la clave */
    public static String getString(final String key, final String... params) {
    	String text = getString(key);
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
