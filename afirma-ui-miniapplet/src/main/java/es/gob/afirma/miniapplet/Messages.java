package es.gob.afirma.miniapplet;

import java.util.ResourceBundle;

public class Messages {
	private static final String BUNDLE_NAME = "miniappletmessages"; //$NON-NLS-1$

	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle
			.getBundle(BUNDLE_NAME);

	private Messages() {
        // No permitimos la instanciacion
	}

	/** Recupera el texto identificado con la clave proporcionada.
     * @param key
     *        Clave del texto.
     * @return Recuerso textual. */
    public static String getString(final String key) {
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
    public static String getString(final String key, final String text) {
        try {
            return RESOURCE_BUNDLE.getString(key).replace("%0", text); //$NON-NLS-1$
        }
        catch (final Exception e) {
            return '!' + key + '!';
        }
    }
}
