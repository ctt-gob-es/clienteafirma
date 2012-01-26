package es.gob.afirma.ui.utils;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/** Clase para acceder a los mensajes del properties sin lanzar excepciones de error */
public final class Messages {

    private static ResourceBundle bundle = ResourceBundle.getBundle("resources/properties/Idioma", Locale.getDefault()); //$NON-NLS-1$

    public static void changeLocale() {
        bundle = ResourceBundle.getBundle("properties/Idioma", Locale.getDefault()); //$NON-NLS-1$
    }

    public static String getString(final String codeString) {
        try {
            return bundle.getString(codeString);
        }
        catch (final MissingResourceException e) {
            return "##ERROR## Cadena no disponible."; //$NON-NLS-1$
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
            return bundle.getString(key).replace("%0", text); //$NON-NLS-1$
        }
        catch (final Exception e) {
            return '!' + key + '!';
        }
    }
}
