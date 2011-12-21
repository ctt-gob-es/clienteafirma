package es.gob.afirma.checker;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * Manejador para la recuperaci&opacute;n de mensajes del Applet de comprobaci&oacute;n.
 */
public class CheckerMessages {
	private static final String BUNDLE_NAME = "es.gob.afirma.checker.messages"; //$NON-NLS-1$

	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle
			.getBundle(BUNDLE_NAME);

	private CheckerMessages() {
		/* Ocultamos el constructor por defecto */
	}

	/**
	 * Recupera un mensaje externalizado.
	 * @param key Clave del mensaje que se desea recuperar.
	 * @return Mensaje recuperado.
	 */
	public static String getString(String key) {
		try {
			return RESOURCE_BUNDLE.getString(key);
		} catch (MissingResourceException e) {
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
    
    /** Recupera el texto identificado con la clave proporcionada y sustituye las
     * subcadenas de tipo "%i" por el texto en la posici&oacute;n 'i' del array
     * proporcionado. Introducir m&aacute;s de 10 cadenas distintas para sustituir
     * conllevar&aacute; errores en el resultado. 
     * @param key
     *        Clave del texto.
     * @param params
     *        Par&aacute;metros que se desean insertar.
     * @return Recuerso textual con las subcadenas sustituidas. */
    static String getString(final String key, final String[] params) {

        String text;
        try {
            text = RESOURCE_BUNDLE.getString(key);
        }
        catch (final Exception e) {
            return '!' + key + '!';
        }

        if (params != null) {
            for (int i = 0; i < params.length; i++) {
                text = text.replace("%" + i, params[i]); //$NON-NLS-1$
            }
        }

        return text;
    }
}
