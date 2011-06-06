package es.gob.afirma.standalone;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/** Gesti&oacute;n de mensajes del aplicativo. */
public class Messages {
	
	private static final String BUNDLE_NAME = "es.gob.afirma.standalone.messages"; //$NON-NLS-1$

	private static ResourceBundle bundle = ResourceBundle
			.getBundle(BUNDLE_NAME, Locale.getDefault());

	private Messages() {
	}

	/**
	 * Obtiene un mensaje.
	 * @param key Clave del mensaje
	 * @return Mensaje correspondiente a la clave
	 */
	public static String getString(final String key) {
		try {
			return bundle.getString(key);
		} catch (MissingResourceException e) {
			return '!' + key + '!';
		}
	}
	
	/**
	 * Cambia la localizaci&oacute;n a la establecida por defecto.
	 */
	public static void changeLocale () {
	    bundle = ResourceBundle.getBundle(BUNDLE_NAME, Locale.getDefault());
	}
}
