package es.gob.afirma.android;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

/** Gestor de mensajes hacia el usuario. */
public final class Messages {
	private static final String BUNDLE_NAME = "es.gob.afirma.android.messages"; //$NON-NLS-1$

	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

	private Messages() {}

	/** Obtiene un mensaje.
	 * @param key C&oacute;digo del mensaje
	 * @return Mensaje */
	public static String getString(final String key) {
		try {
			return RESOURCE_BUNDLE.getString(key);
		}
		catch (final MissingResourceException e) {
			return '!' + key + '!';
		}
	}
}
