package es.gob.afirma.install;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

/** Gestor de mensajes del BootLoader. */
public final class Messages {
	
	private static final String BUNDLE_NAME = "messages"; //$NON-NLS-1$

	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

	private Messages() {}

	/**
	 * Obtiene un mensaje de usuario.
	 * @param key Clave del mensaje a obtener
	 * @return Mensaje obtenido
	 */
	public static String getString(final String key) {
		try {
			return RESOURCE_BUNDLE.getString(key);
		} catch (final MissingResourceException e) {
			return '!' + key + '!';
		}
	}
}
