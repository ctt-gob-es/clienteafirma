package es.gob.afirma.plugin.certvalidation;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * Clase para la carga de mensajes.
 */
public class Messages {
	private static final String BUNDLE_NAME = "es.gob.afirma.plugin.certvalidation.messages"; //$NON-NLS-1$

	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

	private Messages() {
	}

	/**
	 * Obtiene un mensaje del fichero de recursos.
	 * @param key Clave del mensaje a recuperar.
	 * @return Mensaje legible.
	 */
	public static String getString(String key) {
		try {
			return RESOURCE_BUNDLE.getString(key);
		} catch (final MissingResourceException e) {
			return '!' + key + '!';
		}
	}
}
