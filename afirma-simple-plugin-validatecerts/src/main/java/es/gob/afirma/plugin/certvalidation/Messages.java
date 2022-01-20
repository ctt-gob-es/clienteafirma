package es.gob.afirma.plugin.certvalidation;

import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.logging.Logger;

/**
 * Clase para la carga de mensajes.
 */
final class Messages {
	private static final String BUNDLE_NAME = "es.gob.afirma.plugin.certvalidation.messages"; //$NON-NLS-1$

	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

	private Messages() {
		// No instanciable
	}

	/**
	 * Obtiene un mensaje del fichero de recursos.
	 * @param key Clave del mensaje a recuperar.
	 * @return Mensaje legible.
	 */
	public static String getString(final String key) {
		try {
			return RESOURCE_BUNDLE.getString(key);
		}
		catch (final MissingResourceException e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"No se ha encontrado el texto para la clave '" + key + "': " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
			return '!' + key + '!';
		}
	}
}
