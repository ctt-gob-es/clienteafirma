package es.gob.afirma.standalone.ui.restoreconfig;

import java.util.ResourceBundle;

/** Manejador de las cadenas de texto externalizadas. */
final class Messages {
	private static final String BUNDLE_NAME = "es.gob.afirma.standalone.configurator.messages"; //$NON-NLS-1$

	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle
			.getBundle(BUNDLE_NAME);

	private Messages() {
		// No instanciable
	}

	/** Recupera la cadena de texto en el idioma configurado.
	 * @param key Clave de la cadena.
	 * @return Cadena de texto. */
	public static String getString(final String key) {
		try {
			return RESOURCE_BUNDLE.getString(key);
		}
		catch (final Exception e) {
			return '!' + key + '!';
		}
	}
}
