package es.gob.afirma.standalone.updater;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

final class UpdaterMessages {

	private static final String BUNDLE_NAME = "properties/updatermessages"; //$NON-NLS-1$

	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

	private UpdaterMessages() {
		// No permitimos la instanciacion
	}

	static String getString(final String key) {
		try {
			return RESOURCE_BUNDLE.getString(key);
		}
		catch (final MissingResourceException e) {
			return '!' + key + '!';
		}
	}
}
