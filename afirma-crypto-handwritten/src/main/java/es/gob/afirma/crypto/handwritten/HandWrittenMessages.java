package es.gob.afirma.crypto.handwritten;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

class HandWrittenMessages {
	private static final String BUNDLE_NAME = "es.gob.afirma.crypto.handwritten.handwrittenmessages"; //$NON-NLS-1$

	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle
			.getBundle(BUNDLE_NAME);

	private HandWrittenMessages() {
		// No instanciable
	}

	static String getString(final String key) {
		try {
			return RESOURCE_BUNDLE.getString(key);
		} catch (final MissingResourceException e) {
			return '!' + key + '!';
		}
	}
}
